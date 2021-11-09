-module(ex_banking_client_worker).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_info/2]).

-export([create_user/2,get_balance/2,deposit/2,withdraw/2,send/2,handle_bank_request/2]).

-record(user,{
    ref,
    pid
}).
-record(state,{
}).
-define(NAME,?MODULE).
-define(TIMEOUT,4000).
-define(CALL_TIMEOUT,10000).
%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [], []).


init(Args)->
    {ok,#state{}}.
%%%% Handlers
handle_bank_request(To,Message)->
    {_,InnerMessage}=Message,
    case InnerMessage of 
        {create_user,_}->ex_banking_client_worker:create_user(To,Message);
        {get_balance,_}->ex_banking_client_worker:get_balance(To,Message);
        {deposit,_,_}->ex_banking_client_worker:deposit(To,Message);
        {withdraw,_,_}->ex_banking_client_worker:withdraw(To,Message);
        {send,_,_,_}->ex_banking_client_worker:send(To,Message)
    end.

create_user({Pid,ReplyTo},User)->
    gen_server:cast(Pid,{ReplyTo,{create_user,User}},?CALL_TIMEOUT).

deposit({Pid,ReplyTo},{User,Amount})->
    gen_server:cast(Pid, {ReplyTo,{deposit,{User,Amount}}},?CALL_TIMEOUT).

withdraw({Pid,ReplyTo},{User,Amount})->
    gen_server:cast(Pid, {ReplyTo,{withdraw,{User,Amount}}},?CALL_TIMEOUT).

get_balance({Pid,ReplyTo},User)->
    gen_server:cast(Pid, {ReplyTo,{get_balance,User}},?CALL_TIMEOUT).
send({Pid,ReplyTo},{From_User,To_User,Amount})->
    gen_server:cast(Pid, {ReplyTo,{send,From_User,To_User,Amount}},?CALL_TIMEOUT).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
handle_info(timeout,State)->
    {stop,State}.
handle_cast({ReplyTo,{create_user,User}},State) ->
    Reply=case ex_banking_account_map:get_user(User) of
             user_already_exists -> user_already_exists;
             {ok,_U}-> {ok,Pid}=ex_banking_account_sup:create_account_worker(User),
                            Ref=erlang:monitor(process, Pid),
                            ex_banking_account_map:create_user(User, Ref, Pid)                          
          end,
    ex_banking_enqueuer:send_result(ReplyTo,Reply),
    {noreply,State};


handle_cast({ReplyTo,{get_balance,Uid}},State)->
    Reply=case ex_banking_account_map:get_user(Uid) of
        user_does_not_exist-> user_does_not_exist;
        {ok,User}->ex_banking_account_worker:get_balance(User#user.pid)
    end,
    ex_banking_enqueuer:send_result(ReplyTo,Reply),
    {noreply,State};
 

handle_cast({ReplyTo,{deposit,{Uid,Amount}}},State) ->
    Reply=case ex_banking_account_map:get_user(Uid) of
             user_does_not_exist-> user_does_not_exist;
            {ok,User} -> ex_banking_account_worker:deposit(User#user.pid,Amount)
            
           end,
    ex_banking_enqueuer:send_result(ReplyTo,Reply),
    {noreply,State};

handle_cast({ReplyTo,{withdraw,{Uid,Amount}}},State)->
    Reply=case ex_banking_account_map:get_user(Uid) of
        user_does_not_exist-> user_does_not_exist;
        {ok,User} ->ex_banking_account_worker:withdraw(User#user.pid,Amount)
    end,
    ex_banking_enqueuer:send_result(ReplyTo,Reply),
    {noreply,State};

handle_cast({ReplyTo,{send,{From_Uid,To_Uid,Amount}}},State)->
    Reply=case begin F_U=ex_banking_account_worker:get_user(From_Uid),
                     T_U=ex_banking_account_worker:get_user(To_Uid),
                     {F_U,T_U} 
                end 
           of 
            {user_does_not_exist,_} -> user_does_not_exist;
            {_,user_does_not_exist} -> user_does_not_exist;
            {{ok,From_User},{ok,To_User}}-> handle_send(From_User#user.pid, To_User#user.pid, Amount)
    end,
    ex_banking_enqueuer:send_result(ReplyTo,Reply),
    {noreply,State}.

handle_send(From_User_Pid,To_User_Pid,Amount)->
    WithdrawResult=ex_banking_account_worker:withdraw(From_User_Pid, Amount),
    handle_withdraw(WithdrawResult,To_User_Pid,Amount).

handle_withdraw(not_enough_money,_,_)->not_enough_money;
handle_withdraw({ok,_},To_Pid,Amount)->
    ex_banking_account_worker:deposit(To_Pid, Amount).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%