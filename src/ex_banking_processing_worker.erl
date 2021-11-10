-module(ex_banking_processing_worker).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_info/2]).

-export([handle_bank_request/2]).

-record(user,{
    ref,
    pid
}).
-record(state,{
}).
-define(NAME,?MODULE).
-define(TIMEOUT,4000).
-define(CALL_TIMEOUT,10000).

%--------------------------------------
%                  API
% -------------------------------------
start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [], []).


init(Args)->
    {ok,#state{}}.


handle_bank_request(To,Message)->
    gen_server:cast(To, Message).



%------------------------- 
% Handlers
% ------------------------


handle_info(timeout,State)->
    {stop,State}.
handle_cast({ReplyTo,{create_user,User}},State) ->
    Reply=case ex_banking_account_map:get_user(User) of
             user_already_exists -> user_already_exists;
             {ok,_U}->          {ok,Pid}=ex_banking_account_sup:create_account_worker(User),
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
            {user_does_not_exist,_} -> sender_does_not_exist;
            {_,user_does_not_exist} -> receiver_does_not_exist;
            {{ok,From_User},{ok,To_User}}-> handle_send(From_User#user.pid, To_User#user.pid, Amount)
    end,
    ex_banking_enqueuer:send_result(ReplyTo,Reply),
    {noreply,State}.

handle_send(From_User_Pid,To_User_Pid,Amount)->
    WithdrawResult=ex_banking_account_worker:withdraw(From_User_Pid, Amount),
    handle_withdraw_result(WithdrawResult,To_User_Pid,Amount).

handle_withdraw_result(not_enough_money,_,_)->not_enough_money;
handle_withdraw_result({ok,FromNewBalance},To_Pid,Amount)->
    case ex_banking_account_worker:deposit(To_Pid, Amount) of
        {ok,ToNewBalance} -> {ok,FromNewBalance,ToNewBalance};
         Err ->Err
    end.
