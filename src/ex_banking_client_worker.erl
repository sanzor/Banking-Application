-module(ex_banking_client_worker).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_info/2]).

-export([create_user/2,get_balance/2,deposit/2,withdraw/2,send/2]).

-record(user,{
    ref,
    pid
}).
-record(state,{
    users=[]
}).
-define(NAME,?MODULE).
-define(TIMEOUT,4000).
-define(CALL_TIMEOUT,10000).
%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [], []).


init(Args)->
    {ok,#state{users=dict:new()}}.
%%%% Handlers

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
handle_call({ReplyTo,{create_user,User}},State) ->
    case ex_banking_account_map:get_user(User) of
        user_already_exists -> {reply,user_already_exists,State};
        {ok,_U}-> {ok,Pid}=ex_banking_account_sup:create_account_worker(User),
                   Ref=erlang:monitor(process, Pid),
                   Reply=ex_banking_account_map:create_user(User, Ref, Pid),
                   gen_server:reply(ReplyTo, Reply),
                   {noreply,State}
    end;


handle_call({ReplyTo,{get_balance,Uid}},State)->
    case ex_banking_account_map:get_user(Uid) of
        {ok,User}-> Reply=ex_banking_account_worker:get_balance(User#user.pid),
                    gen_server:reply(ReplyTo, Reply),
                    {noreply,State};
        user_does_not_exist->gen_server:reply(ReplyTo, user_does_not_exist),
                            {noreply,State}
    end;
 

handle_call({deposit,{Uid,Amount}},_From,State) ->
    case ex_banking_account_map:get_user(Uid) of 
        {ok,User} -> {reply,ex_banking_account_worker:deposit(User#user.pid,Amount),State};
         user_does_not_exist -> {reply,user_does_not_exist,State}
    end;

handle_call({withdraw,{Uid,Amount}},_From,State)->
    case ex_banking_account_map:get_user(Uid) of 
        {ok,User} ->{reply,ex_banking_account_worker:withdraw(User#user.pid,Amount),State};
        user_does_not_exist -> {reply,user_does_not_exist,State}
    end;
handle_call({send,{From_Uid,To_Uid,Amount}},_From,State)->
    case begin F_U=ex_banking_account_worker:get_user(From_Uid),
               T_U=ex_banking_account_worker:get_user(To_Uid),
               {F_U,T_U} 
          end of 
        {user_does_not_exist,_} -> {reply,user_does_not_exist,State};
        {_,user_does_not_exist} -> {reply,user_does_not_exist,State};
        {{ok,From_User},{ok,To_User}}-> {reply,handle_send(From_User#user.pid, To_User#user.pid, Amount),State}
    end.

handle_send(From_User_Pid,To_User_Pid,Amount)->
    WithdrawResult=ex_banking_account_worker:withdraw(From_User_Pid, Amount),
    handle_withdraw(WithdrawResult,To_User_Pid,Amount).

handle_withdraw(not_enough_money,_,_)->not_enough_money;
handle_withdraw({ok,_},To_Pid,Amount)->
    ex_banking_account_worker:deposit(To_Pid, Amount).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%