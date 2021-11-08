-module(ex_banking_client_worker).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_info/2]).

-export([create_user/1,get_balance/1,deposit/2,withdraw/2,send/3]).

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

create_user(User)->
    gen_server:call(?NAME,{create_user,User},?CALL_TIMEOUT).

deposit(User,Amount)->
    gen_server:call(?NAME, {deposit,{User,Amount}},?CALL_TIMEOUT).

withdraw(User,Amount)->
    gen_server:call(?NAME, {withdraw,{User,Amount}},?CALL_TIMEOUT).

get_balance(User)->
    gen_server:call(?NAME, {get_balance,User},?CALL_TIMEOUT).
send(From_User,To_User,Amount)->
    gen_server:call(?NAME, {send,From_User,To_User,Amount},?CALL_TIMEOUT).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
    

handle_call({create_user,User},_From,State) ->
    case ex_banking_account_map:get_user(User) of
        user_already_exists -> {reply,user_already_exists,State};
        {ok,_U}-> {ok,Pid}=ex_banking_account_sup:create_account_worker(User, _From),
                   Ref=erlang:monitor(process, Pid),
                   Reply=ex_banking_account_map:create_user(User, Ref, Pid),
                  {reply,Reply,State}
    end;


handle_call({get_balance,Uid},_From,State)->
    case ex_banking_account_map:get_user(Uid) of
        {ok,User}->{reply,ex_banking_account_worker:get_balance(User#user.pid),State};
        user_does_not_exist->{reply,user_does_not_exist,State}
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
handle_info(timeout,State)->
    {stop,State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%