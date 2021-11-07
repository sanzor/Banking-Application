-module(ex_banking_client_worker).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_info/2]).

-export([create_user/1,get_balance/1,deposit/2,withdraw/2,send/3]).


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
    Reply=case ex_banking_account_map:get_user(User) of
        error -> user_already_exists
        {ok,U}->{ok,Pid}=ex_banking_account_sup:create_account_worker(User, From),
                 Ref=erlang:monitor(process, Pid), 
                
    Reply=ex_banking_account_server:create_user(User),
    {reply,Reply,State,?TIMEOUT};


handle_call({get_balance,Uid},_From,State)->
    Reply=ex_banking_account_server:get_balance(Uid),
    {reply,Reply,State,?TIMEOUT};      

handle_call({deposit,{Uid,Amount}},_From,State) ->
    case ex_banking_account_server:user_exists(Uid) of 
        true -> Result=ex_banking_account_server:deposit(Uid,Amount),
                gen_server:reply(_From, Result),
                {noreply,State,?TIMEOUT};
        _ -> {reply,user_does_not_exist,State,?TIMEOUT}
    end;

handle_call({withdraw,{Uid,Amount}},_From,State)->
    case ex_banking_account_server:user_exists(Uid) of 
        true -> Result=ex_banking_account_server:withdraw(Uid,Amount),
                gen_server:reply(_From, Result),
                {noreply,State};
        _ -> {reply,user_does_not_exist,State,?TIMEOUT}
    end;
handle_call({send,{From_Uid,To_Uid,Amount}},_From,State)->
    case begin ex_banking_account_server:user_exists(From_Uid),ex_banking_account_server:user_exists(To_Uid) end of 
        true -> Result=ex_banking_account_server:send(From_Uid, To_Uid, Amount),
                gen_server:reply(_From, Result),
                {noreply,State,?TIMEOUT};
        _ -> {reply,user_does_not_exist,State,?TIMEOUT}
    end.

handle_info(timeout,State)->
    {stop,State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%