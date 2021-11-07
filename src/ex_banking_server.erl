-module(ex_banking_server).
-behaviour(gen_server).

-export([start_link/0,init/1]).

-export([create_user/1,get_balance/1,deposit/2,withdraw/2,send/3]).


-record(state,{
    users=[]
}).
-define(NAME,?MODULE).

%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [], []).


init(Args)->
    {ok,#state{users=dict:new()}}.
%%%% Handlers

create_user(User)->
    gen_server:call(?NAME,{create_user,User}).

deposit(User,Amount)->
    gen_server:call(?NAME, {deposit,{User,Amount}}).

withdraw(User,Amount)->
    gen_server:call(?NAME, {withdraw,{User,Amount}}).

get_balance(User)->
    gen_server:call(?NAME, {get_balance,User}).
send(From_User,To_User,Amount)->
    gen_server:call(?NAME, {send,From_User,To_User,Amount}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
    

handle_call({create_user,User},_From,State) ->
    case ex_banking_account_server:user_exists(User) of 
        true -> gen_server:reply(_From, user_already_exists),
                {noreply,State};
        _ -> {reply,ex_banking_account_server:create_user(User),State}
    end;


handle_call({get_balance,Uid},_From,State)->
    case ex_banking_account_server:user_exists(Uid) of 
        true -> Result=ex_banking_account_server:get_balance(Uid),
                gen_server:reply(_From, Result),
                {noreply,State};
        _ -> {reply,user_does_not_exist,State}      
    end;

handle_call({deposit,{Uid,Amount}},_From,State) ->
    case ex_banking_account_server:user_exists(Uid) of 
        true -> Result=ex_banking_account_server:deposit(Uid,Amount),
                gen_server:reply(_From, Result),
                {noreply,State};
        _ -> {reply,user_does_not_exist,State}
    end;

handle_call({withdraw,{Uid,Amount}},_From,State)->
    case ex_banking_account_server:user_exists(Uid) of 
        true -> Result=ex_banking_account_server:withdraw(Uid,Amount),
                gen_server:reply(_From, Result),
                {noreply,State};
        _ -> {reply,user_does_not_exist,State}
    end;
handle_call({send,{Uid,Amount}},_From,State)->
    case ex_banking_account_server:user_exists(Uid) of 
        true -> Result=ex_banking_account_server:withdraw(Uid,Amount),
                gen_server:reply(_From, Result),
                {noreply,State};
        _ -> {reply,user_does_not_exist,State}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%