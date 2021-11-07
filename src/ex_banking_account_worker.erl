-module(ex_banking_account_worker).
-behaviour(gen_server).
-export([init/1,start_link/1]).
-export([handle_call/3,handle_info/2,handle_cast/2]).
-export([deposit/2,withdraw/2,get_balance/1]).

-record(state,{
    id,
    serverId,
    balance=0
}).
-define(NAME,?MODULE).

%%%--------------------------------- API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link({ServerId,UserId})->
    gen_server:start_link(?NAME,[{ServerId,UserId}], []).

init([{ServerId,UserId}])->
    {ok,#state{id=UserId,balance=0,serverId=ServerId}}.

deposit(Pid,Amount)->
    gen_server:call(Pid, {deposit,Amount}).

withdraw(Pid,Amount)->
    gen_server:call(Pid, {withdraw,Amount}).

get_balance(Pid)->
    gen_server:call(Pid, get_balance).


%%%% Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% 
handle_cast(_Message,State)->
    {noreply,State}.
handle_info(_Message,State)->
    {noreply,State}.

handle_call(get_balance,_From,State)->
    Balance=State#state.balance,
    {reply,{ok,Balance},State};
handle_call({deposit,Amount},_From,State)->
    NewBalance=State#state.balance+Amount,
    {reply,{ok,NewBalance},State#state{balance=NewBalance}};

handle_call({withdraw,Amount},_From,State) when State#state.balance<Amount ->
    {reply,not_enough_money,State};
handle_call({withdraw,Amount},_From,State)->
    NewBalance=State#state.balance-Amount,
    {reply,{ok,NewBalance},State#state{balance=NewBalance}}.


