-module(ex_banking_account_worker).
-behaviour(gen_server).
-export([init/1,start_link/1]).
-export([handle_call/3,handle_info/2,handle_cast/2]).
-export([deposit/2,withdraw/2,get_balance/2]).

-record(state,{
    id,
    balance=0
}).
-define(NAME,?MODULE).

%%%--------------------------------- API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link({UserId,ReplyTo})->
    gen_server:start_link(?NAME,[{UserId,ReplyTo}], []).

init([{UserId,ReplyTo}])->
    self() ! {replyto,ReplyTo},
    {ok,#state{id=UserId,balance=0}}.

deposit(Pid,{ReplyTo,Amount})->
    gen_server:call(Pid, {ReplyTo,{deposit,Amount}}).

withdraw(Pid,{ReplyTo,Amount})->
    gen_server:call(Pid, {ReplyTo,{withdraw,Amount}}).

get_balance(Pid,ReplyTo)->
    gen_server:call(Pid, {get_balance,ReplyTo}).


%%%% Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% 
handle_cast(_Message,State)->
    {noreply,State}.
handle_info(_Message,State)->
    {noreply,State}.

handle_call({get_balance,ReplyTo},_From,State)->
    Balance=State#state.balance,
    gen_server:reply(ReplyTo, {ok,Balance}),
    {noreply,{ok,Balance},State};
handle_call({ReplyTo,{deposit,Amount}},_From,State)->
    NewBalance=State#state.balance+Amount,
    gen_server:reply(ReplyTo, {ok,NewBalance}),
    {noreply,State#state{balance=NewBalance}};

handle_call({ReplyTo,{withdraw,Amount}},_From,State) when State#state.balance<Amount ->
    gen_server:reply(ReplyTo, not_enough_money),
    {noreply,State};
handle_call({ReplyTo,{withdraw,Amount}},_From,State)->
    NewBalance=State#state.balance-Amount,
    gen_server:reply(ReplyTo, {ok,NewBalance}),
    {noreply,State#state{balance=NewBalance}}.


