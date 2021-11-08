-module(ex_banking_enqueuer).
-behaviour(gen_server).

-export([start_link/0,init/1]).
-export([handle_cast/2,handle_call/3,consume/0]).
-export([get_balance/1,create_user/1,withdraw/2,deposit/2]).


-define(MAX_OPS,10).
-record(state,{
    queue
}).

-define(NAME,?MODULE).

%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [], []).


init(Args)->
    {ok,#state{queue=queue:new()}}.



create_user(User)->
    gen_server:cast(?NAME,{self(),{create_user,User}}).

deposit(User,Amount)->
    gen_server:cast(?NAME, {self(),{deposit,{User,Amount}}}).

withdraw(User,Amount)->
    gen_server:cast(?NAME, {self(),{withdraw,{User,Amount}}}).

get_balance(User)->
    gen_server:cast(?NAME, {self(),{get_balance,User}}).
send(From_User,To_User,Amount)->
    gen_server:cast(?NAME, {self(),{send,From_User,To_User,Amount}}).

consume()->
    gen_server:call(?NAME, consume_message).
%%%% Handlers

handle_info(timeout,State)->
    {stop,State}.
handle_cast({From,{create_user,User}},State) ->
    {noreply,State#state{queue=queue:in({From,{create_user,User}}, State#state.queue)}};

handle_cast({_From,{get_balance,Uid}},State)->
    {noreply,State#state{queue=queue:in({_From,{get_balance,Uid}}, State#state.queue)}};
 
handle_cast({_From,{deposit,{Uid,Amount}}},State) ->
    {noreply,State#state{queue=queue:in({_From,{deposit,{Uid,Amount}}}, State#state.queue)}};

handle_cast({_From,{withdraw,{Uid,Amount}}},State)->
    {noreply,State#state{queue=queue:in({_From,{withdraw,{Uid,Amount}}}, State#state.queue)}};
handle_cast({_From,{send,{From_Uid,To_Uid,Amount}}},State)->
    {noreply,State#state{queue=queue:in({_From,{send,{From_Uid,To_Uid,Amount}}}, State#state.queue)}}.

handle_call(consume,_From,State)->
    case queue:out(State#state.queue) of
        {{value,Item},RemQueue}->{reply,Item,State#state{queue=RemQueue}};
        {empty,_}->{reply,empty,State}
    end.









