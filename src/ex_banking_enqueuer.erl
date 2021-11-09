-module(ex_banking_enqueuer).
-behaviour(gen_server).

-export([start_link/0,init/1]).

-export([handle_cast/2,handle_info/2,handle_call/3]).

-export([consume/0,process_message/1,send_result/2]).
-record(state,{
    queue
}).

-define(NAME,?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    API     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [], []).

process_message(Message)->
    gen_server:call(?NAME,{process_message,Message}).

consume()->
    gen_server:call(?NAME, consume_message).

send_result(To,Message)->
    gen_server:cast(?NAME, {send_result,{To,Message}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Handlers      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_)->
    {ok,#state{queue=queue:new()}}.

handle_info(timeout,State)->
    {stop,State}.
handle_cast({send_result,{To,Message}},State)->
    gen_server:reply(To,Message),
    {noreply,State}.
handle_call({process_message,Message},_From,State)->
    {noreply,State#state{queue=queue:in({_From,Message}, State#state.queue)}};

handle_call(consume,_From,State)->
    case queue:out(State#state.queue) of
        {{value,Item},RemQueue}->{reply,Item,State#state{queue=RemQueue}};
        {empty,_}->{reply,empty,State}
    end.











