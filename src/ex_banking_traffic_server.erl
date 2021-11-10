-module(ex_banking_traffic_server).
-behaviour(gen_server).

-export([add_user_job/1,remove_user_job/1]).

-export([handle_call/3,handle_cast/2,init/1,start_link/0]).

-define(NAME,?MODULE).
-define(MAX_REQUESTS_PER_USER,10).
-record(state,{
    userTrafficMap
}).
%---------API----------------
% 
start_link()->
    gen_server:start_link({local,?NAME}, ?NAME,[],[]).

-spec add_user_job(User)->{ok,{job_added,User}} | too_many_requests_to_user when User::list()|atom().
add_user_job(User)->
    gen_server:call(?NAME,{add_user_job,User}).

-spec remove_user_job(User::list()|atom())->ok.
remove_user_job(User)->
    gen_server:cast(?NAME,{remove_user_job,User}),
    ok.

%-------------Callbacks----------------
% 
init(_)->
    {ok,#state{userTrafficMap=dict:new()}}.

handle_call({add_user_job,User},_From,State)->
    case dict:find(User, State#state.userTrafficMap) of
        {ok,OngoingRequestCount} when OngoingRequestCount>?MAX_REQUESTS_PER_USER ->{reply,too_many_requests_to_user,State};
        {ok,_} -> NewDict=dict:update(User, fun(CurrentCount)->CurrentCount+1 end, State#state.userTrafficMap),
                                    {reply,{ok,{job_added,User}},State#state{userTrafficMap=NewDict}};
        error -> NewDict=dict:store(User,1, State#state.userTrafficMap),
                 {reply,{ok,{job_added,User}},State#state{userTrafficMap=NewDict}}
    end.

handle_cast({remove_user_job,User},State)->
    case dict:find(User, State#state.userTrafficMap) of
        {ok,OngoingRequestCount} when OngoingRequestCount<1->
                         NewDict=dict:erase(User,State#state.userTrafficMap),
                         {reply,{ok,{job_removed,User}},State#state{userTrafficMap=NewDict}};
        {ok, _} ->NewDict=dict:update(User,fun(CurrentCount)->CurrentCount-1 end,State#state.userTrafficMap),
                 {reply,{ok,{job_removed,User}},State#state{userTrafficMap=NewDict}};
        error ->{reply,{ok,{job_removed,User}},State}
    end.