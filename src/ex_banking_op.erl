-module(ex_banking_op).
-behaviour(gen_server).

-export([start_link/0,init/1]).
-export([notify_user_created/1,notify_job_started/1,notify_job_finished/1]).
-export([handle_cast/2,handle_call/3]).


-define(MAX_OPS,10).
-record(state,{
    users=[]
}).
-record(user,{
    id,
    opCount=0
}).
-define(NAME,?MODULE).

%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [], []).


init(Args)->
    {ok,#state{users=dict:new()}}.

notify_user_created(User)->
    gen_server:call(?NAME, {user_created,User}).

notify_job_started(User)->
    gen_server:cast(?NAME, {job_started,User}).
notify_job_finished(User)->
    gen_server:cast(?NAME, {job_finished,User}).
%%%% Handlers

handle_cast({job_started,User},State)->
    Reply=case dict:find(User, State#state.users) of 
                error -> throw({unknown_user,User});
                {ok,User}->NewUser=User#user{opCount=User#user.opCount-1},
                           NewDict=dict:store(User, NewUser,State#state.users),
                          {noreply,State#state{users=NewDict}}
             end,
    Reply;

handle_cast({job_finished,User},State)->
    NewState=case dict:find(User, State#state.users) of 
                 error -> throw({unknown_user,User});
                 {ok,User}->NewUser=User#user{opCount=User#user.opCount-1},
                            NewDict=dict:store(User, NewUser,State#state.users),
                            {noreply,State#state{users=NewDict}}
             end,
    {noreply,NewState}.

handle_call({user_created,User},_From,State)->
    {reply,{ok,{user_created,User}},State}.

