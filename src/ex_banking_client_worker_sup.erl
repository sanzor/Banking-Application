-module(ex_banking_client_worker_sup).
-behaviour(supervisor).

-define(NAME,?MODULE).
-export([init/1,start_link/0,get_pool/0]).
-define(POOL_SIZE,200).
start_link()->
    {ok,Pid}=supervisor:start_link({local,?NAME}, []),
    {ok,pool_created}=create_pool(),
    {ok,Pid}.

get_pool()->
    {ok,lists:map(fun({_,ChildPid,_,_})->ChildPid end, supervisor:which_children(?NAME))}.
create_pool()->
    [create_child()||_<-lists:seq(0,?POOL_SIZE)],
    {ok,pool_created}.

create_child()->
    {ok,Cpid}=supervisor:start_child(?NAME, []),
    {ok,Cpid}.
init(_Args)->
    Strategy={simple_one_for_one,0,1},
    Flags=[#{
        id=>ex_banking_client_worker,
        start=>{ex_banking_client_worker,start_link,[_Args]},
        restart=>permanent,
        shutdown=>brutal_kill,
        mod=>[ex_banking_client_worker],
        type=>worker
    }],
    {ok,{Strategy,Flags}}.


