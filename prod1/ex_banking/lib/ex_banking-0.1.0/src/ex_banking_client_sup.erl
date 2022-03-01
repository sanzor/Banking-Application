-module(ex_banking_client_sup).
-behaviour(supervisor).

-define(NAME,?MODULE).
-export([init/1,start_link/0,fetch_worker/0]).
-define(POOL_SIZE,200).
start_link()->
    {ok,Pid}=supervisor:start_link({local,?NAME},?MODULE, []),
    {ok,Pid}.

fetch_worker()->
    {ok,Cpid}=supervisor:start_child(?NAME, []),
    {ok,Cpid}.
init(_)->
    % AtomicsRef=atomics:new(?POOL_SIZE,[{signed,false}]),
    Strategy={simple_one_for_one,2,5},
    Flags=[#{
        id=>ex_banking_client,
        start=>{ex_banking_client,start_link,[]},
        restart=>temporary,
        shutdown=>brutal_kill,
        mod=>[ex_banking_client_worker],
        type=>worker
    }],
    {ok,{Strategy,Flags}}.


