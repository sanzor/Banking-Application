-module(ex_banking_account_worker_sup).
-behaviour(supervisor).

-define(NAME,?MODULE).
-export([init/1,start_link/0,create_account_worker/1]).

start_link()->
    supervisor:start_link({local,?NAME}, []).

create_account_worker(User)->
    {ok,Pid}=supervisor:start_child(?NAME,[User]),
    {ok,Pid}.
init(_Args)->
    Strategy={simple_one_for_one,0,1},
    Flags=[#{
        id=>ex_banking_account_worker,
        start=>{ex_banking_account_worker,start_link,[_Args]},
        restart=>transient,
        shutdown=>3000,
        mod=>[ex_banking_account_worker],
        type=>worker
    }],
    {ok,{Strategy,Flags}}.


