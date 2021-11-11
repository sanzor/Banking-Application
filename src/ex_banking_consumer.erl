-module(ex_banking_consumer).
-export([start_link/0]).
-define(NAME,?MODULE).

-record(state,{
}).
start_link()->
    Pid=spawn_link(?MODULE, fun()->startl() end),
    {ok,Pid}.
startl()->
    loop(#state{}).


loop(State=#state{})->
    Message=ex_banking_enqueuer:consume(),
    {ok,WorkerPid}=ex_banking_client_worker_sup:fetch_worker(),
    ex_banking_client_worker:handle_bank_request(WorkerPid, Message),
    loop(State#state{}).



        
