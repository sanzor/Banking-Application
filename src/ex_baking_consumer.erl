-module(ex_banking_consumer).
-export([start_link/0]).
-define(NAME,?MODULE).

-record(state,{
    worker_count=-1,
    workerPids=[],
    cursorAt=1
}).
start_link()->
    Pid=spawn_link(?MODULE, start, []),
    {ok,Pid}.


start()->
    loop(#state{worker_count=-1}).

loop(State=#state{worker_count=Cnt}) when Cnt<0 ->
     {ok,List}=ex_banking_client_worker_sup:get_pool(),
     loop(State#state{worker_count=erlang:length(List),workerPids=List});

loop(State=#state{workerPids=Workers,cursorAt=Cursor,worker_count=Cnt})->
    Message=ex_banking_enqueuer:consume(),
    WorkerPid=lists:nth(Cursor, Workers),
    case Message of 
        {ReplyTo,Command={create_user,_}}->ex_banking_client_worker:create_user({WorkerPid,ReplyTo},Command);
        {ReplyTo,Command={get_balance,_}}->ex_banking_client_worker:get_balance({WorkerPid,ReplyTo},Command);
        {ReplyTo,Command={deposit,_,_}}->ex_banking_client_worker:deposit({WorkerPid,ReplyTo}, Command);
        {ReplyTo,Command={withdraw,_,_}}->ex_banking_client_worker:withdraw({WorkerPid,ReplyTo},Command);
        {ReplyTo,Command={send,_,_,_}}->ex_banking_client_worker:send({WorkerPid,ReplyTo},Command)
    end,
    NewCursor=if Cursor>Cnt-> 1 ; true -> Cursor+1 end,
    loop(State#state{cursorAt=NewCursor}).



        
