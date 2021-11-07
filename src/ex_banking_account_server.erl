-module(ex_banking_account_server).
-behaviour(gen_server).

-export([start_link/0,init/1]).
-export([handle_info/2]).
-export([create_user/1,get_balance/1,deposit/2,withdraw/2,send/3,user_exists/1]).

-record(state,{
    accounts=[]
}).
-record(user,{
    ref,
    pid
}).

-define(NAME,?MODULE).

%%%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,?NAME},?NAME, [],[]).

user_exists(User)->
    gen_server:call(?NAME, {user_exists,User}).
create_user(User)->
    gen_server:call(?NAME,{create_user,User}).

get_balance(User)->
    gen_server:call(?NAME, {get_balance,User}).
deposit(User,Amount)->
    gen_server:call(?NAME, {deposit,{User,Amount}}).

withdraw(User,Amount)->
    gen_server:call(?NAME, {withdraw,{User,Amount}}).

send(From_User,To_User,Amount)->
    gen_server:call(?NAME, {send,From_User,To_User,Amount}).

%%%%%%%%%%%%%%%%%%%%%% Handlers
%%%
init(_Args)->
    {ok,#state{accounts=dict:new()}}.

handle_call({user_exists,User},_From,State)->
    case dict:is_key(User,State#state.accounts) of
        true-> {reply,{ok,true},State};
        _ ->{reply,user_does_not_exist,State}
    end;
    

handle_call({create_user,User},_From,State) ->
    {ok,Pid}=ex_banking_account_sup:create_account_worker(User),
    {Ref,Pid}=erlang:monitor(process,Pid),
    {ok,_}=  ex_banking_op:notify_user_created(User),
    NewDict=dict:store(User,#user{ref=Ref,pid=Pid},State#state.accounts),
    {reply,ok,State#state{accounts=NewDict}};

handle_call({get_balance,Uid},_From,State)->
        {ok,User}=dict:find(Uid, State#state.accounts),
        ex_banking_op:notify_job_started(Uid),
        Reply=case ex_banking_account_worker:get_balance(User#user.pid) of 
           {ok,Value}-> {reply,{ok,Value},State};
           not_enough_money->{reply,not_enough_money,State};
           Error -> throw({unknown_error,Error})
        end,
        notify_finished_operation(Uid),
        Reply;

handle_call({deposit,{Uid,Amount}},_From,State) ->
        {ok,User}=dict:find(Uid, State#state.accounts),
        ex_banking_op:notify_job_started(Uid),
        Reply=case ex_banking_account_worker:deposit(User#user.pid, Amount) of 
            {ok,Value}-> {reply,{ok,Value},State};
            Error -> throw({unknown_error,Error})
        end,
        notify_finished_operation(Uid),
        Reply;

handle_call({withdraw,{Uid,Amount}},_From,State)->
        {ok,User}=dict:find(Uid, State#state.accounts),
        ex_banking_op:notify_job_started(User),
        Reply=case ex_banking_account_worker:withdraw(User#user.pid, Amount) of 
                {ok,NewBalanceValue}-> {reply,{ok,NewBalanceValue},State};
                not_enough_money->{reply,not_enough_money,State};
                Error -> throw({unknown_error,Error})
              end,
        {reply,Reply,State};

handle_call({send,From_Uid,To_Uid,Amount},_From,State)->
        {ok,From_User}=dict:find(From_Uid, State#state.accounts),
        {ok,To_User}=dict:find(To_Uid, State#state.accounts),
        case ex_banking_account_worker:withdraw(From_User#user.pid, Amount) of
            not_enough_money->{reply,not_enough_money,State};
            {ok,NewFromBalance}-> {ok,NewToBalance}=ex_banking_account_worker:deposit(To_User#user.pid, Amount),
                                  {reply,NewFromBalance,NewToBalance}
        end.
                                    
handle_info({finished_operation,User,_},State)->
        ex_banking_op:notify_job_finished(User),
        {noreply,State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    

notify_finished_operation(User)->
    self() ! {finished_operation,User}.




    


