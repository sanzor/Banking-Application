-module(ex_banking_account_server).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_call/3]).
-export([create_user/2,get_balance/2,deposit/3,withdraw/3,send/3,user_exists/2]).

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

user_exists(SenderPid,User)->
    gen_server:call(?NAME, {SenderPid,{user_exists,User}}).
create_user(SenderPid,User)->
    gen_server:call(?NAME,{SenderPid,{create_user,User}}).

get_balance(SenderPid,User)->
    gen_server:cast(?NAME, {SenderPid,{get_balance,User}}).
deposit(SenderPid,User,Amount)->
    gen_server:cast(?NAME, {SenderPid,{deposit,{User,Amount}}}).

withdraw(SenderPid,User,Amount)->
    gen_server:cast(?NAME, {SenderPid,{withdraw,{User,Amount}}}).

send(From_User,To_User,Amount)->
    gen_server:call(?NAME, {send,From_User,To_User,Amount}).

%%%%%%%%%%%%%%%%%%%%%% Handlers
%%%
init(_Args)->
    {ok,#state{accounts=dict:new()}}.

handle_cast({ReplyTo,{user_exists,User}},State)->
    case dict:is_key(User,State#state.accounts) of
        true->  gen_server:reply(ReplyTo, true),
                {noreply,State};
                
        _ ->    gen_server:reply(ReplyTo, user_does_not_exist),
                {noreply,State}
    end;
    

handle_cast({ReplyTo,{create_user,User}},State) ->
    {ok,Pid}=ex_banking_account_sup:create_account_worker(User,ReplyTo),
    {Ref,Pid}=erlang:monitor(process,Pid),
    NewDict= dict:store(User,#user{ref=Ref,pid=Pid},State#state.accounts),
    {noreply,State#state{accounts=NewDict}};

handle_cast({ReplyTo,{get_balance,Uid}},State)->
        {ok,User}=dict:find(Uid, State#state.accounts),
        ex_banking_account_worker:get_balance(User#user.pid,ReplyTo),
        {noreply,State};

handle_cast({ReplyTo,{deposit,{Uid,Amount}}},State) ->
        {ok,User}=dict:find(Uid, State#state.accounts),
        ex_banking_account_worker:deposit(User#user.pid,{ReplyTo, Amount}),
        {noreply,State};

handle_cast({ReplyTo,{withdraw,{Uid,Amount}}},State)->
        {ok,User}=dict:find(Uid, State#state.accounts),
        ex_banking_account_worker:withdraw(User#user.pid, {ReplyTo,Amount}),
        {noreply,State}.

handle_call({send,From_Uid,To_Uid,Amount},_From,State)->
        {ok,From_User}=dict:find(From_Uid, State#state.accounts),
        {ok,To_User}=dict:find(To_Uid, State#state.accounts),
        case ex_banking_account_worker:withdraw(From_User#user.pid, Amount) of
            not_enough_money->{reply,not_enough_money,State};
            {ok,NewFromBalance}-> {ok,NewToBalance}=ex_banking_account_worker:deposit(To_User#user.pid, Amount),
                                  {reply,NewFromBalance,NewToBalance}
        end.
                                    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




    


