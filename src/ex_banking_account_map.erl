-module(ex_banking_account_map).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_call/3]).
-export([create_user/3,get_user/1]).

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

create_user(User,Ref,Pid)->
    gen_server:call(?NAME,{create_user,{User,Ref,Pid}}).

get_user(User)->
    gen_server:call(?NAME, {getUser,User}).

%%%%%%%%%%%%%%%%%%%%%% Handlers
%%%
init(_Args)->
    {ok,#state{accounts=dict:new()}}.

handle_call({get_user,User},From,State)->
    gen_server:reply(From, dict:find(User, State#state.accounts)),
    {noreply,State};

handle_call({create_user,{User,Pid,Ref}},_From,State)->
    Reply=case dict:is_key(User,State#state.accounts) of
                true ->{reply,user_already_exists,State};
                false ->NewDict=dict:store(User,#user{pid=Pid,ref=Ref},State#state.accounts),
                        {reply,ok,State#state{accounts=NewDict}}
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
                                    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




    


