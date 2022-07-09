-module(ex_banking_account_map).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_call/3]).
-export([
    create_user/3,
    delete_user/2,
    get_user/1]).

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

-spec create_user(UserId,Pid,Ref)->  account_already_exists | ok  when Ref::reference(),Pid::pid(),UserId::list()|atom().
create_user(User,Pid,Ref)->
    gen_server:call(?NAME,{create_user,{User,Pid,Ref}}).

-spec delete_user(User::string())->ok | user_does_not_exist.
delete_user(User)->
    gen_server:call(?NAME,{delete_user,User}).
get_user(User)->
    gen_server:call(?NAME, {get_user,User}).

%%%%%%%%%%%%%%%%%%%%%% Handlers
%%%
init(_Args)->
    {ok,#state{accounts=dict:new()}}.

handle_cast(_Message,State)->
    {noreply,State}.
handle_call({get_user,UserId},_From,State)->
    case dict:find(UserId, State#state.accounts) of
        {ok,Value}->{reply,{ok,Value},State};
        error->{reply,user_does_not_exist,State}
    end;
   

handle_call({create_user,{UserId,Pid,Ref}},_From,State)->
    case dict:is_key(UserId,State#state.accounts) of
                true ->{reply,account_already_exists,State};
                false ->NewDict=dict:store(UserId,#user{pid=Pid,ref=Ref},State#state.accounts),
                        {reply,ok,State#state{accounts=NewDict}}
    end;

handle_call({delete_user,UserId},_From,State)->
    case dict:is_key(UserId, State#state.accounts) of
        true ->NewDict= dict:erase(UserId,State#state.accounts),
               {reply,ok,State#state{accounts=NewDict}};
        false-> user_does_not_exist
    end.
                     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




    


