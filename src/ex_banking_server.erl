-module(ex_banking_server).
-behaviour(gen_server).
-export([create_user/1,get_balance/1,deposit/3,withdraw/3,send/4]).
-export([handle_call/3,handle_cast/2,init/1,start_link/0]).

-define(NAME,?MODULE).
-record(state,{

}).

%%%%  Call API %%%%%%%%%%%%%%%%%
start_link()->
    {ok,Pid}=gen_server:start_link({local,?NAME}, ?NAME, [],[]),
    {ok,Pid}.

init(_)->
    {ok,#state{}}.
create_user(User)->
    gen_server:call(?NAME,{create_user,User}).
get_balance(User)->
    gen_server:call(?NAME,{get_balance,User}).
deposit(User,Amount,Currency)->
    gen_server:call(?NAME,{deposit,User,Amount,Currency}).
withdraw(User,Amount,Currency)->
    gen_server:call(?NAME,{withdraw,User,Amount,Currency}).
send(From_User,To_User,Amount,Currency)->
    gen_server:call(?NAME,{send,From_User,To_User,Amount,Currency}).


%%%%%%%%%%%%%%%%%%%% Handlers %%%%%%%%%%%%
handle_cast(_Request,State)->{noreply,State}.
handle_call(Request,_From,State)->
    {ok,Response}=ex_banking_enqueuer:process_message(Request),
    {reply,Response,State}.