%%%-------------------------------------------------------------------
%% @doc ex_banking public API
%% @end
%%%-------------------------------------------------------------------
-module(ex_banking_app).
-behaviour(application).
-export([start/2, stop/1]).
-import(eredis,[start_link/0,q/2]).

%%====================================================================
%% API
%%====================================================================
start({failover,Node},Args)->
    io:format("Failover  ~p",[Node]),
    ex_banking_main_sup:start_link();   
start({takeover,Node},Args)->
    io:format("Takeover from: ~p",[Node]),
    ex_banking_main_sup:start_link();
start(_StartType, _StartArgs)->
    {ok,Pid}=ex_banking_main_sup:start_link(),
    {ok,Pid}.
stop(_State) ->
    ok.

    
