-module(coefficient_SUITE).
-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-define(ip,ct:get_config(ip)).
-define(port,ct:get_config(port)).

-export([all/0,suite/0,init_per_suite/1,end_per_suite/1,init_per_testcase/2,end_per_testcase/1]).

-export([
         can_get_coefficient/1,
         can_add_coefficient/1,
         can_delete_coefficient/1,
         can_update_coefficient/1]).
suite()->
    [{timetrap,{seconds,30}}].

groups()->[].
all()->
    [
        can_get_coefficient,
        can_add_coefficient,
        can_delete_coefficient,
        can_update_coefficient
].
init_per_suite(Config)->
    P=open_port({spawn,"redis-server"}, []),
    {ok,_Started}=application:ensure_all_started(ex_banking),
    [{port,P}|Config].

end_per_suite(Config)->
    % Port=proplists:get_value(port, Config),
    % port_close(Port),
    Config.

init_per_testcase(_Testcase,Config)->Config.
end_per_testcase(_Config)->ok.

can_get_coefficient(_Config)->
    Currency="eur",
    Coefficient=1.5,
    {ok,Con}=eredis:start_link(),
    _=eredis:q(Con,["hset","currencies"|[Currency,float_to_binary(Coefficient)]]),
    {ok,Currency,BinCoefficient}=ex_banking_coefficient_server:get_coefficient(Currency),
    ?assertEqual(float_to_binary(Coefficient),BinCoefficient).
    
can_delete_coefficient(_Config)->
    {ok,Con}=eredis:start_link(),
    Ok=eredis:q(Con,["hset","currencies"|["eur",1]]),
    ex_banking_coefficient_server:remove_coefficient("eur"),
    Ok=eredis:q(Con,["hget","currencies","eur"]).
can_add_coefficient(_Config)->
    Currency="eur",
    Coefficient=1.5,
    BinCoef=float_to_binary(Coefficient),
    ex_banking_coefficient_server:add_coefficient(Currency, BinCoef),
    {ok,Con}=eredis:start_link(),
    {ok,BinCoef}=eredis:q(Con,["hget","currencies",Coefficient]).
   

can_update_coefficient(_Config)->
    Currency="eur",
    InitialCoefficient=1.5,
    NewCoefficient=2.0,
    {ok,Con}=eredis:start_link(),
    _=eredis:q(Con,["hset","currencies"|[Currency,InitialCoefficient]]),
    ex_banking_coefficient_server:update_coefficient(Currency,float_to_binary(NewCoefficient)),
    {ok,BinValue}=eredis:q(Con,["hget","currencies","Currency"]),
    ?assertEqual(float_to_binary(NewCoefficient),BinValue).








