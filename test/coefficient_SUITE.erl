-module(coefficient_SUITE).
-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-define(ip,ct:get_config(ip)).
-define(port,ct:get_config(port)).

-export([all/0,suite/0,init_per_suite/1,end_per_suite/1,init_per_testcase/2,end_per_testcase/1]).

-export([can_add_coefficient/0]).
suite()->
    [{timetrap,{seconds,30}}].

groups()->[].
all()->
    [
        can_add_coefficient
].
init_per_suite(Config)->
    P=open_port({spawn,"redis-server"}, []),
    application:ensure_started(ex_banking),
    [{port,P}|Config].

end_per_suite(Config)->
    Port=proplists:get_value(port, Config),
    %port_close(Port),
    Config.

init_per_testcase(_Testcase,Config)->Config.
end_per_testcase(_Config)->ok.

can_add_coefficient()->
    Currency="eur",
    Coefficient=1.5,
    ex_banking_coefficient_server:add_coefficient(Currency, Coefficient),
    {ok,Coefficient}=ex_banking_coefficient_server:get_coefficient(Currency),
    ok.




