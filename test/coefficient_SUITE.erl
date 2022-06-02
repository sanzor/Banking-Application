-module(coefficient_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-define(ip,ct:get_config(ip)).
-define(port,ct:get_config(port)).

suite()->
    [{timetrap,{seconds,30}}].

groups()->[].
all()->
    [
        can_add_coefficient
].
init_per_suite(Config)->
    P=open_port({spawn,"redis-server"}, []),
    [#{port =>P}].

end_per_suite(Config)->
    Port=proplists:get_value(port, Config),
    port_close(Port).

init_per_testcase(_Testcase,Config)->Config.
end_per_testcase(_Config)->ok.

can_add_coefficient()->
    Currency="eur",
    Coefficient=1.5,
    ex_banking_coefficient_server:add_coefficient(Currency, Coefficient),
    {ok,Coefficient}=ex_banking_coefficient_server:get_coefficient(Currency),
    ok.


