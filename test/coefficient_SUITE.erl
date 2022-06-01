-module(coefficient_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-define(ip,ct:get_config(ip)).
-define(port,ct:get_config(port)).

suite()->
    [{timetrap,{seconds,30}}].
init_per_suite(Config)->
    P=open_port({spawn,"redis-server"}, []),
    [#{port =>P}].

end_per_suite(Config)->
    Port=proplists:get_value(port, Config),
    port_close(Port)


can_add_coefficient()->
    ex_banking:
    ok.

