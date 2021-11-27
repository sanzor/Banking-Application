-module(ex_banking_SUITE).
-author("adriansan_93@yahoo.com").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2]).

-export([can_create_user/1,can_deposit/1]).

init_per_suite(_Config)->
    [].
end_per_suite(_Config)->
    ok.


init_per_testcase(_Case,_Config)->
    case proplists:lookup(ex_banking, application:which_applications()) of
        none -> application:ensure_started(ex_banking);
        _ -> application:stop(ex_banking),
             application:ensure_started(ex_banking)
     end,
    _Config.
end_per_testcase(_Case,_Config)->
    case proplists:lookup(ex_banking, application:which_applications()) of
        none -> ok;
        _ -> application:stop(ex_banking)
    end,
    ok.
all()->[
    can_create_user,
    can_deposit
].

can_create_user(_Config)->
    ?assertEqual(ok,ex_banking:create_user(some_user)),
    ?assertEqual(user_already_exists,ex_banking:create_user(some_user)).

can_deposit(_Config)->
    {Currency,Coef}={usd,0.75},
    Amount=100,
    User=adi,
    ex_banking:create_user(User),
    ex_banking:add_currency(Currency,Coef),
    {ok,Balance}=ex_banking:deposit(User, Amount, Currency),
    ?assertEqual(Balance, Amount).
