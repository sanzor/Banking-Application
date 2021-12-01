-module(ex_banking_concurrency_SUITE).
-author("adriansan_93@yahoo.com").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    spawn_test/2]).

-export([can_limit_requests_to_user/1]).


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


all()->[can_limit_requests_to_user].


can_limit_requests_to_user(_Config)->
    
    {User,DepositAmount,RequestCount}={adi,100,1000},
    ok=ex_banking:create_user(User),
    ex_banking:deposit(User, DepositAmount, eur),
    [spawn(?MODULE,spawn_test,[self(),User])|| _<-lists:seq(0, RequestCount)],
    List=[receive Msg -> Msg end || _<-lists:seq(0, RequestCount)],
    ct:log(List),
    
    DidAnyRequestFail=lists:any(fun(Elem)->Elem=:=too_many_requests_to_user end, List),
    ?assertEqual(true,DidAnyRequestFail).

spawn_test(Pid,User)->
    Pid ! ex_banking:get_balance(User, eur).
