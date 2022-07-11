-module(ex_banking_basic_SUITE).
-author("adriansan_93@yahoo.com").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_group/2,
    end_per_group/2]).

-export([can_create_user/1,
        can_delete_user/1,
        can_not_create_user_multiple_times/1,
        can_create_users/1,
        can_not_create_with_wrong_username/1]).

-export([can_get_balance/1,
         can_initialize_balance_correctly/1]).

-export([does_not_throw_on_deposit/1,
         can_deposit_correctly/1,
         can_not_deposit_illegal_amount/1,
         can_deposit_in_multiple_currencies/1]).

-export([can_not_withdraw_on_default/1,
         can_withdraw_all/1,
         can_withdraw_some/1,
         
         can_deposit_withdraw_same_currency/1,
         can_deposit_withdraw_different_currencies/1]).

-export([can_send/1,
         can_not_send_with_no_sender/1,
         can_not_send_with_no_receiver/1,
         can_not_send_with_not_enough_balance/1]).

init_per_suite(_Config)->
    P=open_port({spawn,"redis-server"}, []),
    application:ensure_all_started(ex_banking),
    [{port,P}|_Config].

end_per_suite(_Config)->
    Port=proplists:get_value(port, _Config),
    port_close(Port),
    _Config.


init_per_testcase(_Case,_Config)->
    _Config.
end_per_testcase(_Case,_Config)->
    ok.

init_per_group(_,_Config)->_Config.
end_per_group(_,_Config)->_Config.
all()->[
    
    can_create_user,
    can_delete_user,
    can_create_users,
    can_not_create_with_wrong_username,
    can_not_create_user_multiple_times,

    can_get_balance,
    can_initialize_balance_correctly,

    does_not_throw_on_deposit,
    can_deposit_correctly,
    can_not_deposit_illegal_amount,
    can_deposit_in_multiple_currencies,

    can_not_withdraw_on_default,
    can_withdraw_all,
    can_withdraw_some,

    can_deposit_withdraw_same_currency,
    can_deposit_withdraw_different_currencies,

    can_send,
    can_not_send_with_no_sender,
    can_not_send_with_no_receiver,
    can_not_send_with_not_enough_balance
].

groups()->[
    
    {user,[],
        [can_create_user,
        can_delete_user]
    }
].
can_create_user(_Config)->
    ok=ex_banking:create_user(some_user).

can_delete_user(_Config)->
    {ok,_Something}=ex_banking:delete_user(some_user_2),
    ok=ex_banking:create_user(some_user_2),
    ok=ex_banking:delete_user(some_user_2),
    user_does_not_exist=ex_banking:delete_user(some_user_2).

can_create_users(_Config)->
    Ls=lists:map(fun(Elem)->erlang:integer_to_list(Elem) end, lists:seq(10,100)),

    [?assertMatch(ok,ex_banking:create_user(X)) || X <-  Ls].
    
    

can_not_create_with_wrong_username(_Config)->
    {error,wrong_arguments}=ex_banking:create_user(1),
    {error,wrong_arguments}=ex_banking:create_user({a,b}).
can_not_create_user_multiple_times(_Config)->
    ok=ex_banking:create_user(some_user),
    user_already_exists=ex_banking:create_user(some_user).

can_get_balance(_Config)->
    {Currency,Coef}={usd,0.75},
    User=adi,
    ex_banking:add_coefficient(Currency,Coef),
    ok=ex_banking:create_user(User),
    ?assertMatch({ok,_},ex_banking:get_balance(User, Currency)).

can_initialize_balance_correctly(_Config)->
    {Currency,Coef}={usd,0.75},
    User=adi,
    ex_banking:add_currency(Currency,Coef),
    ok=ex_banking:create_user(User),
    {ok,Balance}=ex_banking:get_balance(User, Currency),
    ?assertEqual(Balance, 0.0).

does_not_throw_on_deposit(_Config)->
    {Currency,Coef}={usd,0.75},
    Amount=100.2,
    User=adi,
    ex_banking:add_currency(Currency,Coef),
    ok=ex_banking:create_user(User),
    ?assertMatch({ok,_},ex_banking:deposit(User, Amount, Currency)).

can_deposit_correctly(_Config)->
    {User,Currency,Coef,Amount}={adi,usd,0.75,100},
    ex_banking:add_currency(Currency,Coef),
    ok=ex_banking:create_user(User),
    {ok,OldBalance}=ex_banking:get_balance(User,Currency),
    {ok,NewBalance}=ex_banking:deposit(User, Amount, Currency),
    ?assertEqual(OldBalance,NewBalance-Amount).

can_deposit_in_multiple_currencies(_Config)->
    {User,{Currency2,Coef2},Amount}={adi,{usd,0.75},100},
    ex_banking:add_currency(Currency2,Coef2),
    ok=ex_banking:create_user(User),
    {ok,_}=ex_banking:deposit(User, Amount, eur),
    {ok,NewBalance2}=ex_banking:deposit(User, Amount, Currency2),
    ?assertEqual(NewBalance2, Amount+Amount/Coef2).
  

can_not_deposit_illegal_amount(_Config)->
    {Currency,Coef}={usd,0.75},
    User=adi,
    ex_banking:add_currency(Currency,Coef),
    ok=ex_banking:create_user(User),
    ?assertMatch({error,wrong_arguments},ex_banking:deposit(User, -100, Currency)),
    ?assertMatch({error,wrong_arguments},ex_banking:deposit(User, -1.5, Currency)),
    ?assertMatch({error,wrong_arguments},ex_banking:deposit(User, -10200.5, Currency)).

can_not_withdraw_on_default(_Config)->
    {User,Amount,Currency}={adi,100,eur},
    ok=ex_banking:create_user(adi),
    ?assertEqual(not_enough_money, ex_banking:withdraw(User, Amount, Currency)).

can_withdraw_all(_Config)->
    {User,Amount,Currency,Coef}={adi,100,usd,0.75},
    ex_banking:add_coefficient(Currency,Coef),
    ok=ex_banking:create_user(adi),
    {ok,_}=ex_banking:deposit(User, Amount, Currency),
    {ok,AfterWithdrawBalance}=ex_banking:withdraw(User, Amount, Currency),
    ?assertEqual(AfterWithdrawBalance,0.0).

can_withdraw_some(_Config)->
    {User,DepositAmount,WithdrawAmount,Currency}={adi,100.05,10.04,eur},
    ok=ex_banking:create_user(adi),
    {ok,AfterDepositBalance}=ex_banking:deposit(User, DepositAmount, Currency),
    {ok,AfterWithdrawBalance}=ex_banking:withdraw(User, WithdrawAmount, Currency),
    ?assertEqual(AfterWithdrawBalance,AfterDepositBalance-WithdrawAmount).

can_deposit_withdraw_same_currency(_Config)->
    {User,Currency,Coefficient,DepositAmount,WithdrawAmount}={adi,usd,0.75,100.0,55},
    ok=ex_banking:create_user(adi),
    ex_banking:add_currency(Currency, Coefficient),
    {ok,DepositBalance}=ex_banking:deposit(User, DepositAmount, Currency),
    ?assertEqual(DepositAmount,DepositBalance),
    {ok,WithdrawBalance}=ex_banking:withdraw(User, WithdrawAmount, Currency),
    ?assertEqual(WithdrawBalance, DepositAmount-WithdrawAmount).

can_deposit_withdraw_different_currencies(_Config)->
    User=adi,
    [{DepositCurrency,DepositCoefficient,DepositAmount},
     {WithdrawCurrency,WithdrawCoefficient,WithdrawAmount}]=[{usd,0.75,100.0}, % 100*3/4=75
                                                             {ron,0.2,300.0}], % 1/5*300=60
    ok=ex_banking:create_user(adi),
    ex_banking:add_currency(DepositCurrency, DepositCoefficient),
    ex_banking:add_currency(WithdrawCurrency, WithdrawCoefficient),

    {ok,DepositBalance}=ex_banking:deposit(User, DepositAmount, DepositCurrency),
    ?assertEqual(DepositAmount,DepositBalance),
    {ok,WithdrawBalance}=ex_banking:withdraw(User, WithdrawAmount, WithdrawCurrency),
    ?assertEqual(DepositAmount*(DepositCoefficient/WithdrawCoefficient)-WithdrawAmount*WithdrawCoefficient,WithdrawBalance).


can_send(_Config)->
    {From_User,To_User,Currency}={adi,dan,eur},
    {InitDepositAmount,Take_Amount}={100,75},
    ok=ex_banking:create_user(From_User),
    ok=ex_banking:create_user(To_User),
    {ok,_}=ex_banking:deposit(From_User,InitDepositAmount,Currency),
    Result=ex_banking:send(From_User,To_User, Take_Amount, Currency),
    ?assertMatch({ok,_,_},Result).

can_not_send_with_no_sender(_Config)->
    {From_User,To_User,Currency}={adi,dan,eur},
    {InitDepositAmount,Take_Amount}={100,75},
     ok=ex_banking:create_user(To_User),
     Result=ex_banking:send(From_User,To_User, Take_Amount, Currency),
     ?assertMatch(sender_does_not_exist, Result).

can_not_send_with_no_receiver(_Config)->
        {From_User,To_User,Currency}={adi,dan,eur},
        {InitDepositAmount,Take_Amount}={100,75},
         ok=ex_banking:create_user(From_User),
         {ok,_}=ex_banking:deposit(From_User,InitDepositAmount,Currency),
         Result=ex_banking:send(From_User,To_User, Take_Amount, Currency),
         ?assertMatch(receiver_does_not_exist, Result).

can_not_send_with_not_enough_balance(_Config)->
    {From_User,To_User,Currency}={adi,dan,eur},
    {InitDepositAmount,Take_Amount}={100,125},
    ok=ex_banking:create_user(From_User),
    ok=ex_banking:create_user(To_User),
    {ok,_}=ex_banking:deposit(From_User,InitDepositAmount,Currency),
    Result=ex_banking:send(From_User,To_User, Take_Amount, Currency),
    ?assertMatch(not_enough_money,Result).
