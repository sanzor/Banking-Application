%%%-------------------------------------------------------------------
%% @doc ex_banking public API
%% @end
%%%-------------------------------------------------------------------

-module(ex_banking).

-behaviour(application).

-export([start/2, stop/1]).

-export([create_user/1,deposit/3,withdraw/3,get_balance/2]).

-spec create_user(User :: string())-> ok | {error, wrong_arguments} | user_already_exists.
create_user(User) when not is_list(User)->{error,wrong_arguments};
create_user(User)->
    ex_banking_server:create_user(User).

-spec deposit(User :: string(),
              Amount :: number(), 
              Currency :: string())->  {ok, New_balance::number()} |
                                       {error,wrong_arguments} | 
                                       user_does_not_exist | 
                                       too_many_requests_to_user.
deposit(User,Amount,Currency)->undefined.

-spec withdraw( User :: string(), Amount :: number(), Currency :: string()) -> 
                                       {ok, New_balance :: number()} |
                                       {error, wrong_arguments} | 
                                       user_does_not_exist | 
                                       not_enough_money | 
                                       too_many_requests_to_user.
withdraw(User,Amount,Currency)->undefined.

-spec get_balance(User :: string(), Currency :: string()) 
                  -> {ok, Balance::number()} | {error, wrong_arguments }| user_does_not_exist | too_many_requests_to_user.
get_balance(User,Currency)->undefined.


-spec send(From_User :: string(), To_User :: string(), Amount :: number(), Currency :: string()) ->
                                     {ok, From_User_Balance :: number(), To_User_Balance :: number()} |
                                     {error, wrong_arguments} | not_enough_money | sender_does_not_exist | 
                                     receiver_does_not_exist | too_many_requests_to_sender | 
                                     too_many_requests_to_receiver.
send(From_User,To_User,Amount,Currency)->undefined.
start(_StartType, _StartArgs) ->
    ex_banking_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
