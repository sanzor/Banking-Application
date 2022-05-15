-module(ex_banking_api).
-export([create_user/1,deposit/3,withdraw/3,get_balance/2,send/4]).

%----------------------------------------------------------------------------------
%--------------------Banking  API--------------------------------------------------
%----------------------------------------------------------------------------------
% 
-spec create_user(User :: string())-> ok | {error, wrong_arguments} | user_already_exists.
create_user(User) when not is_list(User) , not is_atom(User)->{error,wrong_arguments};
create_user(User)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    Result= ex_banking_worker:create_user(Pid, User),
    Result.
           



-spec get_balance(User :: string(), Currency :: string()) 
                  -> {ok, Balance::number()} | {error, wrong_arguments }| 
                        user_does_not_exist | too_many_requests_to_user.
get_balance(User,Currency)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    {ok,Balance}= ex_banking_worker:get_balance(Pid,{User,Currency}),
    {ok,Balance}.
            



-spec deposit(User :: string(),
              Amount :: number(), 
              Currency :: string())->  {ok, New_balance::number()} |
                                       {error,wrong_arguments} | 
                                       user_does_not_exist | 
                                       too_many_requests_to_user.
deposit(_User,Amount,Currency) when 
                not is_number(Amount) or 
                (is_number(Amount) andalso Amount<0) or
                not (is_atom(Currency) or is_list(Currency))->{error,wrong_arguments};


deposit(User,Amount,Currency)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    Result= ex_banking_worker:deposit(Pid, {User,Amount,Currency}),
    Result.
           




-spec withdraw( User :: string(), Amount :: number(), Currency :: string()) -> 
                                       {ok, New_balance :: number()} |
                                       {error, wrong_arguments} | 
                                       user_does_not_exist | 
                                       not_enough_money | 
                                       too_many_requests_to_user.
withdraw(_User,Amount,Currency) when 
                            not is_number(Amount) or 
                            not (is_atom(Currency) or is_list(Currency))->{error,wrong_arguments};
                    
withdraw(User,Amount,Currency)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    Result=ex_banking_worker:withdraw(Pid,{User,Amount,Currency}),
    Result.
            


-spec send(From_User :: string(), To_User :: string(), Amount :: number(), Currency :: string()) ->
                                     {ok, From_User_Balance :: number(), To_User_Balance :: number()} |
                                     {error, wrong_arguments} | not_enough_money | sender_does_not_exist | 
                                     receiver_does_not_exist | too_many_requests_to_sender | 
                                     too_many_requests_to_receiver.
send(_From_User,_To_User,Amount,Currency) when 
                not is_number(Amount) or 
                not (is_atom(Currency) or is_list(Currency))->{error,wrong_arguments};
            
send(From_User,To_User,Amount,Currency)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    Result=ex_banking_worker:send(Pid,{From_User,To_User,Amount,Currency}),
    Result.