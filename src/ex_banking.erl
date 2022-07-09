-module(ex_banking).
-behaviour(gen_server).

-define(SERVER,?MODULE).
-export([handle_call/3,handle_cast/2,init/1,start_link/0]).

-export([create_user/1,
         delete_user/1,
         deposit/3,
         withdraw/3,
         get_balance/2,
         send/4]).

-export([get_coefficient/1,
         add_coefficient/2,
         remove_coefficient/1,
         update_coefficient/2]).
%----------------------------------------------------------------------------------
%--------------------Banking  API--------------------------------------------------
%----------------------------------------------------------------------------------
% 


start_link()->
    gen_server:start_link({local,?SERVER}, ?MODULE,[], []).


-spec create_user(User :: string())-> ok | {error, wrong_arguments} | user_already_exists.
create_user(User) when not is_list(User) , not is_atom(User)->{error,wrong_arguments};
create_user(User)->
    gen_server:call(?SERVER, {create_user,User}).
            
-spec delete_user(User::string()|list())-> ok | user_does_not_exist.
delete_user(User)->
    gen_server:call(?SERVER,{delete_user,User}).


-spec get_balance(User :: string(), Currency :: string()) 
                  -> {ok, Balance::number()} | {error, wrong_arguments }| 
                        user_does_not_exist | too_many_requests_to_user.
get_balance(User,Currency)->
    gen_server:call(?SERVER, {get_balance,{User,Currency}}).
            



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
    gen_server:call(?SERVER, {deposit,{User,Amount,Currency}}).
           




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
    gen_server:call(?SERVER, {withdraw,{User,Amount,Currency}}).
           
            


-spec send(From_User :: string(), To_User :: string(), Amount :: number(), Currency :: string()) ->
                                     {ok, From_User_Balance :: number(), To_User_Balance :: number()} |
                                     {error, wrong_arguments} | not_enough_money | sender_does_not_exist | 
                                     receiver_does_not_exist | too_many_requests_to_sender | 
                                     too_many_requests_to_receiver.
send(_From_User,_To_User,Amount,Currency) when 
                not is_number(Amount) or 
                not (is_atom(Currency) or is_list(Currency))->{error,wrong_arguments};
            
send(From_User,To_User,Amount,Currency)->
    gen_server:call(?SERVER, {send,{From_User,To_User,Amount,Currency}}).


%%
% coefficient API
%%

-spec get_coefficient(CoefficientId)->{ok,{CoefficientId, Coefficient::number()}} | currency_not_found |  {error , wrong_arguments} 
                                    when CoefficientId:: list() | atom().
get_coefficient(CoefficientId) when not is_list(CoefficientId) , not is_atom(CoefficientId)->
    {error,invalid_arguments};
get_coefficient(CoefficientId)->
    ex_banking_coefficient_server:get_coefficient(CoefficientId).



-spec add_coefficient(Currency::string(), Coefficient::number())->{ok,{added,Currency}} | 
                                                      currency_already_exists |{error , wrong_arguments}
                                                      when Currency :: list() | atom() .
add_coefficient(Currency,Coefficient) when not is_number(Coefficient) ;not Coefficient>0; not (is_list(Currency) or is_atom(Currency))  ->
    {error,invalid_arguments};

add_coefficient(Currency,Coefficient)->
    ex_banking_coefficient_server:add_coefficient(Currency, Coefficient).



-spec remove_coefficient(Currency)->{ok,{removed,Currency}}  | {error , wrong_arguments}
                                    when Currency:: list() | atom().
remove_coefficient(Currency) when not is_atom(Currency) , not is_list(Currency) ->
    {error,invalid_arguments};

remove_coefficient(Currency)->
    ex_banking_coefficient_server:remove_coefficient(Currency).



-spec update_coefficient(Currency, Coefficient::number())->{ok,{updated,Currency}} |
                                                         currency_does_not_exist | {error , wrong_arguments}
                                                         when Currency:: list() | atom().
update_coefficient(Currency,Coefficient) when not is_number(Coefficient) ; not Coefficient>0 ; not is_list(Currency) , not is_atom(Currency) ->
    {error,wrong_arguments};

update_coefficient(Currency,Coefficient)->
    ex_banking_coefficient_server:update_coefficient(Currency, Coefficient).


%%%
%  Handlers
%%%

init(Args)->
    {ok,{}}.

handle_cast(_Args,State)->
    {noreply,State}.


handle_call({create_user,User},From,State)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
     Result= ex_banking_worker:fwd_create_user(Pid,From,User),
     gen_server:reply(From,Result),
     {noreply,State};

handle_call({create_user,User},From,State)->
     {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
     Result= ex_banking_worker:fw(Pid,From,User),
     gen_server:reply(From,Result),
     {noreply,State};
    

handle_call({get_balance,{User,Currency}},From,State)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    {ok,Balance}= ex_banking_worker:fwd_get_balance(Pid,From,{User,Currency}),
    gen_server:reply(From,{ok,Balance}),
    {noreply,State};


handle_call({deposit,{User,Amount,Currency}},From,State)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    {ok,NewBalance}= ex_banking_worker:fwd_deposit(Pid,From,{User,Amount,Currency}),
    gen_server:reply(From,{ok,NewBalance}),
    {noreply,State};

handle_call({withdraw,{User,Amount,Currency}},From,State)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    Result=ex_banking_worker:fwd_withdraw(Pid,From,{User,Amount,Currency}),
    gen_server:reply(From,Result),
    {noreply,State};

handle_call({send,{From_User,To_User,Amount,Currency}},From,State)->
    {ok,Pid}=ex_banking_worker_sup:fetch_worker(),
    Result=ex_banking_worker:fwd_send(Pid,From,{From_User,To_User,Amount,Currency}),
    gen_server:reply(From,Result),
    {noreply,State}.
