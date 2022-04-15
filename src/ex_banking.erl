%%%-------------------------------------------------------------------
%% @doc ex_banking public API
%% @end
%%%-------------------------------------------------------------------

-module(ex_banking).
-behaviour(application).

-export([start/2, stop/1]).
-export([create_user/1,deposit/3,withdraw/3,get_balance/2,send/4]).
-export([get_currency/1,add_currency/2,remove_currency/1,update_currency/2]).
-define(FU(Key,List),proplists:get_value(Key, List)).

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

%----------------------------------------------------------------------------------
%--------------------Currency API-------------------------------------------------
%----------------------------------------------------------------------------------
get_currency(Currency)->
    ex_banking_currency_server:get_coefficient(Currency).
add_currency(Currency,Coefficient)->
    ex_banking_currency_server:add_currency(Currency, Coefficient).

remove_currency(Currency)->
    ex_banking_currency_server:remove_currency(Currency).

update_currency(Currency,Coefficient)->
    ex_banking_currency_server:update_currency(Currency, Coefficient).


   
start({takeover,Node},Args)->
    io:format("taking over to node ~p",[Node]),
    ex_banking_main_sup:start_link();
start(_StartType, _StartArgs)->
    io:format("\nStarting non-distributed\n"),
    connect_to_cluster(),
    {ok,Pid}=ex_banking_main_sup:start_link(),
    {ok,Pid}.
stop(_State) ->
    ok.

    
connect_to_cluster()->
     {ok,{All,Mandatory}}=get_nodes(),
     ping_nodes(Mandatory).
     
    %  Targets=lists:filter(fun({Name,Importance})->Name =/=node() andalso Importance=:=primary  end, Nodes),
    %  io:format("~p",[Targets]),
    %  ping_nodes(Targets).
    % 
get_nodes()->
        io:format("~p",[node()]),
        {ok,Env}=application:get_env(kernel,distributed),
        io:format("\nEnv:\n~p\n",[Env]),
        AllNodes=?FU(ex_banking,Env),
        io:format("\nNodes:\n ~p\n",[AllNodes]),
        Mandatory=?FU(sync_nodes_mandatory,Env),
        io:format("\nMandatory:\n ~p\n",[Mandatory]),
        ToPingNodes=lists:filter(fun(Elem)->Elem =/= node() end, AllNodes),
        io:format("\nToPing:\n ~p\n",[ToPingNodes]),
        {ok,{AllNodes,ToPingNodes}}.

ping_nodes(List)->
    Results=[net_adm:ping(Name)||Name<-List],
    case lists:any(fun(Elem)->Elem =/= pong end ,Results) of
            true -> throw({error,connect_to_nodes});
            false-> ok
    end.
%% internal functions
