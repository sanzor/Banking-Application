-module(ex_banking_client_worker).
-behaviour(gen_server).
-export([create_user/2,get_balance/2,deposit/2,withdraw/2,send/2]).
-export([handle_call/3,handle_cast/2,init/1,start_link/0,send_result/2]).

-define(NAME,?MODULE).
-record(state,{
   counter_Ref
}).

%%%%  Call API %%%%%%%%%%%%%%%%%
start_link()->
    {ok,Pid}=gen_server:start_link(?NAME, [],[]),
    {ok,Pid}.

init(CounterRef)->
    {ok,#state{counter_Ref=CounterRef}}.
create_user(Pid,User)->
    gen_server:call(Pid,{create_user,User}).
get_balance(Pid,{User,Currency})->
    gen_server:call(Pid,{get_balance,{User,Currency}}).
deposit(Pid,{User,Amount,Currency})->
    gen_server:call(Pid,{deposit,{User,Amount,Currency}}).
withdraw(Pid,{User,Amount,Currency})->
  gen_server:call(Pid,{withdraw,{User,Amount,Currency}}).
send(Pid,{From_User,To_User,Amount,Currency})->
    gen_server:call(Pid,{send,{From_User,To_User,Amount,Currency}}).


send_result(Pid,Message)->
    gen_server:reply(Pid,Message).

%%%%%%%%%%%%%%%%%%%% Handlers %%%%%%%%%%%%
handle_cast(_Request,State)->{noreply,State}.

handle_call({create_user,User},_From,State)->
    {stop,normal,ex_banking_business:create_user(User),State};
   
handle_call({get_balance,{User,Currency}},_From,State)->
         CoefficientResult=ex_banking_currency_server:get_coefficient(Currency),
         Result=handle_with_currency(CoefficientResult,fun (Coefficient)-> 
                                                        GetBalanceResult=ex_banking_business:get_balance(User),
                                                        Reply=handle_get_balance(GetBalanceResult,Coefficient),
                                                        Reply end),
    
                                                  
        {stop,normal,Result,State};
    


handle_call({deposit,{User,Amount,Currency}},_From,State)->
    case ex_banking_currency_server:get_coefficient(Currency) of
        {ok,Coefficient}->case ex_banking_business:deposit(User, Amount*Coefficient) of
                            {ok,NewBalance}->{stop,normal,{ok,NewBalance/Coefficient},State} ;
                             Err -> {stop,normal,Err,State}
                          end;
        currency_does_not_exist -> {get_coefficientwrong_arguments,State};
        Err -> {stop,normal,Err,State}
    end;

    
handle_call({withdraw,{User,Amount,Currency}},_From,State)->
    case ex_banking_currency_server:get_coefficient(Currency) of
        {ok,Coefficient}->case ex_banking_business:withdraw(User, Amount*Coefficient) of
                            {ok,NewBalance}->{stop,normal,{ok,NewBalance/Coefficient},State} ;
                             Err -> {stop,normal,Err,State}
                          end;
        currency_does_not_exist -> {get_coefficientwrong_arguments,State};
        Err -> {stop,normal,Err,State}
    end;

handle_call({send,{From_User,To_User,Amount,Currency}},_From,State)->
    case ex_banking_currency_server:get_coefficient(Currency) of
        {ok,Coefficient}->case ex_banking_business:send(From_User, To_User, Amount*Coefficient) of
                            {ok,From_New_Balance,To_New_Balance}->{stop,normal,{ok,From_New_Balance/Coefficient,To_New_Balance/Coefficient},State} ;
                             Err -> {stop,normal,Err,State}
                          end;
        currency_does_not_exist -> {get_coefficientwrong_arguments,State};
        Err -> {stop,normal,Err,State}
    end;

handle_call(Message,From,State)->
    {reply,{unnkownMessage,Message},State}.

    
handle_with_currency(currency_does_not_exist,_)->currency_does_not_exist;
handle_with_currency({ok,Coefficient},F)->F(Coefficient).

handle_get_balance(too_many_requests_to_user,_)->too_many_requests_to_user;
handle_get_balance({ok,Balance},Coefficient)->{ok,Balance/Coefficient};
handle_get_balance(Err,_)->throw({cant_deal_with,Err}).    