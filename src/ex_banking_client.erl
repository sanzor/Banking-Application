-module(ex_banking_client).
-behaviour(gen_server).
-include("records.hrl").
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
    gen_server:call(Pid,{Currency,{get_balance,User}}).
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

handle_call({Currency,Request},From,State)->
    {ok,Coefficient}=can_get_coefficient(Currency, State),
    do_call(Coefficient,Request,From,State).

do_call(Coefficient,{get_balance,UserId},_From,State)->
    {ok,User}=get_user(UserId),
    BalanceResult=ex_banking_account:get_balance(User#user.pid),
    {stop,normal,BalanceResult,State}.                             
    
    


% do_call({deposit,{User,Amount,Currency}},_From,State)->
%     case ex_banking_currency_server:get_coefficient(Currency) of
%         {ok,Coefficient}->case ex_banking_business:deposit(User, Amount*Coefficient) of
%                             {ok,NewBalance}->{stop,normal,{ok,NewBalance/Coefficient},State} ;
%                              Err -> {stop,normal,Err,State}
%                           end;
%         currency_does_not_exist -> {get_coefficientwrong_arguments,State};
%         Err -> {stop,normal,Err,State}
%     end;

    
% do_call({withdraw,{User,Amount,Currency}},_From,State)->
%     case ex_banking_currency_server:get_coefficient(Currency) of
%         {ok,Coefficient}->case ex_banking_business:withdraw(User, Amount*Coefficient) of
%                             {ok,NewBalance}->{stop,normal,{ok,NewBalance/Coefficient},State} ;
%                              Err -> {stop,normal,Err,State}
%                           end;
%         currency_does_not_exist -> {get_coefficientwrong_arguments,State};
%         Err -> {stop,normal,Err,State}
%     end;

% do_call({send,{From_User,To_User,Amount,Currency}},_From,State)->
%     case ex_banking_currency_server:get_coefficient(Currency) of
%         {ok,Coefficient}->case ex_banking_business:send(From_User, To_User, Amount*Coefficient) of
%                             {ok,From_New_Balance,To_New_Balance}->{stop,normal,{ok,From_New_Balance/Coefficient,To_New_Balance/Coefficient},State} ;
%                              Err -> {stop,normal,Err,State}
%                           end;
%         currency_does_not_exist -> {wrong_arguments,State};
%         Err -> {stop,normal,Err,State}
%     end;

% do_call(Message,From,State)->
%     {reply,{unnkownMessage,Message},State}.


can_get_coefficient(Currency,State)->
    case ex_banking_currency_server:get_coefficient(Currency) of
        {ok,Coefficient} -> {ok, Coefficient};
        _ -> throw({stop,normal,wrong_arguments,State})
    end.
get_user(UserId)->
    case ex_banking_account_map:get_user(UserId) of
        {ok,User}-> {ok,User};
         user_does_not_exist  -> throw(user_does_not_exist)
    end. 