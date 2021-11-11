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
    {ok,Pid}=gen_server:start_link({local,?NAME}, ?NAME, [],[]),
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
    ok=ex_banking_business:create_user(User),
    {stop,normal,ok,State};
handle_call({get_balance,User,Currency},_From,State)->
    try 
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,Balance}=ex_banking_business:get_balance(User),
        {stop,normal,{ok,Balance/Coefficient},State}
    catch
        Err->{stop,normal,Err,State}
    end;

handle_call({deposit,{User,Amount,Currency}},_From,State)->
    try 
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,NewBalance}=ex_banking_business:deposit(User,Amount*Coefficient),
        {stop,normal,{ok,NewBalance/Coefficient},State}
    catch
        Err->{stop,normal,Err,State}
    end;
handle_call({withdraw,{User,Amount,Currency}},_From,State)->
    try 
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,NewBalance}=ex_banking_business:withdraw(User,Amount*Coefficient),
        {stop,normal,{ok,NewBalance/Coefficient},State}
    catch
        Err->{stop,normal,Err,State}
    end;

handle_call({send,{From_User,To_User,Amount,Currency}},_From,State)->
    try 
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,FromNewBalance,ToNewBalance}=ex_banking_business:send(From_User,To_User,Amount*Coefficient),
        {stop,normal,{ok,FromNewBalance/Coefficient,ToNewBalance/Coefficient},State}
    catch
        Err->{stop,normal,Err,State}
    end.



    
    