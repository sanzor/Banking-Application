-module(ex_banking_client_worker).
-behaviour(gen_server).
-export([create_user/2,get_balance/2,deposit/2,withdraw/2,send/2]).
-export([handle_call/3,handle_cast/2,init/1,start_link/0,send_result/2]).

-define(NAME,?MODULE).
-record(state,{

}).

%%%%  Call API %%%%%%%%%%%%%%%%%
start_link()->
    {ok,Pid}=gen_server:start_link({local,?NAME}, ?NAME, [],[]),
    {ok,Pid}.

init(_)->
    {ok,#state{}}.
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
    ok=ex_banking_enqueuer:process_message({create_user,User}),
    {reply,ok,State};
handle_call({get_balance,User,Currency},_From,State)->
    try 
        {ok,_}=ex_banking_traffic_server:add_user_job(User),
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,Balance}=ex_banking_enqueuer:process_message({get_balance,User}),
        ok=ex_banking_traffic_server:remove_user_job(User),
        {reply,{ok,Balance/Coefficient},State}
    catch
        Err->{reply,Err,State}
    end;

handle_call({deposit,{User,Amount,Currency}},_From,State)->
    try 
        {ok,_}=ex_banking_traffic_server:add_user_job(User),
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,NewBalance}=ex_banking_enqueuer:process_message({deposit,{User,Amount*Coefficient}}),
        ok=ex_banking_traffic_server:remove_user_job(User),
        {reply,{ok,NewBalance/Coefficient},State}
    catch
        Err->{reply,Err,State}
    end;
handle_call({withdraw,{User,Amount,Currency}},_From,State)->
    try 
        {ok,_}=ex_banking_traffic_server:add_user_job(User),
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,NewBalance}=ex_banking_enqueuer:process_message({withdraw,{User,Amount*Coefficient}}),
         ok=ex_banking_traffic_server:remove_user_job(User),
        {reply,{ok,NewBalance/Coefficient},State}
    catch
        Err->{reply,Err,State}
    end;

handle_call({send,{From_User,To_User,Amount,Currency}},_From,State)->
    try 
         ok=handle_send_traffic(From_User, To_User),
        {ok,Coefficient}=ex_banking_currency_server:get_currency(Currency),
        {ok,FromNewBalance,ToNewBalance}=ex_banking_enqueuer:process_message({send,{From_User,To_User,Amount*Coefficient}}),
        ok=ex_banking_traffic_server:remove_user_job(From_User),
        ok=ex_banking_traffic_server:remove_user_job(To_User),
        {reply,{ok,FromNewBalance/Coefficient,ToNewBalance/Coefficient},State}
    catch
        Err->{reply,Err,State}
    end.

handle_send_traffic(From_User,To_User)->
        ok=handle_traffic({from,From_User}),
        ok=handle_traffic({to,To_User}),
        ok.


handle_traffic({to,To_User})->
    try
        {ok,_}=ex_banking_traffic_server:add_user_job(To_User),
         ok
    catch
        too_many_requests_to_user->too_many_requests_to_receiver;
        Err->Err
    end;
handle_traffic({from,From_User})->
    try
        {ok,_}=ex_banking_traffic_server:add_user_job(From_User),
         ok
    catch
        too_many_requests_to_user->too_many_requests_to_sender;
        Err->Err
    end.
    
    