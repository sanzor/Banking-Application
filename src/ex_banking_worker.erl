-module(ex_banking_worker).
-behaviour(gen_server).
-include("records.hrl").
-export([create_user_sync/2,get_balance_sync/2,deposit_sync/2,withdraw_sync/2,send_sync/2]).
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

%%
% Async API
%%
create_user_async(Pid,SendTo,User)->
    gen_server:cast(Pid,{SendTo,{create_user,User}}).
get_balance_async(Pid,SendTo,{User,Currency})->
    gen_server:cast(Pid,{SendTo,{Currency,{get_balance,User}}}).
deposit_async(Pid,SendTo,{User,Amount,Currency})->
    gen_server:cast(Pid,{SendTo{Currency,{deposit,{User,Amount}}}}).
withdraw_async(Pid,SendTo,{User,Amount,Currency})->
    gen_server:cast(Pid,{SendTo,{Currency,{withdraw,{User,Amount}}}}).
send_async(Pid,SendTo,{From_User,To_User,Amount,Currency})->
    gen_server:cast(Pid,{SendTo,{Currency,{send,{From_User,To_User,Amount}}}}).


%%
% Sync API
%%
send_result_sync(Pid,SendTo,Message)->
    gen_server:reply(Pid,Message).
create_user_sync(Pid,User)->
    gen_server:call(Pid,{create_user,User}).
get_balance_sync(Pid,{User,Currency})->
    gen_server:call(Pid,{Currency,{get_balance,User}}).
deposit_sync(Pid,{User,Amount,Currency})->
    gen_server:call(Pid,{Currency,{deposit,{User,Amount}}}).
withdraw_sync(Pid,{User,Amount,Currency})->
    gen_server:call(Pid,{Currency,{withdraw,{User,Amount}}}).
send_sync(Pid,{From_User,To_User,Amount,Currency})->
    gen_server:call(Pid,{Currency,{send,{From_User,To_User,Amount}}}).
send_result_sync(Pid,Message)->
    gen_server:reply(Pid,Message).

%%%%%%%%%%%%%%%%%%%% Handlers %%%%%%%%%%%%
%%%
%%%
%%%
%%%
-spec handle_cast({SendTo::pid(),{create_user,UserId::string()}},State::state())->StateResult::term(). 
handle_cast({SendTo,{create_user,UserId}}, State)->
    ok=do_create_user(UserId, State),
    gen_server:reply(SendTo,ok),
    {stop,normal,State};

-spec handle_cast({SendTo::pid(),{Currency::string(),Request::term()}},State::state())->StateResult::term().
handle_cast({SendTo,{Currency,Request}})->
    {ok,Coefficient}=can_get_coefficient(Currency, State),
     do_call(Coefficient,Request,From,State).

handle_cast(_Request,State)->{noreply,State}.

handle_call({create_user,UserId}, _From, State)->
    ok=do_create_user(UserId, State),
    {stop,normal,ok,State};

handle_call({Currency,Request},From,State)->
    {ok,Coefficient}=can_get_coefficient(Currency, State),
     do_call(Coefficient,Request,From,State).



do_call(Coefficient,{get_balance,UserId},_From,State)->
    {ok,User}=get_user(UserId,State),
    Reply=case ex_banking_account:get_balance(User#user.pid) of
            {ok,BalanceInBaseCurrency}->{ok,BalanceInBaseCurrency/Coefficient};
             Error -> Error
          end,
    {reply,Reply,State};
        

do_call(Coefficient,{deposit,{UserId,Amount}},_From,State)->
    {ok,User}=get_user(UserId,State),
    Reply=case ex_banking_account:deposit(User#user.pid,Amount*Coefficient) of
            {ok,BalanceInBaseCurrency}->{ok,BalanceInBaseCurrency/Coefficient};
             Error -> Error
          end,
    {reply,Reply,State};

    
do_call(Coefficient,{withdraw,{UserId,Amount}},_From,State)->
    {ok,User}=get_user(UserId,State),
    Reply=case ex_banking_account:withdraw(User#user.pid,Amount*Coefficient) of
            {ok,BalanceInBaseCurrency}->{ok,BalanceInBaseCurrency/Coefficient};
             Error -> Error
          end,
    {reply,Reply,State};

do_call(Coefficient,{send,{From_User_Id,To_User_Id,Amount}},_From,State)->
    {ok,From_User,To_User}=get_sender_and_receiver(From_User_Id, To_User_Id, State),
    {ok,From_User_Balance,To_User_Balance}=handle_send(From_User#user.pid, To_User#user.pid, Amount*Coefficient, State),
    {stop,normal,{ok,From_User_Balance/Coefficient,To_User_Balance/Coefficient},State}.



can_get_coefficient(Currency,State)->
    case ex_banking_currency_server:get_coefficient(Currency) of
        {ok,Coefficient} -> {ok, Coefficient};
        _ -> throw({stop,normal,{error,wrong_arguments,Currency},State})
    end.
get_user(UserId,State)->
    case ex_banking_account_map:get_user(UserId) of
        {ok,User}-> {ok,User};
         user_does_not_exist  -> throw({stop,normal,user_does_not_exist,State})
    end. 
get_sender_and_receiver(From_Uid,To_Uid,State)->
    From_User=ex_banking_account_map:get_user(From_Uid),
    To_User=ex_banking_account_map:get_user(To_Uid),
    handle_sender_receiver_identity(From_User,To_User,State).


handle_sender_receiver_identity(user_does_not_exist,_,State)->throw({stop,normal,sender_does_not_exist,State});
handle_sender_receiver_identity(_,user_does_not_exist,State)->throw({stop,normal,receiver_does_not_exist,State});
handle_sender_receiver_identity({ok,From},{ok,To},_)->{ok,From,To}.

handle_send(From_User_Pid,To_User_Pid,Amount,State)->
    WithdrawResult=ex_banking_account:withdraw(From_User_Pid, Amount),
    handle_withdraw_result(WithdrawResult,To_User_Pid,Amount,State).

handle_withdraw_result(too_many_requests_to_user,_,_,State)->throw({stop,normal,too_many_requests_to_sender,State});
handle_withdraw_result(not_enough_money,_,_,State)->throw({stop,normal,not_enough_money,State});

handle_withdraw_result({ok,From_NewBalanceInBaseCurrency},To_Pid,Amount,State)->
    case ex_banking_account:deposit(To_Pid, Amount) of
        {ok,To_NewBalanceInBaseCurrency} -> {ok,From_NewBalanceInBaseCurrency,To_NewBalanceInBaseCurrency};
        too_many_requests_to_user -> throw({stop,normal,too_many_requests_to_receiver,State})
    end.

do_create_user(UserId,State)->
    {ok,Pid}=ex_banking_account_sup:create_account(UserId),
    Ref=erlang:monitor(process, Pid),
    case ex_banking_account_map:create_user(UserId, Pid, Ref) of
       account_already_exists-> throw({stop,normal,user_already_exists,State});
       ok ->ok
    end.