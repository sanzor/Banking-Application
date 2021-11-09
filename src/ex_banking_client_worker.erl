-module(ex_banking_server).
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
    case ex_banking_currency_server:get_currency(Currency) of
        {ok,Coefficient}->case gen_server:call(Pid,{get_balance,User}) of  
                            {ok,NewBalance}->{ok,NewBalance/Coefficient};
                             Err->Err
                          end;
        Error -> Error
    end.
deposit(Pid,{User,Amount,Currency})->
    case ex_banking_currency_server:get_currency(Currency) of
        {ok,Coefficient}->case gen_server:call(Pid,{deposit,User,Amount*Coefficient,Currency}) of  
                            {ok,NewBalance}->{ok,NewBalance/Coefficient};
                             Err->Err
                          end;
        Error -> Error
    end.
    
withdraw(Pid,{User,Amount,Currency})->
    case ex_banking_currency_server:get_currency(Currency) of
        {ok,Coefficient}->case gen_server:call(Pid,{withdraw,User,Amount*Coefficient,Currency}) of  
                            {ok,NewBalance}->{ok,NewBalance/Coefficient};
                             Err->Err
                          end;
        Error -> Error
    end.
send(Pid,{From_User,To_User,Amount,Currency})->
    case ex_banking_currency_server:get_currency(Currency) of
        {ok,Coefficient}->case gen_server:call(Pid,{send,From_User,To_User,Amount*Coefficient}) of  
                            {ok,From_User_Balance,To_User_Balance}->{ok,From_User_Balance/Coefficient,To_User_Balance/Coefficient};
                             Err->Err
                          end;
        Error -> Error
    end.


send_result(Pid,Message)->
    gen_server:reply(Pid,Message).

%%%%%%%%%%%%%%%%%%%% Handlers %%%%%%%%%%%%
handle_cast(_Request,State)->{noreply,State}.
handle_call(Request,_From,State)->
    {ok,Response}=ex_banking_enqueuer:process_message(Request),
    {reply,Response,State}.