-module(ex_banking_business).
-export([create_user/1,get_balance/1,deposit/2,withdraw/2,send/3]).
-export([handle_get_balance/1]).
-record(user,{
    ref,
    pid
}).



create_user(User)->
    
    case ex_banking_account_map:get_user(User) of
        {ok,_}->user_already_exists;
         user_does_not_exist->{ok,Pid}=ex_banking_account_sup:create_account(User),
                             Ref=erlang:monitor(process, Pid),
                             ok=ex_banking_account_map:create_user(User, Ref, Pid),
                             ok
    end.                     

get_balance(Uid)->
    UserFoundResult=ex_banking_account_map:get_user(Uid),
    handle_get_balance(UserFoundResult).
    
  
handle_get_balance({ok,User})->
    
    try ex_banking_account:get_balance(User#user.pid) of
        Result->Result
    catch
       Class:Reason:StackTrace -> io:format("~p~n~n~p~n~n~p",[Class,Reason,StackTrace]),
                                  {ok,555}
    end.

   



deposit(Uid,Amount) ->
    case ex_banking_account_map:get_user(Uid) of
        {ok,User}->{ok,NewBalance}=ex_banking_account:deposit(User#user.pid,Amount),
                   {ok,NewBalance};
        Err->io:format("Failure!!!~p",[Err]),
              Err
    end.
   

withdraw(Uid,Amount)->
    case ex_banking_account_map:get_user(Uid) of
        {ok,User}->ex_banking_account:withdraw(User#user.pid,Amount);
         Err->Err
    end.
 

send(From_Uid,To_Uid,Amount)->
    case begin F_U=ex_banking_account:get_user(From_Uid),
                     T_U=ex_banking_account:get_user(To_Uid),
                     {F_U,T_U} 
                end 
           of 
            {user_does_not_exist,_} -> sender_does_not_exist;
            {_,user_does_not_exist} -> receiver_does_not_exist;
            {{ok,From_User},{ok,To_User}}-> handle_send(From_User#user.pid, To_User#user.pid, Amount)
    end.

handle_send(From_User_Pid,To_User_Pid,Amount)->
    WithdrawResult=ex_banking_account:withdraw(From_User_Pid, Amount),
    handle_withdraw_result(WithdrawResult,To_User_Pid,Amount).

handle_withdraw_result(not_enough_money,_,_)->not_enough_money;
handle_withdraw_result({ok,FromNewBalance},To_Pid,Amount)->
    case ex_banking_account:deposit(To_Pid, Amount) of
        {ok,ToNewBalance} -> {ok,FromNewBalance,ToNewBalance};
         Err ->Err
    end.
