-module(ex_banking_api).


-export([create_user/1,get_balance/2,deposit/3,withdraw/3,send/4]).


create_user(User)->