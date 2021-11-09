-module(ex_banking_currency_server).
-behaviour(gen_server).
-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([get_currency/1,add_currency/2,remove_currency/1,update_currency/2]).

-define(NAME,?MODULE).
-record(state,{
    currencies
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         API    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [],[]).

init(Args)->
    {ok,#state{currencies=dict:new()}}.


-spec get_currency(Currency:: list() | atom())->{ok,{Currency::list() | atom(), Coefficient::number()}} | currency_not_found |  {error , invalid_arguments}.
get_currency(Currency) when not is_list(Currency) , not is_atom(Currency)->
    {error,invalid_arguments};
get_currency(Currency)->
    gen_server:call(?NAME, {get_currency,Currency}).



-spec add_currency(Currency:: list() | atom() , Coefficient::number())->{ok,{added,Currency::list() | atom()}} | currency_already_exists |{error , invalid_arguments}.
add_currency(Currency,Coefficient) when not is_number(Coefficient) ;not Coefficient>0; not is_list(Currency) ->
    {error,invalid_arguments};

add_currency(Currency,Coefficient)->
    gen_server:call(?NAME, {add_currency,Currency,Coefficient}).



-spec remove_currency(Currency:: list() | atom())->{ok,{removed,Currency::list() | atom()}}  | {error , invalid_arguments}.
remove_currency(Currency) when not is_atom(Currency) ; not is_list(Currency) ->
    {error,invalid_arguments};

remove_currency(Currency)->
    gen_server:call(?NAME, {remove_currency,Currency}).



-spec update_currency(Currency:: list() | atom() , Coefficient::number())->{ok,{updated,Currency::list() | atom()}} | currency_does_not_exist |{error , invalid_arguments}.
update_currency(Currency,Coefficient) when not is_number(Coefficient) ; not Coefficient>0 ; not is_list(Currency) , not is_atom(Currency) ->
    {error,invalid_arguments};

update_currency(Currency,Coefficient)->
    gen_server:call(?NAME, {update_currency,Currency,Coefficient}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         Handlers    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(stop,State)->
    {stop,State}.
handle_call({get_currency,Currency},_From,State)->
    Reply=case dict:find(Currency, State#state.currencies) of
            error ->currency_does_not_exist;
            {ok,Value}->{ok,Value}
    end,
    {reply,Reply,State};
handle_call({add_currency,Currency,Coefficient},_From,State)->
    Reply=case dict:find(Currency, State#state.currencies) of
            {ok,_Value}->{reply,currency_already_exists,State};
            error -> NewDict=dict:store(Currency,Coefficient,State#state.currencies),
                     {reply,{ok,{added,Currency}},State#state{currencies=NewDict}}
            
    end,
    {reply,Reply,State};

handle_call({remove_currency,Currency},_From,State)->
    NewDict=dict:erase(Currency, State),
    {reply,{ok,{removed,Currency}},State#state{currencies=NewDict}};


handle_call({update_currency,Currency,Coefficient},_From,State)->
   case dict:find(Currency, State#state.currencies) of
                error ->{reply,currency_does_not_exist,State};
                {ok,_}-> NewDict=dict:store(Currency, Coefficient,State#state.currencies),
                         {reply,{ok,{updated,Currency}},State#state{currencies=NewDict}}
    end.
   