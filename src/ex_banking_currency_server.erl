-module(ex_banking_currency_server).
-behaviour(gen_server).
-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2,handle_continue/2]).
-export([get_coefficient/1,add_currency/2,remove_currency/1,update_currency/2]).
-define(NAME,?MODULE).
-record(state,{
    conn
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         API    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_link()->
    gen_server:start_link({local,?NAME}, ?MODULE, [],[]).

init(_Args)->
        {ok,Connection}=eredis:start_link(),
        {ok,#state{conn=Connection},{continue,hidrate_store}}.

-spec get_coefficient(Currency)->{ok,{Currency, Coefficient::number()}} | currency_not_found |  {error , wrong_arguments} 
                                    when Currency:: list() | atom().
get_coefficient(Currency) when not is_list(Currency) , not is_atom(Currency)->
    {error,invalid_arguments};
get_coefficient(Currency)->
    gen_server:call(?NAME, {get_currency,Currency}).



-spec add_currency(Currency, Coefficient::number())->{ok,{added,Currency}} | 
                                                      currency_already_exists |{error , wrong_arguments}
                                                      when Currency :: list() | atom() .
add_currency(Currency,Coefficient) when not is_number(Coefficient) ;not Coefficient>0; not (is_list(Currency) or is_atom(Currency))  ->
    {error,invalid_arguments};

add_currency(Currency,Coefficient)->
    gen_server:call(?NAME, {add_currency,Currency,Coefficient}).



-spec remove_currency(Currency)->{ok,{removed,Currency}}  | {error , wrong_arguments}
                                    when Currency:: list() | atom().
remove_currency(Currency) when not is_atom(Currency) ; not is_list(Currency) ->
    {error,invalid_arguments};

remove_currency(Currency)->
    gen_server:call(?NAME, {remove_currency,Currency}).



-spec update_currency(Currency, Coefficient::number())->{ok,{updated,Currency}} |
                                                         currency_does_not_exist | {error , wrong_arguments}
                                                         when Currency:: list() | atom().
update_currency(Currency,Coefficient) when not is_number(Coefficient) ; not Coefficient>0 ; not is_list(Currency) , not is_atom(Currency) ->
    {error,wrong_arguments};

update_currency(Currency,Coefficient)->
    gen_server:call(?NAME, {update_currency,Currency,Coefficient}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         Handlers    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_continue(hidrate_store,State=#state{conn=Conn})->
    {ok,Value}=application:get_env(ex_banking, base_currency),
    eredis:q(Conn,["HMSET","currencies"|[Value,1]]),
    {noreply,State}.
handle_cast(stop,State)->
    {stop,State}.
handle_call({get_currency,Currency},_From,State)->
    Reply=case eredis:q(State#state.conn,"hget","currencies","Currency") of
            {ok,<<Value>>} -> binary_to_float(Value)
            error ->currency_does_not_exist;
            {ok,Value}->{ok,Value}
    end,
    {reply,Reply,State};
handle_call({add_currency,Currency,Coefficient},_From,State)->
    case dict:find(Currency, State#state.currencies) of
            {ok,_Value}->{reply,currency_already_exists,State};
            error -> NewDict=dict:store(Currency,Coefficient,State#state.currencies),
                     {reply,{ok,{added,Currency}},State#state{currencies=NewDict}}
            
    end;
    

handle_call({remove_currency,Currency},_From,State)->
    NewDict=dict:erase(Currency, State),
    {reply,{ok,{removed,Currency}},State#state{currencies=NewDict}};


handle_call({update_currency,Currency,Coefficient},_From,State)->
   case dict:find(Currency, State#state.currencies) of
                error ->{reply,currency_does_not_exist,State};
                {ok,_}-> NewDict=dict:store(Currency, Coefficient,State#state.currencies),
                         {reply,{ok,{updated,Currency}},State#state{currencies=NewDict}}
    end.
   