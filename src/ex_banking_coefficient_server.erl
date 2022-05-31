-module(ex_banking_coefficient_server).
-behaviour(gen_server).
-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2,handle_continue/2]).
-export([get_coefficient/1,add_coefficient/2,remove_coefficient/1,update_coefficient/2]).
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
    gen_server:call(?NAME, {get_coefficient,Currency}).



-spec add_coefficient(Currency::string(), Coefficient::number())->{ok,{added,Currency}} | 
                                                      currency_already_exists |{error , wrong_arguments}
                                                      when Currency :: list() | atom() .
add_coefficient(Currency,Coefficient) when not is_number(Coefficient) ;not Coefficient>0; not (is_list(Currency) or is_atom(Currency))  ->
    {error,invalid_arguments};

add_coefficient(Currency,Coefficient)->
    gen_server:call(?NAME, {add_currency,Currency,Coefficient}).



-spec remove_coefficient(Currency)->{ok,{removed,Currency}}  | {error , wrong_arguments}
                                    when Currency:: list() | atom().
remove_coefficient(Currency) when not is_atom(Currency) ; not is_list(Currency) ->
    {error,invalid_arguments};

remove_coefficient(Currency)->
    gen_server:call(?NAME, {remove_currency,Currency}).



-spec update_coefficient(Currency, Coefficient::number())->{ok,{updated,Currency}} |
                                                         currency_does_not_exist | {error , wrong_arguments}
                                                         when Currency:: list() | atom().
update_coefficient(Currency,Coefficient) when not is_number(Coefficient) ; not Coefficient>0 ; not is_list(Currency) , not is_atom(Currency) ->
    {error,wrong_arguments};

update_coefficient(Currency,Coefficient)->
    gen_server:call(?NAME, {update_currency,Currency,Coefficient}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         Handlers    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_continue(hidrate_store,State=#state{conn=Conn})->
    {ok,Value}=application:get_env(ex_banking, base_currency),
    eredis:q(Conn,["hset","currencies"|[Value,1]]),
    {noreply,State}.
handle_cast(stop,State)->
    {stop,State}.
% handle_call(Request,From,State)->
%     try
%         {reply,{ok,handle_call_impl(Request, From, State)},State}
%     catch
%         throw:currency_does_exist->{reply,{error,{currency_does_not_exist,[]}}};
%         error:Reason->{reply,{error,Reason}}
%     end;
handle_call({get_coefficient,Currency},_From,State)->
        {ok,Value}=get_coefficient(Currency,State#state.conn),
        {reply,Value,State};
     
        
        
handle_call({add_currency,Currency,Coefficient},_From,State)->
    eredis:q(State#state.conn,["hset","currencies"|[Currency,Coefficient]]),
    {reply,{ok,{added,Currency}},State};
    

handle_call({remove_currency,Currency},_From,State)->
    eredis:q(State#state.conn,["hdel","currencies",Currency]),
    {reply,{ok,{removed,Currency}},State};


handle_call({update_currency,Currency,Coefficient},_From,State)->
    try
         {ok,_}=get_coefficient(Currency,State#state.conn),
         eredis:q(State#state.conn,["hset","currencies",[Currency,Coefficient]]),
         {reply,{ok,{updated,Currency}},State}
    catch
        error:does_not_exist->{reply,currency_does_not_exist,State}
    end.

%%
% Internal functions
%%
handle_call_impl(Request,From,State)->undefined.

-spec get_coefficient(Currency::string(),Conn::port())->{ok,number()}|does_not_exist.
get_coefficient(Currency,Conn)->
    Reply=case eredis:q(Conn,["hget","currencies",[Currency]]) of
            {ok,<<Value/binary>>} -> {ok,to_number(Value)};
            _ -> erlang:raise(error,currency_does_not_exist,[])
           end,
    Reply.
to_number(Binary) when is_binary(Binary)->
    case contains_dot(Binary) of
        true -> binary_to_float(Binary);
        false-> binary_to_integer(Binary)
    end.

contains_dot(<<>>)->false;
contains_dot(<<".",_>>)->true;
contains_dot(<<_,Rest/binary>>)->contains_dot(Rest).