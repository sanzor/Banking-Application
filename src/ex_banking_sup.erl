%%%-------------------------------------------------------------------
%% @doc ex_banking top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ex_banking_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
    #{
        id=>ex_banking_currency_server,
        start=>{ex_banking_currency_server,start_link,[]},
        restart=>permanent,
        shutdown=>brutal_kill,
        type=>worker,
        mod=>[ex_banking_currency_server]

    },
    #{
        id=>ex_banking_account_map,
        start=>{ex_banking_account_map,start_link,[]},
        restart=>permanent,
        shutdown=>5000,
        type=>worker,
        mod=>[ex_banking_account_map]
    },
    #{
        id=>ex_banking_account_sup,
        start=>{ex_banking_account_sup,start_link,[]},
        restart=>permanent,
        shutdown=>5000,
        type=>supervisor,
        mod=>[ex_banking_account_worker_sup]
    },
    #{
        id=>ex_banking_client_sup,
        start=>{ex_banking_client_sup,start_link,[]},
        restart=>permanent,
        shutdown=>brutal_kill,
        type=>supervisor,
        mod=>[ex_banking_client_worker_sup]
    }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
