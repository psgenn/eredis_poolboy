%%%-------------------------------------------------------------------
%% @doc eredis_poolboy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eredis_poolboy_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_pool/3
]).

%% Supervisor callbacks
-export([
  init/1
]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_pool(PoolName, PoolSize, Args) ->
  PoolArgs = [{name, {local, PoolName}},
    {worker_module, eredis}] ++ PoolSize,
  PoolSpec = poolboy:child_spec(PoolName, PoolArgs, Args),
  supervisor:start_child(?MODULE, PoolSpec).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  SupFlags = #{
    strategy => one_for_one
  },
  {ok, {SupFlags, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
