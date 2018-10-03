%%-------------------------------------------------------------------
%% @doc
%% This file is part of eredis + poolboy.
%% eredis_poolboy top level supervisor.
%% @end
%%-------------------------------------------------------------------

-module(eredis_poolboy_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  add_pool/3
]).

%% Supervisor callbacks
-export([
  init/1
]).

-include_lib("eredis/include/eredis.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_pool(PoolName, PoolSize, Option) -> supervisor:startchild_ret() when
  PoolName :: atom(),
  PoolSize :: proplists:proplist(),
  Option   :: option().
add_pool(PoolName, PoolSize, Option) ->
  PoolArgs = [{name, {local, PoolName}},
    {worker_module, eredis}] ++ PoolSize,
  PoolSpec = poolboy:child_spec(PoolName, PoolArgs, Option),
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
