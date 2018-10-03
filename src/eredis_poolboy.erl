%%-------------------------------------------------------------------
%% @doc
%% This file is part of eredis + poolboy.
%% @end
%%-------------------------------------------------------------------

-module(eredis_poolboy).

%% API
-export([
  add_pool/3,
  q/2,
  q/3
]).

-include_lib("eredis/include/eredis.hrl").

-spec add_pool(PoolName, PoolSize, Option) -> {ok, pid} | {error, Reason} when
  PoolName :: atom(),
  PoolSize :: proplists:proplist(),
  Option   :: option(),
  Reason   :: {already_started, pid()}.
add_pool(PoolName, PoolSize, Option) ->
  eredis_poolboy_sup:add_pool(PoolName, PoolSize, Option).

-spec q(PoolName :: atom(), Command :: [any()]) ->
  {ok, return_value()} | {error, Reason :: binary() | no_connection}.
q(PoolName, Command) ->
  Func = fun(Worker) ->
    eredis:q(Worker, Command)
  end,
  poolboy:transaction(PoolName, Func).

-spec q(PoolName, Command, Timeout) ->
  {ok, return_value()} | {error, Reason :: binary() | no_connection} when
  PoolName :: atom(),
  Command  :: [any()],
  Timeout  :: non_neg_integer() | infinity.
q(PoolName, Command, Timeout) ->
  Func = fun(Worker) ->
    eredis:q(Worker, Command, Timeout)
         end,
  poolboy:transaction(PoolName, Func).
