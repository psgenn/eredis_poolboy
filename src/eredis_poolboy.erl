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
  q/3,
  qp/2,
  qp/3,
  q_noreply/2
]).

-include_lib("eredis/include/eredis.hrl").

%% @doc
%% Add new pool connections.
%% @end
-spec add_pool(PoolName, PoolSize, Option) -> {ok, pid} | {error, Reason} when
  PoolName :: atom(),
  PoolSize :: proplists:proplist(),
  Option   :: option(),
  Reason   :: {already_started, pid()}.
add_pool(PoolName, PoolSize, Option) ->
  eredis_poolboy_sup:add_pool(PoolName, PoolSize, Option).

%% @doc
%% Executes the given command. The command must be a valid Redis command.
%% @end
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

%% @doc
%% Executes the given pipeline (list of commands) in the
%% specified connection. The commands must be valid Redis commands.
%% @end
-spec qp(PoolName :: atom(), Command :: [any()]) ->
  {ok, return_value()} | {error, Reason :: binary() | no_connection}.
qp(PoolName, Command) ->
  Func = fun(Worker) ->
    eredis:qp(Worker, Command)
  end,
  poolboy:transaction(PoolName, Func).

-spec qp(PoolName, Command, Timeout) ->
  {ok, return_value()} | {error, Reason :: binary() | no_connection} when
  PoolName :: atom(),
  Command  :: [any()],
  Timeout  :: non_neg_integer() | infinity.
qp(PoolName, Command, Timeout) ->
  Func = fun(Worker) ->
    eredis:qp(Worker, Command, Timeout)
  end,
  poolboy:transaction(PoolName, Func).

%% @doc
%% Executes the command but does not wait for a response and ignores any errors.
%% @end
-spec q_noreply(PoolName :: atom(), Command :: [any()]) -> ok.
q_noreply(PoolName, Command) ->
  Func = fun(Worker) ->
    eredis:q_noreply(Worker, Command)
  end,
  poolboy:transaction(PoolName, Func).
