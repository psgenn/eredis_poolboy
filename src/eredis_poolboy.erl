%%%-------------------------------------------------------------------
%%% @author psgenn@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 02. Окт. 2018 21:01
%%%-------------------------------------------------------------------
-module(eredis_poolboy).
-author("psgenn@gmail.com").

%% API
-export([
  start_pool/3,
  q/2,
  q/3
]).

-include_lib("eredis/include/eredis.hrl").

-spec start_pool(PoolName, PoolSize, Option) -> {ok, pid} | {error, Reason} when
  PoolName :: atom(),
  PoolSize :: proplists:proplist(),
  Option   :: option(),
  Reason   :: {already_started, pid()}.
start_pool(PoolName, PoolSize, Option) ->
  eredis_poolboy_sup:start_pool(PoolName, PoolSize, Option).

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
