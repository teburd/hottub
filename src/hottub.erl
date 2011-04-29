%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Shell and Testing API. For real usage you should use ht_sup.

-module(hottub).

%% api
-export([start/5, stop/1, with_worker/2, call/2, cast/2]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a hot tub worker pool.
-spec start(PoolName::atom(), Limit::pos_integer(), M::module(), F::function(), A::list()) -> {ok, pid()}.
start(PoolName, Limit, Module, Function, Args) ->
    ht_sup:start_link(PoolName, Limit, Module, Function, Args).

%% @doc Stop a hot tub worker pool.
-spec stop(PoolName::atom()) -> ok.
stop(PoolName) ->
    ht_sup:stop(PoolName).

%% @doc Perform a function with a worker process.
-spec with_worker(PoolName::atom(), Fun::fun()) -> any().
with_worker(PoolName, Fun) ->
    ht_pool:with_worker(PoolName, Fun).

%% @doc Perform a gen_server:call with a worker process.
-spec call(PoolName::atom(), Args::any()) -> Result::any().
call(PoolName, Args) ->
    ht_pool:call(PoolName, Args).

%% @doc Perform a gen_server:call with a worker process.
-spec cast(PoolName::atom(), Args::any()) -> Result::any().
cast(PoolName, Args) ->
    ht_pool:cast(PoolName, Args).

