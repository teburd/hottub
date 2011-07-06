%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hottub API

-module(hottub).

%% api
-export([start/5, execute/2, call/2, cast/2]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a hot tub worker pool.
-spec start(PoolName::atom(), Limit::pos_integer(), M::module(), F::function(), A::list()) -> {ok, pid()}.
start(PoolName, Limit, Module, Function, Args) ->
    ht_sup:start_link(PoolName, Limit, Module, Function, Args).

%% @doc Perform a gen_server:call with a worker process.
-spec call(PoolName::atom(), Args::any()) -> Result::any().
call(PoolName, Args) ->
    execute(PoolName,
        fun(Worker) ->
            gen_server:call(Worker, Args)
        end).

%% @doc Perform a gen_server:call with a worker process.
-spec cast(PoolName::atom(), Args::any()) -> Result::any().
cast(PoolName, Args) ->
    execute(PoolName,
        fun(Worker) ->
            gen_server:cast(Worker, Args)
        end).

%% @doc Execute a function using a worker.
-spec execute(PoolName::atom(), Function::fun((Worker::pid()) -> Result::any())) -> Result::any().
execute(PoolName, Function) ->
    Worker = ht_pool:checkout_worker(PoolName),
    try
        Function(Worker)
    after
        ht_pool:checkin_worker(PoolName, Worker)
    end.
