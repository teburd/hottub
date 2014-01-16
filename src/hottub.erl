%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hottub API

-module(hottub).

%% api
-export([start_link/5, stop/1, execute/2, call/2, call/3, cast/2]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked hottub worker pool supervisor.
-spec start_link(atom(), pos_integer(), atom(), atom(), list(any())) ->
    ignore | {error, any()} | {ok, pid()}.
start_link(PoolName, Limit, Module, Function, Args) ->
    ht_sup:start_link(PoolName, Limit, Module, Function, Args).

%% @doc Stop a hottub worker pool supervisor
-spec stop(atom()) -> ok.
stop(PoolName) ->
    ht_sup:stop(PoolName).

%% @doc Perform a gen_server:call with a worker process.
-spec call(atom(), any()) -> any().
call(PoolName, Args) ->
    execute(PoolName,
        fun(Worker) ->
            gen_server:call(Worker, Args)
        end).

%% @doc Perform a gen_server:call with a worker process.
-spec call(atom(), any(), timeout()) -> any().
call(PoolName, Args, Timeout) ->
    execute(PoolName,
        fun(Worker) ->
            gen_server:call(Worker, Args)
        end,
        Timeout).

%% @doc Perform a gen_server:cast with a worker process.
-spec cast(atom(), any()) -> any().
cast(PoolName, Args) ->
    execute(PoolName,
        fun(Worker) ->
            gen_server:cast(Worker, Args)
        end).

%% @doc Execute a function using a worker.
-spec execute(atom(), fun((pid()) -> any())) -> any().
execute(PoolName, Function) ->
    Worker = ht_pool:checkout_worker(PoolName),
    try
        Function(Worker)
    after
        ht_pool:checkin_worker(PoolName, Worker)
    end.
