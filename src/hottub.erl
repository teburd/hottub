%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Shell and Testing API. For real usage you should use ht_sup.

-module(hottub).

%% api
-export([start/5, stop/1, worker/1, call/2, cast/2]).


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

%% @doc Get a worker Pid.
-spec worker(PoolName::atom()) -> Worker::pid() | undefined.
worker(PoolName) ->
    Worker = ets:foldl(
        fun 
            ({Pid, _, N}, {_OPid, A}) when N > A -> {Pid, N};
            ({_, _, _}, {OPid, A}) -> {OPid, A}
        end, 0, PoolName),
    case Worker of
        {Pid, _N} ->
            ht_pool:using_worker(PoolName, Pid),
            Pid;
        0 ->
            undefined
    end.

%% @doc Perform a gen_server:call with a worker process.
-spec call(PoolName::atom(), Args::any()) -> Result::any().
call(PoolName, Args) ->
    gen_server:call(worker(PoolName), Args).

%% @doc Perform a gen_server:call with a worker process.
-spec cast(PoolName::atom(), Args::any()) -> Result::any().
cast(PoolName, Args) ->
    gen_server:cast(worker(PoolName), Args).

