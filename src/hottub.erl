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
    gen_server:call(worker(PoolName), Args).

%% @doc Perform a gen_server:call with a worker process.
-spec cast(PoolName::atom(), Args::any()) -> Result::any().
cast(PoolName, Args) ->
    gen_server:cast(worker(PoolName), Args).


%% ----------------------------------------------------------------------------
%% private api
%% ----------------------------------------------------------------------------

%% @doc Mark a worker as used.
-spec mark_worker(PoolName::atom(), Pid::pid()) -> ok.
mark_worker(PoolName, Pid) ->
    ets:update_counter(PoolName, Pid, {3, 1}),
    ok.

%% @doc Mark a worker as being unused.
-spec unmark_worker(PoolName::atom(), Pid::pid()) -> ok.
unmark_worker(PoolName, Pid) ->
    ets:update_counter(PoolName, Pid, {3, -1}),
    ok.

%% @doc Checkout a worker.
-spec checkout_worker(PoolName::atom()) -> Worker::pid() | undefined.
checkout_worker(PoolName) ->
    Worker = ets:foldl(
        fun 
            ({Pid, _, N}, undefined) -> {Pid, N};
            ({Pid, _, N}, {_OPid, A}) when A > N -> {Pid, N};
            ({_, _, _}, {OPid, A}) -> {OPid, A}
        end, undefined, PoolName),
    case Worker of
        undefined ->
           undefined;
        {Pid, N} ->
            mark_worker(PoolName, Pid),
            Pid
    end.

%% @doc Checkin a worker.
-spec checkin_worker(PoolName::atom(), Worker::pid()) -> ok.
checkin_worker(PoolName, Worker) ->
    unmark_worker(PoolName, Worker).


