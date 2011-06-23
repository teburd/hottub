%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc Hot Tub Worker. 

-module(ht_worker).

%% @doc The pool manager needs to know when a worker is alive.
%% It turns out the simplest way is to simply wrap the function the
%% supervisor calls to start a process with another that does some additional
%% work.
%% @end
-spec start_worker(atom(), pos_integer(), {module(), function(), list()})
    -> {ok, Pid::pid()}.
start_worker(PoolName, Worker, {Module, Function, Arguments}) ->
    {ok, Pid} = erlang:apply(Module, Function, Arguments),
    ht_pool:worker_started(PoolName, Worker, Pid),
    {ok, Pid}.
