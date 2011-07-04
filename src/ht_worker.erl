%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc Hot Tub Worker. 

-module(ht_worker).

-export([start_worker/2]).

%% @doc The pool manager needs to know when a worker is alive.
%% It turns out the simplest way is to simply wrap the function the
%% supervisor calls to start a process with another that does some additional
%% work.
%% @end
-spec start_worker(atom(), {module(), function(), list()})
    -> {ok, Pid::pid()}.
start_worker(PoolName, {Module, Function, Arguments}) ->
    {ok, Pid} = erlang:apply(Module, Function, Arguments),
    ht_pool:add_worker(PoolName, Pid),
    {ok, Pid}.
