Hot Tub
=======

Hot Tub is a permanent erlang worker pool.

Goals
-----

* Keeps some number of worker processes alive at all times.
* Be very fast on average under load.

Primarily I have used this for database workers though other uses are clearly
possible.


Implementations
---------------

There are 3 pool manager implementations to choose from with different performance
characteristics. One that uses a single ets table and a counter, one that uses two
ets tables, one that uses a queue and a set held by a process.

There is a benchmark as part of the test suite which can be run against the 3
branches to give you an idea of which one works will under your circumstances.


Example Usage
-------------

demo_worker.erl

``` erlang
-module(demo_worker).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, undefined}

handle_call({add, A, B}, _From, State) ->
    {reply, A+B, State}.

handle_cast({print, Message}, State) ->
    io:format("~p~n", [Message]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

erl shell

``` erlang
hottub:start(demo, 5, demo_worker, start_link, []).
hottub:call(demo, {add, 5, 5}).
hottub:cast(demo, {print, "what up from a pool"}).
hottub:with_worker(demo, 
    fun(Worker) -> 
        io:format("causing a worker to crash~n"),
        gen_server:call(Worker, {hocus_pocus}) end).
hottub:stop(demo).
```
