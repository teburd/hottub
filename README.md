Hot Tub
=======

HotTub is a simple, fast, permanent erlang worker pool.

Goals
-----

* Keeps some number of worker processes alive at all times.
* Add as little latency as possible to using a worker process under all
  circumstances.
* Use only closures as the public interface to avoid missing worker processes
  due to errors or missing return to pool type calls. Closures ensure errors
  and returns are handled automatically for the callers.

Primarily I have used this for database workers though other uses are clearly
possible. When using this for connection pools the connections themselves should
be lazy. Meaning that the process should not fail due to disconnects.

Non-Goals
---------

* Handle extraordinary large numbers of requests. In those cases several pools
  could be used in a rand() mod n_pools style distribution if desired. Or other
  libraries dealing with such situations might be a better choice.
* Deal with the connection cycle of network client libraries, that should be
  done as something like a gen_fsm as a worker instead or a lazy retry loop
  in gen_server:init.

Implementation
---------------

HotTub uses a gen_server process to manage a queue of available workers and
of requests for workers. When a worker is available it is dequeued and
given away. If there are no workers then the request for a worker is queued.
As soon as a worker is returned if a request is queued the worker is given away.

A best effort at avoiding unnecessary work has been done however things could
still probably be better.

There is a benchmark as part of the test suite which can be run to give you an
idea of the overhead of hottubs worker pool management routines.

Several implementations were tried prior to settling on this one, including
an ETS table with N workers using rand() mod n_workers selection, trying 
to find an available worker from there linearly. This was found to perform
faster in some cases, but very poorly in the worst case. See the git history
for more information.


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
hottub:start_link(demo, 5, demo_worker, start_link, []).
hottub:call(demo, {add, 5, 5}).
hottub:call(demo, {add, 5, 5}, 1000). %% call with timeout
hottub:cast(demo, {print, "what up from a pool"}).
hottub:execute(demo, 
    fun(Worker) -> 
        io:format("causing a worker to crash~n"),
        gen_server:call(Worker, {hocus_pocus}) end).
hottub:stop(demo).
```
