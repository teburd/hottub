About
-----

Hot Tub is a permanent erlang worker pool. It keeps some number of worker
processes alive at all times and then attempts to load balance requests across
the set of workers by picking the one with the fewest current users.

Primarily I use this as a database worker pool though it certainly could have
other uses.

Example
-------


demo_worker.erl
+++++++++++++++

::

    -module(demo_worker.erl).

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


From the Shell
++++++++++++++

::

    hottub:start(demo, 5, demo_worker, start_link, []).
    hottub:call(demo, {add, 5, 5}).
    hottub:cast(demo, {print, "what up from a pool"}).
    hottub:with_worker(demo, 
        fun(Worker) -> 
            io:format("causing a worker to crash~n"),
            gen_server:call(Worker, {hocus_pocus}) end).
    hottub:stop(demo).
