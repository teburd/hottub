%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Manager. 
%% Keeps two simple queues. One of workers and another of pending checkouts
%% if there are no available workers. On checkin or addition of a worker
%% if pending checkouts are waiting the worker is immediately handed off
%% with little cost it is otherwise added back in to the queue of unused
%% workers.
%%
%% A best attempt is made to ensure a worker is alive when checked out though
%% in some circumstances a process may be dead on arrival if the last request
%% caused it to have a delayed termination. Such scenarios are easily
%% possibly when gen_server:cast or something equivalent.
%%
%% checkin/checkout should not be used directly. Instead the simple,
%% functional wrapper hottub:execute should be used or one of its
%% additional halpers hottub:call or hottub:cast.
%%
%% @end

-module(ht_pool).

-behaviour(gen_server).

%% api
-export([start_link/1]).
-export([add_worker/2]).
-export([checkout_worker/1]).
-export([checkout_worker/2]).
-export([checkin_worker/2]).
-export([available_workers/1]).
-export([pending_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {poolname=undefined, unused=queue:new(), checkouts=queue:new(),
                available_workers=0, pending_tasks=0}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked pool manager.
-spec start_link(atom()) -> {ok, pid()}.
start_link(PoolName) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName], []).

%% @doc Called by ht_worker after the worker process has started.
-spec add_worker(atom(), pid()) -> term().
add_worker(PoolName, Pid) ->
    gen_server:cast(PoolName, {add_worker, Pid}).

%% @doc Checkin a worker.
-spec checkin_worker(atom(), pid()) -> term().
checkin_worker(PoolName, Pid) when is_pid(Pid) ->
    %% only checkin live workers
    case is_process_alive(Pid) of
        true ->
            gen_server:cast(PoolName, {checkin_worker, Pid});
        false ->
            ok
    end.

%% @doc Checkout a worker.
-spec checkout_worker(atom()) -> pid() | undefined.
checkout_worker(PoolName) ->
    checkout_worker(PoolName, infinity).

%% @doc Checkout a worker with a timeout
-spec checkout_worker(atom(), timeout()) -> pid() | undefined.
checkout_worker(PoolName, Timeout) ->
    Worker = gen_server:call(PoolName, {checkout_worker}, Timeout),
    %% only checkout live workers 
    case is_process_alive(Worker) of
        true ->
            Worker;
        false ->
            checkout_worker(PoolName, Timeout)
   end.

%% @doc Available workers returns the number of idle and alive workers.
-spec available_workers(atom()) -> int().
available_workers(PoolName) ->
    gen_server:call(PoolName, available_workers).

%% @doc Pending tasks returns the number of queue tasks awaiting a worker.
-spec pending_tasks(atom()) -> int().
pending_tasks(PoolName) -> 
    gen_server:call(PoolName, pending_tasks).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([PoolName]) ->
    {ok, #state{poolname=PoolName}}.

%% @private
handle_call({checkout_worker}, From, State) ->
    case queue:out(State#state.unused) of
        {{value, Worker}, Unused} ->
            AvailableWorkers = State#state.available_workers+1
            {reply, Worker, State#state{unused=Unused}};
        {empty, _Unused} ->
            PendingTasks = State#state.pending_tasks+1
            Checkouts = queue:in(From, State#state.checkouts),
            {noreply, State#state{checkouts=Checkouts}}
    end;
handle_call(available_workers, From, State) ->


%% @private
handle_cast({checkin_worker, Worker}, State) ->
    case queue:out(State#state.checkouts) of
        {{value, P}, Checkouts} ->
            PendingTasks = State#state.pending_tasks-1
            gen_server:reply(P, Worker),
            {noreply, State#state{checkouts=Checkouts, pending_tasks=PendingTasks}};
        {empty, _Checkouts} ->
            AvailableWorkers = State#state.available_workers+1
            Unused = queue:in(Worker, State#state.unused),
            {noreply, State#state{unused=Unused}}
    end;
handle_cast({add_worker, Worker}, State) ->
    erlang:monitor(process, Worker),
    case queue:out(State#state.checkouts) of
        {{value, P}, Checkouts} ->
            gen_server:reply(P, Worker),
            {noreply, State#state{checkouts=Checkouts, pending_tasks=State#state.pending_tasks-1}};
        {empty, _Checkouts} ->
            Unused = queue:in(Worker, State#state.unused),
            {noreply, State#state{unused=Unused, available_workers=State#state.available_workers+1}}
    end.

%% @private
handle_info({'DOWN', _, _, Worker, _}, State) ->
    Unused = queue:from_list(lists:delete(Worker,
        queue:to_list(State#state.unused))),
    AvailableWorkers = case Unused of
        State#state.unused ->
            State#state.available_workers
        _ ->
            State#state.available_workers-1
    end,
    {noreply, State#state{unused=Unused, available_workers=AvailableWorkers}}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
