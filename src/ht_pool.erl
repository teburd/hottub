%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Manager. Manages an ETS table that contains the current list
%% of workers. Monitors workers for crashes and removes them from the ETS table.
%% @end

-module(ht_pool).

-behaviour(gen_server).

%% api
-export([start_link/1, add_worker/2, checkout_worker/1, checkin_worker/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {poolname=undefined, checkouts=queue:new()}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked pool manager.
-spec start_link(PoolName::atom()) -> {ok, pid()}.
start_link(PoolName) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName], []).

%% @doc Called by ht_worker after the worker process has started.
-spec add_worker(PoolName::atom(), Pid::pid()) -> term().
add_worker(PoolName, Pid) ->
    gen_server:cast(PoolName, {add_worker, Pid}).

%% @doc Checkin a worker.
-spec checkin_worker(PoolName::atom(), Pid::pid()) -> term().
checkin_worker(PoolName, Pid) ->
    gen_server:cast(PoolName, {checkin_worker, Pid}).

%% @doc Checkout a worker.
-spec checkout_worker(PoolName::atom()) -> Worker::pid() | undefined.
checkout_worker(PoolName) ->
    Worker = checkout_worker(PoolName, ets:first(PoolName)),
    case Worker of
        undefined ->
            checkout_next_worker(PoolName);
        Pid ->
            Pid
    end.

%% @doc Wait for a worker to be checked in and check it out.
-spec checkout_next_worker(PoolName::atom()) -> Pid::pid().
checkout_next_worker(PoolName) ->
    gen_server:call(PoolName, {checkout_next_worker}).


%% ------------------------------------------------------------------
%% private api 
%% ------------------------------------------------------------------

%% @doc Unused Worker Table Name
unused_worker_table(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_unused").

%% @doc Used Worker Table Name
used_worker_table(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_used").

%% @doc Checkout a worker using an atomic ets update_counter operation.
-spec checkout_worker(PoolName::atom(), Worker::pid()) -> Worker::pid() | undefined.
checkout_worker(_PoolName, '$end_of_table') ->
    undefined;
checkout_worker(PoolName, Worker) ->
    case ets:update_counter(PoolName, Worker, {3, 1}) of
        1 -> Worker;
        _ -> checkout_worker(PoolName, ets:next(PoolName, Worker)) 
    end.

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([PoolName]) ->
    ets:new(PoolName, [set, public, named_table,
            {read_concurrency, true}, {write_concurrency, true}]),
    {ok, #state{poolname=PoolName}}.

%% @private
handle_call({checkout_next_worker}, From, State) ->
    case checkout_worker(State#state.poolname, ets:first(State#state.poolname)) of
        undefined ->
            Queue = queue:in(From, State#state.checkouts),
            {noreply, State#state{checkouts=Queue}};
        Pid ->
            {reply, Pid, State}
    end.

%% @private
handle_cast({checkin_worker, Worker}, State) ->
    case queue:out(State#state.checkouts) of
        {{value, P}, Checkouts} ->
            gen_server:reply(P, Worker),
            {noreply, State#state{checkouts=Checkouts}};
        {empty, _Checkouts} ->
            ets:update_element(State#state.poolname, Worker, {3, 0}),
            {noreply, State}
    end;
handle_cast({add_worker, Worker}, State) ->
    MonitorRef = erlang:monitor(process, Worker),
    case queue:out(State#state.checkouts) of
        {{value, P}, Checkouts} ->
            ets:insert(State#state.poolname, {Worker, MonitorRef, 1}),
            gen_server:reply(P, Worker),
            {noreply, State#state{checkouts=Checkouts}};
        {empty, _Checkouts} ->
            ets:insert(State#state.poolname, {Worker, MonitorRef, 0}),
            {noreply, State}
    end.

%% @private
handle_info({'DOWN', _, _, Pid, _}, State) ->
    ets:delete(State#state.poolname, Pid),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
