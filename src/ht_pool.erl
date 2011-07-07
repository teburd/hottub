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

-record(state, {poolname=undefined, unused=undefined, used=undefined, checkouts=queue:new()}).


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
    UnusedTable = unused_worker_table(PoolName),
    UsedTable = used_worker_table(PoolName),
    ets:safe_fixtable(UnusedTable, true),
    Worker = checkout_worker(UnusedTable, UsedTable, ets:first(UnusedTable)),
    ets:safe_fixtable(UnusedTable, false),
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
-spec checkout_worker(UnusedTable::atom(), UsedTable::atom(), Worker::pid()) -> Worker::pid() | undefined.
checkout_worker(_UnusedTable, _UsedTable, '$end_of_table') ->
    undefined;
checkout_worker(UnusedTable, UsedTable, Worker) ->
    case ets:lookup(UnusedTable, Worker) of
        [Object] ->
            case ets:insert_new(UsedTable, Object) of
                true ->
                    ets:delete(UnusedTable, Worker),
                    Worker;
                false -> checkout_worker(UnusedTable, UsedTable, ets:next(UnusedTable, Worker)) 
            end;
        [] ->
            checkout_worker(UnusedTable, UsedTable, ets:next(UnusedTable, Worker))
    end.

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([PoolName]) ->
    UnusedTable = unused_worker_table(PoolName),
    io:format(user, "unused table is ~p~n", [UnusedTable]),
    ets:new(UnusedTable, [set, public, named_table,
            {read_concurrency, true}, {write_concurrency, true}]),
    UsedTable = used_worker_table(PoolName),
    ets:new(UsedTable, [set, public, named_table,
            {read_concurrency, true}, {write_concurrency, true}]),
    {ok, #state{poolname=PoolName, unused=UnusedTable, used=UsedTable}}.

%% @private
handle_call({checkout_next_worker}, From, State) ->
    ets:safe_fixtable(State#state.unused, true),
    case checkout_worker(State#state.unused, State#state.used, ets:first(State#state.unused)) of
        undefined ->
            ets:safe_fixtable(State#state.unused, false),
            Queue = queue:in(From, State#state.checkouts),
            {noreply, State#state{checkouts=Queue}};
        Pid ->
            ets:safe_fixtable(State#state.unused, false),
            {reply, Pid, State}
    end.

%% @private
handle_cast({checkin_worker, Worker}, State) ->
    case queue:out(State#state.checkouts) of
        {{value, P}, Checkouts} ->
            gen_server:reply(P, Worker),
            {noreply, State#state{checkouts=Checkouts}};
        {empty, _Checkouts} ->
            case ets:lookup(State#state.used, Worker) of
                [Object] ->
                    ets:delete(State#state.used, Worker),
                    ets:insert(State#state.unused, Object),
                    {noreply, State};
                [] ->
                    {noreply, State}
            end
    end;
handle_cast({add_worker, Worker}, State) ->
    MonitorRef = erlang:monitor(process, Worker),
    case queue:out(State#state.checkouts) of
        {{value, P}, Checkouts} ->
            ets:insert(State#state.used, {Worker, MonitorRef}),
            gen_server:reply(P, Worker),
            {noreply, State#state{checkouts=Checkouts}};
        {empty, _Checkouts} ->
            ets:insert(State#state.unused, {Worker, MonitorRef}),
            {noreply, State}
    end.

%% @private
handle_info({'DOWN', _, _, Worker, _}, State) ->
    ets:delete(State#state.unused, Worker),
    ets:delete(State#state.used, Worker),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
