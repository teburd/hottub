%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Manager. Manages an ETS table that contains the current list
%% of workers. Monitors workers for crashes and removes them from the ETS table.
%% @end

-module(ht_pool).

-behaviour(gen_server).

%% api
-export([start_link/1, add_worker/2, using_worker/2]).

%% test api
-ifdef(TEST).
-export([set_worker_usage/3]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {poolname=undefined}).


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

%% @doc Called by hottub to signify usage.
-spec using_worker(PoolName::atom(), Pid::pid()) -> term().
using_worker(PoolName, Pid) ->
    gen_server:cast(PoolName, {using_worker, Pid}).

-ifdef(TEST).
set_worker_usage(PoolName, Pid, Usage) ->
    gen_server:call(PoolName, {set_worker_usage, Pid, Usage}).
-endif.

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([PoolName]) ->
    _PidTable = ets:new(PoolName, [set, protected, named_table, {read_concurrency, true}]),
    {ok, #state{poolname=PoolName}}.

%% @private
-ifdef(TEST).
handle_call({set_worker_usage, Pid, Usage}, _From, State) ->
    ets:update_element(State#state.poolname, Pid, {3, Usage}),
    {reply, ok, State}.
-else.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
-endif.

%% @private
handle_cast({add_worker, Pid}, State) ->
    MonitorRef = erlang:monitor(process, Pid),
    case ets:first(State#state.poolname) of
        '$end_of_table' ->
            ets:insert(State#state.poolname, {Pid, MonitorRef, 0});
        K ->
            [{_, _, N}] = ets:lookup(State#state.poolname, K),
            ets:insert(State#state.poolname, {Pid, MonitorRef, N})
    end,
    {noreply, State};
handle_cast({using_worker, Pid}, State) ->
    case ets:update_counter(State#state.poolname, Pid, {3, 1, 1000000000, -1}) of
        -1 ->
            Workers = ets:tab2list(State#state.poolname),
            lists:foreach(fun({K, _, _}) -> ets:update_element(State#state.poolname, K, {3, 0}) end, Workers);
        _ ->
            ok
    end,
    {noreply, State}.

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
