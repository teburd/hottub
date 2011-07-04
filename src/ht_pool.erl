%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Manager. Manages an ETS table that contains the current list
%% of workers. Monitors workers for crashes and removes them from the ETS table.
%% @end

-module(ht_pool).

-behaviour(gen_server).

%% api
-export([start_link/1, add_worker/2, using_worker/2]).

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

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([PoolName]) ->
    io:format(user, "starting ets table for pool~n", []),
    _PidTable = ets:new(PoolName, [set, protected, named_table, {read_concurrency, true}]),
    io:format(user, "started ets table for pool~n", []),
    {ok, #state{poolname=PoolName}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({add_worker, Pid}, State) ->
    io:format(user, "adding worker to pool~n", []),
    MonitorRef = erlang:monitor(process, Pid),
    case ets:first(State#state.poolname) of
        '$end_of_table' ->
            io:format(user, "setting worker stats to 0~n", []),
            ets:insert(State#state.poolname, {Pid, MonitorRef, 0});
        {_, _, N} ->
            io:format(user, "setting worker stats to ~p~n", [N]),
            ets:insert(State#state.poolname, {Pid, MonitorRef, N})
    end,
    io:format(user, "added worker to pool~n", []),
    {noreply, State};
handle_cast({using_worker, Pid}, State) ->
    io:format(user, "adding worker usage~n", []),
    case ets:update_counter(State#state.poolname, Pid, {3, 1, 1000000000, -1}) of
        -1 ->
            io:format(user, "worker usage rollover~n", []),
            Workers = ets:tab2list(State#state.poolname),
            lists:foreach(fun({K, _, _}) -> ets:update_counter(State#state.poolname, K, 0) end, Workers);
        _ ->
            ok
    end,
    io:format(user, "added worker usage~n", []),
    {noreply, State}.

%% @private
handle_info({'DOWN', _, _, Pid, _}, State) ->
    io:format(user, "removing worker from pool~n", []),
    ets:delete(State#state.poolname, Pid),
    io:format(user, "removed worker from pool~n", []),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
