%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hottub Pool Benchmark Worker.

-module(benchmark).

-behaviour(gen_server).

%% api
-export([start_link/2, perform/3, done/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id=undefined, stats_table=undefined}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked test worker.
-spec start_link(Id::any(), StatsTable::atom()) -> {ok, pid()}.
start_link(Id, StatsTable) ->
    gen_server:start_link(?MODULE, [Id, StatsTable], []).

%% @doc Perform a function many times recording the call time in an ets table.
-spec perform(Pid::pid(), Function::fun(), Times::pos_integer()) -> ok.
perform(Pid, Function, Times) ->
    gen_server:cast(Pid, {perform, Function, Times}).

%% @doc Wait until the server is done working.
-spec done(Pid::pid()) -> ok.
done(Pid) ->
    gen_server:call(Pid, {done}, infinity).

%% @doc Stop a benchmark worker.
-spec stop(Pid::pid()) -> any().
stop(Pid) ->
    gen_server:cast(Pid, {stop}).


%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([Id, StatsTable]) ->
    {ok, #state{id=Id, stats_table=StatsTable}}.

%% @private
handle_call({done}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({perform, Function, Times}, State) ->
    lists:foreach(
        fun(I) -> 
            Begin = erlang:now(),
            Function(),
            End = erlang:now(),
            ets:insert(State#state.stats_table, {Begin, End})
        end,
        lists:seq(0, Times-1)),
    {noreply, State};
handle_cast({stop}, State) ->
    {stop, normal, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
