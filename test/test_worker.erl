%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Test Worker.

-module(test_worker).

-behaviour(gen_server).

%% api
-export([start_link/0, nothing/1, crash/1, increment/1, count/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {count=0}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked test worker.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Do nothing.
-spec nothing(Pid::pid()) -> ok.
nothing(_) ->
    ok.

%% @doc Increment counter.
-spec increment(Pid::pid()) -> any().
increment(Pid) ->
    gen_server:call(Pid, {increment}).

%% @doc Return counter.
-spec count(Pid::pid()) -> any().
count(Pid) ->
    gen_server:call(Pid, {count}).

%% @doc Crash a test worker.
-spec crash(Pid::pid()) -> any().
crash(Pid) ->
    gen_server:cast(Pid, {crash}).

%% @doc Stop a test worker.
-spec stop(Pid::pid()) -> any().
stop(Pid) ->
    gen_server:cast(Pid, {stop}).


%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call({increment}, _From, State) ->
    C = State#state.count + 1,
    {reply, ok, State#state{count=C}};
handle_call({count}, _From, State) ->
    {reply, State#state.count, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({crash}, State) ->
    {stop, crash, State};
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
