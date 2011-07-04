%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Test Worker.

-module(test_worker).

-behaviour(gen_server).

%% api
-export([start_link/0, crash/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked test worker.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    io:format(user, "starting test worker~n", []),
    {ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({crash}, State) ->
    io:format(user, "crashing test worker~n", []),
    {stop, crash, State};
handle_cast({stop}, State) ->
    io:format(user, "stopping test worker~n", []),
    {stop, normal, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, _State) ->
    io:format(user, "test worker terminating with reason ~p~n", [Reason]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
