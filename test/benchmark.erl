%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hottub Pool Benchmark Worker.

-module(benchmark).

-behaviour(gen_server).

%% api
-export([start_link/1, perform/3, results/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id=undefined, min=0, max=0, avg=0}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked test worker.
-spec start_link(Id::any()) -> {ok, pid()}.
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

%% @doc Perform a function many times recording the call time in an ets table.
-spec perform(Pid::pid(), Function::fun(), Times::pos_integer()) -> ok.
perform(Pid, Function, Times) ->
    gen_server:cast(Pid, {perform, Function, Times}).

%% @doc Wait until the server is done working then return the min, max, and average call time.
-spec results(Pid::pid()) -> {float(), float(), float()}.
results(Pid) ->
    gen_server:call(Pid, {results}, infinity).

%% @doc Stop a benchmark worker.
-spec stop(Pid::pid()) -> any().
stop(Pid) ->
    gen_server:cast(Pid, {stop}).


%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([Id]) ->
    {ok, #state{id=Id}}.

%% @private
handle_call({results}, _From, State) ->
    {reply, {State#state.min, State#state.max, State#state.avg}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({perform, Function, Times}, State) ->
    {Min, Max, Sum} = 
        lists:foldl(
            fun(_, {Min, Max, Sum}) -> 
                Begin = erlang:now(),
                Function(),
                End = erlang:now(),
                Tdiff = timer:now_diff(End, Begin)*0.001,
                {min(Min, Tdiff), max(Max, Tdiff), Sum+Tdiff}
            end,
            {1000000000, 0, 0},
            lists:seq(0, Times-1)),
    Mean = Sum/Times,
    {noreply, State#state{min=Min, max=Max, avg=Mean}};
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
