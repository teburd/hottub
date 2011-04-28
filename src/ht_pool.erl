%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Balancer.

-module(ht_pool).

-behaviour(gen_server).

%% api
-export([start_link/2, with_worker/2, call/2, cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {poolname=undefined, workers=dict:new()}).


-ifndef(TIMEON).
%% Yes, these need to be on a single line to work...
-define(TIMEON, erlang:put(debug_timer, [now()|case erlang:get(debug_timer) == undefined of true -> []; false -> erlang:get(debug_timer) end])).
-define(TIMEOFF(Var), io:format("~s :: ~10.2f ms : ~p~n", [string:copies(" ", length(erlang:get(debug_timer))), (timer:now_diff(now(), hd(erlang:get(debug_timer)))/1000), Var]), erlang:put(debug_timer, tl(erlang:get(debug_timer)))).
-endif.

%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked pool manager.
-spec start_link(PoolName::atom(), Limit::pos_integer()) -> {ok, pid()}.
start_link(PoolName, Limit) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName, Limit], []).

%% @doc Perform a function with a worker process.
-spec with_worker(PoolName::atom(), Fun::fun()) -> any().
with_worker(PoolName, Fun) ->
    ?TIMEON,
    {ok, Worker} = checkout_worker(PoolName),
    ?TIMEOFF(checkout_worker),
    try
        Fun(Worker)
    after
        checkin_worker(PoolName, Worker)
    end.

%% @doc Perform a gen_server:call with a worker process.
-spec call(PoolName::atom(), Args::any()) -> Result::any().
call(PoolName, Args) ->
    ?TIMEON,
    {ok, Worker} = checkout_worker(PoolName),
    ?TIMEOFF(checkout_worker),
    try
        gen_server:call(Worker, Args)
    after
        checkin_worker(PoolName, Worker)
    end.

%% @doc Perform a gen_server:call with a worker process.
-spec cast(PoolName::atom(), Args::any()) -> Result::any().
cast(PoolName, Args) ->
    ?TIMEON,
    {ok, Worker} = checkout_worker(PoolName),
    ?TIMEOFF(checkout_worker),
    try
        gen_server:cast(Worker, Args)
    after
        checkin_worker(PoolName, Worker)
    end.


%% ------------------------------------------------------------------
%% private api 
%% ------------------------------------------------------------------

checkout_worker(PoolName) ->
    gen_server:call(PoolName, {checkout}).

checkin_worker(PoolName, Worker) ->
    gen_server:cast(PoolName, {checkin, Worker}).

%% @doc Start a worker and return the tuple {Worker, 0}
start_worker(PoolName) ->
    {ok, Worker} = ht_worker_sup:start_worker(PoolName),
    monitor(process, Worker),
    {Worker, 0}.

%% @doc Find the least used worker in the pool.
unused_worker(Key, Value, undefined) ->
    {Key, Value};
unused_worker(_Key, Value, {OKey, OValue}) when OValue < Value ->
    {OKey, OValue};
unused_worker(Key, Value, {_OKey, _OValue}) ->
    {Key, Value}.

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([PoolName, Limit]) ->
    io:format("starting workers~n"),
    Workers =
        lists:map(
            fun(_) ->
                start_worker(PoolName)
            end,
            lists:seq(0, Limit)),
    io:format("started workers~n"),
    {ok, #state{poolname=PoolName, workers=dict:from_list(Workers)}}.

%% @private
handle_call({checkout}, _From, State) ->
    {Worker, _Checkouts} = dict:fold(fun unused_worker/3, undefined, State#state.workers),
    Workers = dict:update_counter(Worker, 1, State#state.workers),
    {reply, {ok, Worker}, State#state{workers=Workers}}.

%% @private
handle_cast({checkin, Worker}, State) ->
    case dict:is_key(Worker, State#state.workers) of
        true ->
            Workers = dict:update_counter(Worker, -1, State#state.workers),
            {noreply, State#state{workers=Workers}};
        false ->
            {noreply, State}
    end.

%% @private
handle_info({'DOWN', _, _, Pid, _}, State) ->
    Workers0 = dict:erase(Pid, State#state.workers),
    {Worker, Count} = start_worker(State#state.poolname),
    Workers1 = dict:store(Worker, Count, Workers0),
    {noreply, State#state{workers=Workers1}}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
