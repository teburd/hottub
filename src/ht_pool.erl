%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Balancer. Attempts to always use what it believes is an
%% idle worker if possible. If not it resorts to round robin distribution.
%% @end

-module(ht_pool).

-behaviour(gen_fsm).

%% api
-export([start_link/1, with_worker/2, call/2, cast/2]).

%% worker callback
-export([worker_started/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {poolname=undefined, idle=[], busy=[]}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked pool manager.
-spec start_link(PoolName::atom()) -> {ok, pid()}.
start_link(PoolName) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName], []).

%% @doc Perform a function with a worker process.
-spec with_worker(PoolName::atom(), Fun::fun()) -> any().
with_worker(PoolName, Fun) ->
    {ok, Worker} = checkout_worker(PoolName),
    try
        Fun(Worker)
    after
        checkin_worker(PoolName, Worker)
    end.

%% @doc Perform a gen_server:call with a worker process.
-spec call(PoolName::atom(), Args::any()) -> Result::any().
call(PoolName, Args) ->
    {ok, Worker} = checkout_worker(PoolName),
    try
        gen_server:call(Worker, Args)
    after
        checkin_worker(PoolName, Worker)
    end.

%% @doc Perform a gen_server:call with a worker process.
-spec cast(PoolName::atom(), Args::any()) -> Result::any().
cast(PoolName, Args) ->
    {ok, Worker} = checkout_worker(PoolName),
    try
        gen_server:cast(Worker, Args)
    after
        checkin_worker(PoolName, Worker)
    end.

%% @private
%% @doc Called by ht_worker after the worker process has started.
-spec worker_started(PoolName::atom(), Worker::pos_integer(), Pid::pid()) -> term().
worker_started(PoolName, Worker, Pid) ->
    gen_server:cast(PoolName, {worker_started, Worker, Pid}).

%% ------------------------------------------------------------------
%% private api 
%% ------------------------------------------------------------------

checkout_worker(PoolName) ->
    gen_server:call(PoolName, {checkout}).

checkin_worker(PoolName, Worker) ->
    gen_server:cast(PoolName, {checkin, Worker}).

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
init([PoolName]) ->
    {ok, #state{poolname=PoolName}}.

%% @private
handle_call({checkout}, _From, State) ->
    {Worker, _Checkouts} = dict:fold(fun unused_worker/3, undefined, State#state.workers),
    Workers = dict:update_counter(Worker, 1, State#state.workers),
    {reply, {ok, Worker}, State#state{workers=Workers}}.

%% @private
handle_Cast({worker_started, _Worker, Pid}, State) ->
    dict:
    

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
    Workers = dict:erase(Pid, State#state.workers),
    {noreply, State#state{workers=Workers}}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
