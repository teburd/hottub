%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool Balancer.

-module(ht_pool).

-behaviour(gen_server).

%% api
-export([start_link/2, with_worker/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {workers=dict:new()}).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start a linked pool manager.
-spec start_link(Name::atom(), Limit::pos_integer()) -> {ok, pid()}.
start_link(Name, Limit) ->
    gen_server:start_link({local, Name}, [Limit], []).

%% @doc Perform a function with a worker process.
-spec with_worker(Name::atom(), Fun::fun()) -> any().
with_worker(Name, Fun) ->
    {ok, Worker} = checkout_worker(Name),
    try
        Fun(Worker)
    after
        checkin_worker(Name, Worker)
    end.


%% ------------------------------------------------------------------
%% private api 
%% ------------------------------------------------------------------

checkout_worker(Name) ->
    gen_server:call(Name, {checkout}).

checkin_worker(Name, Worker) ->
    gen_server:cast(Name, {checkin, Worker}).

%% @doc Find the least used worker in the pool.
unused_worker(Key, Value, undefined) ->
    {Key, Value};
unused_worker(_Key, Value, {OKey, OValue}) when OValue < Value ->
    {OKey, OValue}.


%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call({checkout}, _From, State) ->
    {Worker, _Checkouts} = dict:fold(fun unused_worker/3, State#state.workers),
    Workers = dict:update_counter(Worker, 1, State#state.workers),
    {reply, Worker, State#state{workers=Workers}}.

%% @private
handle_cast({checkin, Worker}, State) ->
    case dict:is_key(Worker) of
        true ->
            Workers = dict:update_counter(Worker, -1, State#state.workers),
            {noreply, State#state{workers=Workers}};
        false ->
            {noreply, State}
    end.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
