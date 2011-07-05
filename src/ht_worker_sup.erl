%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc Hot Tub Worker Supervisor. 

-module(ht_worker_sup).

-behaviour(supervisor).

%% api
-export([start_link/5]).

%% supervisor callbacks
-export([init/1]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start linked hot tub worker supervisor.
-spec start_link(PoolName::atom(), Limit::pos_integer(), Module::module(),
    Function::function(), Arguments::list()) -> {ok, Sup::pid()}.
start_link(PoolName, Limit, Module, Function, Arguments) ->
    supervisor:start_link({local, sup_name(PoolName)}, ?MODULE, [PoolName, Limit, Module, Function, Arguments]).


%% ----------------------------------------------------------------------------
%% private api
%% ----------------------------------------------------------------------------

sup_name(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_worker_sup").

worker_name(PoolName, Id) ->
    lists:flatten([atom_to_list(PoolName) | ["_worker_" | io_lib:format("~p", [Id])]]).


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([PoolName, Limit, M, F, A]) ->
    ChildSpecs = lists:map(
        fun (Id) ->
            {worker_name(PoolName, Id),
                {ht_worker, start_worker, [PoolName, {M, F, A}]},
                permanent, 2000, worker, [ht_worker, M]}
        end, lists:seq(0, Limit-1)),
    {ok, {{one_for_one, 10, 60}, ChildSpecs}}.
