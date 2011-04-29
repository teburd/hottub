%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc Hot Tub Worker Supervisor. 

-module(ht_worker_sup).

-behaviour(supervisor).

%% api
-export([start_link/4, start_worker/1]).

%% supervisor callbacks
-export([init/1]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start linked hot tub worker supervisor.
-spec start_link(PoolName::atom(), Module::module(),
    Function::function(), Arguments::list()) -> {ok, Sup::pid()}.
start_link(PoolName, Module, Function, Arguments) ->
    supervisor:start_link({local, sup_name(PoolName)}, ?MODULE, [PoolName, Module, Function, Arguments]).

%% @doc Start a worker.
-spec start_worker(PoolName::atom()) -> {ok, Worker::pid()}.
start_worker(PoolName) ->
    supervisor:start_child(sup_name(PoolName), []).


%% ----------------------------------------------------------------------------
%% private api
%% ----------------------------------------------------------------------------

sup_name(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_worker_sup").


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([PoolName, M, F, A]) ->
    Name = atom_to_list(PoolName) ++ "_worker",
    ChildSpec = {Name, {M, F, A},
        temporary, 2000, worker, [M]},
    {ok, {{simple_one_for_one, 1, 1}, [ChildSpec]}}.
