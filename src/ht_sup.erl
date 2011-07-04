%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc Hot Tub Supervisor. 

-module(ht_sup).

-behaviour(supervisor).

%% api
-export([start_link/5]).

%% supervisor callbacks
-export([init/1]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start linked hot tub supervisor.
-spec start_link(PoolName::string(), Limit::pos_integer(), Module::module(),
    Function::function(), Arguments::list()) -> {ok, Sup::pid()}.
start_link(PoolName, Limit, Module, Function, Arguments) ->
    supervisor:start_link(?MODULE, [PoolName, Limit, Module, Function, Arguments]).


%% ----------------------------------------------------------------------------
%% private api
%% ----------------------------------------------------------------------------

worker_sup_name(PoolName) -> 
    atom_to_list(PoolName) ++ "_worker_sup".

pool_name(PoolName) ->
    atom_to_list(PoolName) ++ "_pool".


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([PoolName, Limit, Module, Function, Arguments]) ->
    PoolSpec = {pool_name(PoolName),
        {ht_pool, start_link, [PoolName]},
        permanent, 2000, worker, [ht_pool]},
    WorkerSupSpec = {worker_sup_name(PoolName),
        {ht_worker_sup, start_link, [PoolName, Limit, Module, Function,
                Arguments]},
        permanent, 2000, supervisor, [ht_worker_sup]},
    io:format(user, "starting pool supervisor~n", []),
    {ok, {{one_for_one, 5, 10}, [PoolSpec, WorkerSupSpec]}}.
