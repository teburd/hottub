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
-spec start_link(PoolName::atom(), Limit::pos_integer(), Module::module(),
    Function::function(), Arguments::list()) -> {ok, Sup::pid()}.
start_link(PoolName, Limit, Module, Function, Arguments) ->
    Name = list_to_atom(atom_to_list(PoolName) ++ "_sup"),
    {ok, Sup} = supervisor:start_link({local, Name}, ?MODULE, []),
    {ok, _WorkSup} = start_worker_sup(Sup, PoolName, Module, Function, Arguments),
    {ok, _Pool} = start_pool(Sup, PoolName, Limit),
    {ok, Sup}.


%% ----------------------------------------------------------------------------
%% private api
%% ----------------------------------------------------------------------------

start_worker_sup(Sup, PoolName, Module, Function, Arguments) ->
    Name = atom_to_list(PoolName) ++ "_worker_sup",
    Spec = {Name, {ht_worker_sup, start_link, [PoolName, Module, Function, Arguments]},
            permanent, 2000, supervisor, [ht_worker_sup]},
    supervisor:start_child(Sup, Spec).

start_pool(Sup, PoolName, Limit) ->
    Name = atom_to_list(PoolName) ++ "_pool",
    Spec = {Name, {ht_pool, start_link, [PoolName, Limit]},
            permanent, 2000, worker, [ht_pool]},
    supervisor:start_child(Sup, Spec).


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

