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
-spec start_link(Name::atom(), Limit::pos_integer(), Module::module(),
    Function::function(), Arguments::list()) -> {ok, Sup::pid()}.
start_link(Name, Limit, Module, Function, Arguments) ->
    Name1 = list_to_atom(atom_to_list(Name) ++ "_sup"),
    {ok, Sup} = supervisor:start_link({local, Name1}, ?MODULE, []),
    {ok, _WorkSup} = start_worker_sup(Sup, Name, Module, Function, Arguments),
    {ok, _Pool} = start_pool(Sup, Name, Limit),
    {ok, Sup}.


%% ----------------------------------------------------------------------------
%% private api
%% ----------------------------------------------------------------------------

start_worker_sup(Sup, Name, Module, Function, Arguments) ->
    Name1 = list_to_atom(atom_to_list(Name) ++ "_worker_sup"),
    Spec = {Name1, {ht_worker_sup, start_link, [Name, Module, Function, Arguments]},
            permanent, 2000, supervisor, [ht_worker_sup]},
    supervisor:start_child(Sup, Spec).

start_pool(Sup, Name, Limit) ->
    Name1 = list_to_atom(atom_to_list(Name) ++ "_pool"),
    Spec = {Name1, {ht_pool, start_link, [Name, Limit]},
            permanent, 2000, worker, [ht_pool]},
    supervisor:start_child(Sup, Spec).


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

