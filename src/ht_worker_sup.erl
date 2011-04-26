%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc Hot Tub Worker Supervisor. 

-module(ht_worker_sup).

-behaviour(supervisor).

%% api
-export([start_link/4]).

%% supervisor callbacks
-export([init/1]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start linked hot tub worker supervisor.
-spec start_link(Name::atom(), Module::module(),
    Function::function(), Arguments::list()) -> {ok, Sup::pid()}.
start_link(Name, Module, Function, Arguments) ->
    Name1 = list_to_atom(atom_to_list(Name) ++ "_worker_sup"),
    supervisor:start_link({local, Name1}, ?MODULE, [Name, Module, Function, Arguments]).


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([Name, M, F, A]) ->
    Name1 = list_to_atom(atom_to_list(Name) + "_worker"),
    ChildSpec = {Name1, {M, F, A},
        temporary, 2000, worker, [M]},
    {ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.
