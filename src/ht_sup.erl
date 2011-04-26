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
    {ok, Sup} = supervisor:start_link({local, Name}, ?MODULE, []),
    {ok, WorkSup} = start_worker_sup(Sup, Name, Module, Function, Arguments),
    {ok, Pool} = start_pool(Sup, WorkSup, Name, Min, Max),
    {ok, Sup}.


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

