%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc Hot Tub Supervisor. 

-module(ht_sup).

-behaviour(supervisor).

%% api
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start corbel system.
-spec start_link() -> {ok, Sup::pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ----------------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

