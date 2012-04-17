%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick.
%% @doc HotTub Supervisor. 

-module(ht_sup).

-behaviour(supervisor).

%% api
-export([start_link/5, stop/1]).

%% supervisor callbacks
-export([init/1]).


%% ----------------------------------------------------------------------------
%% api
%% ----------------------------------------------------------------------------

%% @doc Start linked hottub supervisor.
-spec start_link(atom(), pos_integer(), atom(), atom(), list(any())) -> 
    ignore | {error, term()} | {ok, pid()}.
start_link(PoolName, Limit, Module, Function, Arguments) when
        is_atom(PoolName),
        is_integer(Limit),
        is_atom(Module),
        is_atom(Function),
        is_list(Arguments) ->
    supervisor:start_link({local, sup_name(PoolName)}, ?MODULE,
        [PoolName, Limit, Module, Function, Arguments]).

%% @doc Stop a hottub supervisor.
-spec stop(atom()) -> ok.
stop(PoolName) ->
    SupName = sup_name(PoolName),
    case whereis(SupName) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            Ref = monitor(process, Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, shutdown} ->
                    ok
            after 
                1000 ->
                    erlang:exit(Pid, kill),
                    ok
            end
    end.

%% ----------------------------------------------------------------------------
%% private api
%% ----------------------------------------------------------------------------

sup_name(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_sup").

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
    {ok, {{one_for_one, 5, 10}, [PoolSpec, WorkerSupSpec]}}.
