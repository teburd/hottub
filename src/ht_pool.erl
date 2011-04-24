%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Hot Tub Pool State Machine.

-module(ht_pool).

-behaviour(gen_fsm).

%% api
-export([start_link/3, with_worker/2]).

%% gen_fsm callbacks
-export([init/1, initialized/2, started/2, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {sup=undefined, pool_procs=queue:new()}).
