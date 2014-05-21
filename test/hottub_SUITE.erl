%% Copyright (c) 2011-2013, Tom Burdick <thomas.burdick@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(hottub_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).

%% Tests.
-export([start_stop/1]).
-export([dead_worker/1]).
-export([timeout/1]).
-export([crash/1]).
-export([benchmark/1]).

all() ->
    [
        start_stop,
        dead_worker,
        timeout,
        crash,
        benchmark
    ].

%% Basic Worker Pool Test.
start_stop(_Config) ->
    {ok, Pid} = hottub:start_link(ss_pool, 1, test_worker, start_link, []),
    ok = hottub:stop(ss_pool),
    false = is_process_alive(Pid),
    ok.

dead_worker(_Config) ->
    hottub:start_link(dead_pool, 1, test_worker, start_link, []),
    Pid = self(),
    BlockFun = fun() ->
        hottub:execute(dead_pool, fun(Worker) ->
            Pid ! waiting,
            receive
                continue ->
                    test_worker:crash(Worker)
            end
        end)
    end,
    BlockedFun = fun() ->
        Pid ! waiting,
        hottub:execute(dead_pool, fun(Worker) ->
            case is_process_alive(Worker) of
                true ->
                    Pid ! ok;
                false ->
                    Pid ! fail
            end
        end)
    end,
    Blocker = spawn(BlockFun),
    receive
        waiting ->
            ok
    end,
    spawn(BlockedFun),
    receive 
        waiting ->
            ok
    end,
    Blocker ! continue,
    receive
        ok ->
            ok;
        fail ->
            throw(fail) 
    end.

timeout(_Config) ->
    hottub:start_link(test_pool, 1, test_worker, start_link, []),
    hottub:execute(test_pool,
        fun(Worker) ->
            true = is_pid(Worker),
            try
                hottub:execute(test_pool, 100, fun(Worker0) ->
                    false = is_pid(Worker0)
                end)
            catch
                exit:{timeout, _} ->
                    ok
            end
        end),
    hottub:stop(test_pool),
    ok.


crash(_Config) ->
    hottub:start_link(test_pool, 1, test_worker, start_link, []),
    hottub:execute(test_pool,
        fun(Worker) ->
            true = is_pid(Worker),
            test_worker:crash(Worker)
        end),
    hottub:execute(test_pool,
        fun(Worker) ->
            true = is_pid(Worker)
        end),
    hottub:stop(test_pool),
    ok.

benchmark(_Config) ->
    NWorkers = 500,
    hottub:start_link(bench_pool, 100, test_worker, start_link, []),
    BenchFun = fun() ->
        hottub:execute(bench_pool,
            fun(Worker) ->
                test_worker:nothing(Worker)
            end)
    end,
    BenchWorkers = lists:map(
        fun(Id) ->
            {ok, Pid} = benchmark:start_link(Id),
            benchmark:perform(Pid, BenchFun, 1000),
            Pid
    end, lists:seq(0, NWorkers)),
    {Min, Max, AvgSum} = lists:foldl(
        fun(Pid, {Min, Max, AvgSum}) ->
            {RMin, RMax, RAvg} = benchmark:results(Pid),
            {min(RMin, Min), max(RMax, Max), AvgSum + RAvg}
        end, {10000000000, 0, 0}, BenchWorkers),
    Mean = AvgSum/NWorkers,
    hottub:stop(bench_pool),
    io:format(user, "Worker Execute Results: Min ~pms, Max ~pms, Mean ~pms~n", [Min, Max, Mean]),
    ok.
