-module(hottub_tests).
-include_lib("eunit/include/eunit.hrl").

%% Basic Worker Pool Test.
pool_crash_test() ->
    hottub:start(test_pool, 1, test_worker, start_link, []),
    hottub:execute(test_pool,
        fun(Worker) ->
            ?assert(is_pid(Worker)),
            test_worker:crash(Worker)
        end),
    hottub:execute(test_pool,
        fun(Worker) ->
            ?assert(is_pid(Worker))
        end),
    ?debugHere,
    ok.

%% Benchmark Pool Checkout/Checkin Test.
pool_benchmark_test() ->
    hottub:start(bench_pool, 100, test_worker, start_link, []),
    BenchFun = fun() ->
        hottub:execute(bench_pool,
            fun(Worker) ->
                test_worker:nothing(Worker)
            end)
    end,
    ets:new(ht_stats, [bag, public, named_table,
            {read_concurrency, true}, {write_concurrency, true}]),
    BenchWorkers = lists:map(
        fun(Id) ->
            {ok, Pid} = benchmark_worker:start_link(Id, ht_stats),
            benchmark_worker:perform(Pid, BenchFun, 10000),
            Pid
    end, lists:seq(0, 1000)),
    lists:foreach(
        fun(Pid) ->
            benchmark_worker:done(Pid),
            benchmark_worker:stop(Pid),
        end, BenchWorkers),
    {MinTime, MeanTime, MaxTime} = ets:foldl(
        fun({Begin, End}, {Min, Mean, Avg}) ->
            {0.0, 0.0, 0.0}
        end, ht_stats),
    ?debugVal(MinTime),
    ?debugVal(MeanTime),
    ?debugVal(MaxTime),
    ok.
