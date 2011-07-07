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
    ets:new(ht_stats, [duplicate_bag, public, named_table,
            {read_concurrency, true}, {write_concurrency, true}]),
    BenchWorkers = lists:map(
        fun(Id) ->
            {ok, Pid} = benchmark:start_link(Id, ht_stats),
            benchmark:perform(Pid, BenchFun, 100),
            Pid
    end, lists:seq(0, 100)),
    lists:foreach(
        fun(Pid) ->
            benchmark:done(Pid),
            benchmark:stop(Pid)
        end, BenchWorkers),
    {Min, Max, Sum, Count} = ets:foldl(
        fun
            ({Tdiff}, undefined) ->
                {Tdiff, Tdiff, Tdiff, 1};
            ({Tdiff}, {Min, Max, Sum, Count}) ->
                {min(Min, Tdiff), max(Max, Tdiff), Sum+Tdiff, Count+1}
        end, undefined, ht_stats),
    Mean = Sum/Count,
    io:format(user, "Benchmark Results: Min ~p, Max ~p, Mean ~p~n", [Min, Max, Mean]),
    ok.
