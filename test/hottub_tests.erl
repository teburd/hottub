-module(hottub_tests).
-include_lib("eunit/include/eunit.hrl").

%% Basic Worker Pool Test.
pool_start_stop_test() ->
    {ok, Pid} = hottub:start_link(test_pool, 1, test_worker, start_link, []),
    ok = hottub:stop(test_pool),
    ?assertEqual(false, is_process_alive(Pid)).

pool_crash_test() ->
    hottub:start_link(test_pool, 1, test_worker, start_link, []),
    hottub:execute(test_pool,
        fun(Worker) ->
            ?assert(is_pid(Worker)),
            test_worker:crash(Worker)
        end),
    hottub:execute(test_pool,
        fun(Worker) ->
            ?assert(is_pid(Worker))
        end),
    hottub:stop(test_pool),
    ok.

%% Benchmark Pool Checkout/Checkin Test.
pool_benchmark_test_() ->
    {timeout, 120, ?_assertEqual(ok, begin benchmark() end)}.

benchmark() ->
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
