-module(hottub_tests).
-include_lib("eunit/include/eunit.hrl").

%% Basic Worker Pool Test.
pool_start_stop_test() ->
    {ok, Pid} = hottub:start_link(ss_pool, 1, test_worker, start_link, []),
    ok = hottub:stop(ss_pool),
    ?assertEqual(false, is_process_alive(Pid)),
    ok.

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

pending_pool_tasks_test() ->
    NWorkers = 2,
    NOps = 5000,
    io:format(user, "Creating pool of ~p workers. Running the test ~p times.\n", [NWorkers, NOps]),
    hottub:start_link(items_test, NWorkers, test_worker, start_link, []),
    lists:map(fun(_) -> 
        %% sleep 10 milliseconds
        hottub:execute(items_test, fun(Worker) -> 
            test_worker:sleep(Worker, 1)
        end)
    end, lists:seq(0, NOps)),
    timer:sleep(2500),
    io:format(user, "\tTotal items queued in the pool: ~p\n", [ht_pool:pending_pool_tasks(items_test)]),
    timer:sleep(2500),
    io:format(user, "\tTotal items queued in the pool: ~p\n", [ht_pool:pending_pool_tasks(items_test)]),
    ok.

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
