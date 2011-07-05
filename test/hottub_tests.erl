-module(hottub_tests).
-include_lib("eunit/include/eunit.hrl").

pool_test() ->
    ?debugHere,
    hottub:start(test_pool, 1, test_worker, start_link, []),
    ?debugHere,
    timer:sleep(20),
    Pid = hottub:worker(test_pool),
    ?assertEqual(true, is_pid(Pid)),
    test_worker:crash(Pid),
    timer:sleep(20),
    ?assertEqual(true, is_pid(hottub:worker(test_pool))),
    ?debugHere,
    ok.

pool_usage_test() ->
    ?debugHere,
    hottub:start(test_usage_pool, 2, test_worker, start_link, []),
    timer:sleep(10),
    ?debugHere,
    lists:foreach(fun(_) -> test_worker:increment(hottub:worker(test_usage_pool)) end, lists:seq(0, 9)),
    timer:sleep(100),
    ?debugHere,
    lists:foreach(fun({_, _, N}) -> ?assertEqual(N, 5) end, ets:tab2list(test_usage_pool)),
    test_worker:crash(hottub:worker(test_usage_pool)),
    ?debugHere,
    timer:sleep(20),
    lists:foreach(fun({_, _, N}) -> ?assertEqual(N, 5) end, ets:tab2list(test_usage_pool)),
    ?debugHere,
    ok.
