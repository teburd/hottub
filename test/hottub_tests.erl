-module(hottub_tests).
-include_lib("eunit/include/eunit.hrl").

pool_test() ->
    ?debugHere,
    hottub:start(test_pool, 4, test_worker, start_link, []),
    ?debugHere,
    ?assertEqual(true, is_pid(hottub:worker(test_pool))),
    ?debugHere,
    ok.
