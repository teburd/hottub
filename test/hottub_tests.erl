-module(hottub_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test to ensure workers really do exist.
pool_test() ->
    hottub:start(test_pool, 1, test_worker, start_link, []),
    hottub:execute(test_pool,
        fun(Worker) ->
            ?assertEqual(true, is_pid(Worker)),
            test_worker:crash(Worker)
        end),
    hottub:execute(test_pool,
        fun(Worker) ->
            ?assertEqual(true, is_pid(Worker))
        end),
    ?debugHere,
    ok.
