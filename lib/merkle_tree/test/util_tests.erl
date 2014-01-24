-module(util_tests).
-include_lib("eunit/include/eunit.hrl").

-include_lib("triq/include/triq.hrl").

triq_test_() ->
    {timeout, 60,
     fun() ->
             true = triq:module(?MODULE)
     end}.

prop_pos_to_bin_encode_decode() ->
    ?FORALL(Xs, list(oneof([l,r])),
	    Xs == util:bin_to_pos(util:pos_to_bin(Xs))).
