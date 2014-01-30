-module(merkle_tree_pb_tests).
-include_lib("eunit/include/eunit.hrl").

%% test encoding and decoding of the different merkle-tree protocol
%% messages.

gen_pair() ->
    merkle_tree_pb_util:create_pair(<<"key">>, <<"val">>).

merkle_msg_test() ->
    Pair = gen_pair(),
    HSReq = merkle_tree_pb_util:create_handshake_req(1,0,[Pair]),
    Data = merkle_tree_pb_util:create_merkle_msg(HSReq),
    gen_test(Data,
             fun merkle_tree_pb:encode_merklemsg/1,
             fun merkle_tree_pb:decode_merklemsg/1).

handshake_req_test() ->
    Pair = gen_pair(),
    Data = merkle_tree_pb_util:create_handshake_req(1,0,[Pair]),
    gen_test(Data,
             fun merkle_tree_pb:encode_handshakereq/1,
             fun merkle_tree_pb:decode_handshakereq/1).

ack_resp_test() ->
    Data = merkle_tree_pb_util:create_ack_resp(),
    gen_test(Data,
             fun merkle_tree_pb:encode_ackresp/1,
             fun merkle_tree_pb:decode_ackresp/1).

fetch_req_test() ->
    PosBin = <<1:1, 0:1, 1:1, 1:1, 0:1, 1:1>>,
    Data = merkle_tree_pb_util:create_fetch_req(PosBin),
    gen_test(Data,
             fun merkle_tree_pb:encode_fetchreq/1,
             fun merkle_tree_pb:decode_fetchreq/1).

leaf_test() ->
    Data = gen_leaf(),
    gen_test(Data,
             fun merkle_tree_pb:encode_leaf/1,
             fun merkle_tree_pb:decode_leaf/1).

gen_leaf() ->
    PosBin = <<500:9>>,
    LeafData = <<"leafdata">>,
    merkle_tree_pb_util:create_leaf(PosBin, LeafData).

leaves_test() ->
    Leaves  = [gen_leaf() || _ <- lists:seq(1,100)],
    Data = merkle_tree_pb_util:create_leaves_resp(Leaves),
    gen_test(Data,
             fun merkle_tree_pb:encode_leavesresp/1,
             fun merkle_tree_pb:decode_leavesresp/1).

fetch_done_msg_test() ->
    Data = merkle_tree_pb_util:create_fetch_done_msg(),
    gen_test(Data,
             fun merkle_tree_pb:encode_fetchdonemsg/1,
             fun merkle_tree_pb:decode_fetchdonemsg/1).

hash_req_test() ->
    PosBin = <<234:9>>,
    Data = merkle_tree_pb_util:create_hash_req(PosBin),
    gen_test(Data,
             fun merkle_tree_pb:encode_hashreq/1,
             fun merkle_tree_pb:decode_hashreq/1).

hash_resp_test() ->
    PosBin = <<34:13>>,
    Hash = <<"a binary representing a hashed value">>,
    Data = merkle_tree_pb_util:create_hash_resp(PosBin, Hash),
    gen_test(Data,
             fun merkle_tree_pb:encode_hashresp/1,
             fun merkle_tree_pb:decode_hashresp/1).

error_msg_test() ->
    Code = 3,
    Msg = "Something went wrong",
    Data = merkle_tree_pb_util:create_error_msg(Code, Msg),
    gen_test(Data,
             fun merkle_tree_pb:encode_errormsg/1,
             fun merkle_tree_pb:decode_errormsg/1).

gen_test(Data, EncFun, DecFun) ->
    EncData = erlang:iolist_to_binary(EncFun(Data)),
    ?assertEqual(Data, DecFun(EncData)).
