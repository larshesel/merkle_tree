%%% @doc
%%% A collection of protobuff helper functions.
%%% @end
-module(merkle_tree_pb_util).
-include("merkle_tree_pb.hrl").

%% TODO: only export relevant functions
-compile(export_all).

-define(HANDSHAKE_REQ, 1).
-define(ACK_RESP, 2).
-define(FETCH_REQ, 3).
-define(LEAVES_RESP, 4).
-define(FETCHDONE_MSG,5).
-define(GETHASH_REQ, 6).
-define(HASH_RESP, 7).
-define(ERROR_MSG, 8).

create_merkle_msg(#handshakereq{} = Inner) when is_record(Inner, handshakereq) ->
    #merklemsg{type = 'HANDSHAKE_REQ', handshakereq = Inner};
create_merkle_msg(#ackresp{} = Inner) ->
    #merklemsg{type = 'ACK_RESP', ackresp = Inner};
create_merkle_msg(#fetchreq{} = Inner) ->
    #merklemsg{type = 'FETCH_REQ', fetchreq = Inner};
create_merkle_msg(#leavesresp{} = Inner) ->
    #merklemsg{type = 'LEAVES_RESP', leavesresp = Inner};
create_merkle_msg(#fetchdonemsg{} = Inner) ->
    #merklemsg{type = 'FETCHDONE_MSG', fetchdonemsg = Inner};
create_merkle_msg(#hashreq{} = Inner) ->
    #merklemsg{type = 'HASH_REQ', hashreq = Inner};
create_merkle_msg(#errormsg{} = Inner) ->
    #merklemsg{type = 'ERROR_MSG', errormsg = Inner}.

create_pair(K, V) ->
    #pair{key = K, val = V}.

create_handshake_req(Major, Minor, Opts) when is_list(Opts) ->
    #handshakereq{major_version = Major, minor_version = Minor, options = Opts}.

create_ack_resp() ->
    #ackresp{}.

create_fetch_req(PosBin) when is_bitstring(PosBin) ->
    #fetchreq{pos = pos(PosBin)}.

pos(PosBin) when is_bitstring(PosBin)->
    #pos{pos = bitstring_to_binary_pad(PosBin),
         num_bits = erlang:bit_size(PosBin)}.

bitstring_to_binary_pad(PosBin) ->
    NumBits = erlang:bit_size(PosBin),
    MissingBits = 8 - (NumBits rem 8),
    <<0:MissingBits, PosBin/bitstring>>.

create_leaf(PosBin, Data) ->
    #leaf{pos = pos(PosBin),
          data = Data}.

create_leaves_resp(Leaves) ->
    #leavesresp{leaves = Leaves}.

create_fetch_done_msg() ->
    #fetchdonemsg{}.

create_hash_req(PosBin) ->
    #hashreq{pos = pos(PosBin)}.

create_hash_resp(PosBin, Hash) ->
    #hashresp{pos = pos(PosBin), hash = Hash}.

create_error_msg(Code, Msg) ->
    #errormsg{code = Code, msg = Msg}.
