-module(mtree_raw).

-export([build_tree/1]).

-include("mtree.hrl").

-define(CHUNK_SIZE, 256).

-spec build_tree(binary()) -> mtree().
build_tree(Raw) ->
    Leaves = mk_leaves(Raw),
    mtree:build_tree_bottom_up(Leaves).

mk_leaves(Raw) ->
    mk_leaves(Raw, ?CHUNK_SIZE, []).

mk_leaves(Data, Size, Acc) ->
    case Data of
	<<Chunk:Size/binary, Rest/binary>> ->
	    mk_leaves(Rest, Size, [mtree:mk_leaf(Chunk) | Acc]);
	<<>> ->
	    lists:reverse(Acc);
	Rest ->
	    lists:reverse([mtree:mk_leaf(Rest) | Acc])
    end.

