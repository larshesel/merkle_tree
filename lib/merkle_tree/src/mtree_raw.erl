-module(mtree_raw).

-export([build_tree/1, build_tree/2]).

-include("mtree.hrl").

-define(CHUNK_SIZE, 256).

-spec build_tree(binary()) -> mtree().
build_tree(Raw) ->
    build_tree(Raw, ?CHUNK_SIZE).

build_tree(Raw, ChunkSize) ->
    Leaves = mk_leaves(Raw, ChunkSize, []),
    mtree:build_tree_bottom_up(Leaves).

mk_leaves(Data, Size, Acc) ->
    case Data of
	<<Chunk:Size/binary, Rest/binary>> ->
	    mk_leaves(Rest, Size, [mtree:mk_leaf(Chunk) | Acc]);
	<<>> ->
	    lists:reverse(Acc);
	Rest ->
	    lists:reverse([mtree:mk_leaf(Rest) | Acc])
    end.

