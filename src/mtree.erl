-module(mtree).

-export([build_tree/1, build_tree_bottom_up/1, mk_leaf/1]).

-include("mtree.hrl").

-spec build_tree_bottom_up([mtree_leaf()]) -> mtree().
build_tree_bottom_up(Leaves) ->
    BottomNodes = lists:reverse(build_leaves(Leaves, [])),
    build_tree(BottomNodes, []).

build_tree(LeafData) ->
    Leaves = lists:sort([ mk_leaf(Data) || Data <- LeafData ]),
    build_tree(Leaves, []).


build_leaves([L, R | T], Acc) ->
    build_leaves(T, [create_node(L, R) | Acc]);
build_leaves([L], Acc) ->
    [L | Acc];
build_leaves([], Acc) ->
    Acc.


-spec build_tree([mtree()], [mtree()]) ->
			mtree().
build_tree([L, R| T], Acc) ->
    build_tree(T, [create_node(L,R) | Acc]);
build_tree([L], Acc) ->
    build_tree([], [L | Acc]);
build_tree([], [_, _ | _] = L) ->
    build_tree(lists:reverse(L), []);
build_tree([], [Root]) ->
    Root.


-spec create_node(mtree(), mtree()) ->
			 mtree().
create_node({leaf, D1} = L, {leaf, D2} = R) ->
    {node, hash(<<D1/binary, D2/binary>>), L, R};
create_node({leaf, D1} = L, {node, K2, _LC, _RC} = R)->
    {node, hash(<<D1/binary, K2/binary>>), L, R};
create_node({node, K1, _LC, _RC} = L, {leaf, D2} = R) ->
    {node, hash(<<K1/binary, D2/binary>>), L, R};
create_node({node, K1, _, _} = L, {node, K2, _, _} = R) ->
    {node, hash(<<K1/binary, K2/binary>>), L, R}.

-spec mk_leaf(val()) ->
		     mtree_leaf().
mk_leaf(Data) ->
    {leaf, Data}.

-spec hash(Data::binary()) -> Digest::hash().
hash(Data) ->
    crypto:hash(sha256, Data).


