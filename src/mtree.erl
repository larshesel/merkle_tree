-module(mtree).

-export([build_tree_bottom_up/1, mk_leaf/1, get_node_val/2]).

-export([new/0, insert/3, verify/1]).

-include("mtree.hrl").

%% build tree node, by node.

new() ->
    {leaf, nil}.

-spec insert(mtree(), val(), pos_bin()) ->
		    mtree().
insert({leaf, _Val}, Val, <<>>) ->
    %% replace leaf val.
    {leaf, Val};
insert({leaf, Val}, LVal, <<0:1, Pos/bitstring>>) ->
    %% upgrade leaf to node and recurse left
    {node, Val, insert({leaf, nil}, LVal, Pos), {leaf, nil}};
insert({leaf, Val}, RVal, <<1:1, Pos/bitstring>>) ->
    %% upgrade leaf to node and recurse right
    {node, Val, {leaf, nil}, insert({leaf, nil}, RVal, Pos)};
insert({node, _Val, L, R}, Val, <<>>) ->
    %% replace node val
    {node, Val, L, R};
insert({node, Val, L, R}, LVal, <<0:1, Pos/bitstring>>) ->
    {node, Val, insert(L, LVal, Pos), R};
insert({node, Val, L, R}, RVal, <<1:1, Pos/bitstring>>) ->
    {node, Val, L, insert(R, RVal, Pos)}.

-spec get_node_val(T :: mtree(), pos_bin()) ->
			  {leaf, val()} | {node, val()}.
get_node_val({node, Val, _L,_R}, <<>>) ->
    {node, Val};
get_node_val({node, _Val, L, _R}, <<0:1, T/bitstring>>) ->
    get_node_val(L, T);
get_node_val({node, _Val, _L, R}, <<1:1, T/bitstring>>) ->
    get_node_val(R, T);
get_node_val({leaf, Val}, <<>>) ->
    {leaf, Val}.


verify({node, Val, L, R}) ->
    VL = verify(L),
    VR = verify(R),
    Hash = hash(<<VL/binary, VR/binary>>),
    Val = Hash;
verify({leaf, Val}) ->
    Val.

-spec build_tree_bottom_up([mtree_leaf()]) -> mtree().
build_tree_bottom_up(Leaves) ->
    BottomNodes = lists:reverse(build_leaves(Leaves, [])),
    build_tree(BottomNodes, []).


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


