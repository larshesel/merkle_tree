-module(mtree).

-export([build_tree_bottom_up/1, get_subtree/2, mk_leaf/1,
	 get_node_val/2, traverse_preorder/3]).

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
    {inner, Val, insert({leaf, nil}, LVal, Pos), {leaf, nil}};
insert({leaf, Val}, RVal, <<1:1, Pos/bitstring>>) ->
    %% upgrade leaf to node and recurse right
    {inner, Val, {leaf, nil}, insert({leaf, nil}, RVal, Pos)};
insert({inner, _Val, L, R}, Val, <<>>) ->
    %% replace node val
    {inner, Val, L, R};
insert({inner, Val, L, R}, LVal, <<0:1, Pos/bitstring>>) ->
    {inner, Val, insert(L, LVal, Pos), R};
insert({inner, Val, L, R}, RVal, <<1:1, Pos/bitstring>>) ->
    {inner, Val, L, insert(R, RVal, Pos)}.

-spec get_subtree(T :: mtree(), pos_bin()) ->
			 mtree() | {error, invalid_pos}.
get_subtree({inner, _Val, _L,_R} = N, <<>>) ->
    N;
get_subtree({leaf, _Val} = N, <<>>) ->
    N;
get_subtree({inner, _Val, L, _R}, <<0:1, T/bitstring>>) ->
    get_subtree(L, T);
get_subtree({inner, _Val, _L, R}, <<1:1, T/bitstring>>) ->
    get_subtree(R, T);
get_subtree(_, _) ->
    {error, invalid_pos}.

-spec get_node_val(T :: mtree(), pos_bin()) ->
			  {leaf, val()}
			      | {inner, val()}
			      | {error, invalid_pos}.
get_node_val(T, Pos) ->
    case get_subtree(T, Pos) of
	{inner, Val, _L, _R} ->
	    {inner, Val};
	{leaf, Val} ->
	    {leaf, Val};
	{error, invalid_pos} ->
	    {error, invalid_pos}
    end.

-spec verify(mtree()) -> ok | {pos_bin(), hash(), hash()}.
verify(T) ->
    try
	verify(T, <<>>),
	ok
    catch Ex ->
	    Ex
    end.

verify({inner, Val, L, R}, Pos) ->
    VL = verify(L, <<Pos/bitstring, 0:1>>),
    VR = verify(R, <<Pos/bitstring, 1:1>>),
    ActualHash = hash(<<VL/binary, VR/binary>>),

    if ActualHash =/= Val ->
	    throw({Pos, Val, ActualHash});
       true ->
	    ActualHash
    end;
verify({leaf, Val}, _Pos) ->
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
    {inner, hash(<<D1/binary, D2/binary>>), L, R};
create_node({leaf, D1} = L, {inner, K2, _LC, _RC} = R)->
    {inner, hash(<<D1/binary, K2/binary>>), L, R};
create_node({inner, K1, _LC, _RC} = L, {leaf, D2} = R) ->
    {inner, hash(<<K1/binary, D2/binary>>), L, R};
create_node({inner, K1, _, _} = L, {inner, K2, _, _} = R) ->
    {inner, hash(<<K1/binary, K2/binary>>), L, R}.

-spec mk_leaf(val()) ->
		     mtree_leaf().
mk_leaf(Data) ->
    {leaf, Data}.

-spec hash(Data::binary()) -> Digest::hash().
hash(Data) ->
    crypto:hash(sha256, Data).

-spec traverse_preorder(mtree(), fun(), pos_bin()) -> ok.
traverse_preorder({leaf, _} = N, F, Pos) ->
    F(N, Pos);
traverse_preorder({inner, _, L, R} = N, F, Pos) ->
    F(N, Pos),
    traverse_preorder(L, F, <<Pos/bitstring, 0:1>>),
    traverse_preorder(R, F, <<Pos/bitstring, 1:1>>).


