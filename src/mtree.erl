-module(mtree).

-compile([export_all]).

-type data() :: any().
-type hash() :: binary().

-type mtree_node() :: {node, Left::mtree(), Left::mtree(), ChildHash::hash()}.
-type mtree_leaf() :: {leaf, DataHash::hash(), Data::data()}.
-type mtree() :: mtree_node()
	       | mtree_leaf().

fake_data() ->
    [<<"abe">>, <<"appelsin">>, <<"bold">>,
     <<"skatkiste">>, <<"xylofon">>, <<"foo">>].


build_tree() ->
    Leaves = lists:sort([ mk_leaf(hash(Data), Data) || Data <- fake_data() ]),
    build_tree(Leaves, []).

-spec build_tree([mtree()], [mtree()]) ->
			mtree().
build_tree([L, R| T], Acc) ->
    build_tree(T, [create_node(L,R) | Acc]);
build_tree([L], Acc) ->
    build_tree([], [L | Acc]);
build_tree([], [_, _ | _] = L) ->
    build_tree(L, []);
build_tree([], [Root]) ->
    Root.

build_tree([Single]) ->
    [Single].

-spec create_node(mtree(), mtree()) ->
			 mtree().
create_node({leaf, H1, _} = L, {leaf, H2, _} = R) ->
    {node, L, R, hash(<<H1/binary, H2/binary>>)};
create_node({leaf, H1, _} = L, {node, _LC, _RC, H2} = R)->
    {node, L, R, hash(<<H1/binary, H2/binary>>)};
create_node({node, _LC, _RC, H1} = L, {leaf, H2, _} = R) ->
    {node, L, R, hash(<<H1/binary, H2/binary>>)};
create_node({node, _, _, H1} = L, {node, _, _, H2} = R) ->
    {node, L, R, hash(<<H1/binary, H2/binary>>)}.

-spec mk_leaf(hash(), data()) ->
		     mtree_leaf().
mk_leaf(Hash, Data) ->
    {leaf, Hash, Data}.

-spec hash(Data::binary()) -> Digest::binary().
hash(Data) ->
    crypto:hash(sha256, Data).


