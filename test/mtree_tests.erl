-module(mtree_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

create_mtree() ->
    SL = lists:seq(1,200),
    BinList = erlang:list_to_binary(lists:flatten([SL || _ <- lists:seq(1,142)])),
    mtree_raw:build_tree(BinList).

server_client_get_root_node_test() ->
    MTree = create_mtree(),
    {ok, ServerPid} = mtree_server:start_link(MTree, client_mock, self()),
    {ok, ServerRootNode} = mtree_server:get_node_val(ServerPid, <<>>),
    RealRoot = mtree:get_node_val(MTree, <<>>),
    RealRoot = ServerRootNode.

server_client_fetch_tree_test() ->
    MTree = create_mtree(),
    mtree:verify(MTree),
    {ok, ServerPid} = mtree_server:start_link(MTree, client_mock, self()),
    {ok, FetchedTree} = mtree_fetch:fetch(ServerPid),
    MTree = FetchedTree.

count_nodes_test() ->
    T = mtree:new(),
    T1 = mtree:insert(T, root, <<>>),
    ?assertEqual(1, util:count_nodes(T1)),
    ?assertEqual(1, util:count_leaves(T1)),

    T2 = mtree:insert(T1, left_leaf, <<0:1>>),
    %% the above insert will have forced the root node to have two
    %% children (the right has a nil value):
    ?assertEqual(3, util:count_nodes(T2)),
    ?assertEqual(2, util:count_leaves(T2)),

    T3 = mtree:insert(T1, right_leaf, <<1:1>>),
    %% the above insert will only have updated the value of the right
    %% child, not added anymore nodes to the tree.
    ?assertEqual(3, util:count_nodes(T3)),
    ?assertEqual(2, util:count_leaves(T3)).

mtree_new_test() ->
    ?assertEqual({leaf, nil}, mtree:new()).

mtree_create_root_test() ->
    T = mtree:new(),
    ?assertEqual({leaf, root}, mtree:insert(T, root, <<>>)).

verify_invalid_tree_test() ->
    Data = <<1,2,3,4>>,
    T = mtree_raw:build_tree(Data, 1),
    ?assertEqual(ok, mtree:verify(T)),
    InvalidTree = mtree:insert(T, <<27>>, <<0:1,1:1>>),
    {Pos, _ExHash, _ActHash} = mtree:verify(InvalidTree),
    ?assertEqual(<<0:1>>, Pos).

create_root_and_two_leaves_test() ->
    T = mtree:new(),
    T2 = mtree:insert(T, root, <<>>),
    T3 = mtree:insert(T2, left_leaf, <<0:1>>),
    T4 = mtree:insert(T3, right_leaf, <<1:1>>),
    ?assertEqual({inner, root, {leaf, left_leaf}, {leaf, right_leaf}}, T4).

triq_test_() ->
    {timeout, 60,
     fun() ->
             true = triq:module(?MODULE)
     end}.

prop_gen_one_node_tree() ->
    ?FORALL(
       Pos, list(oneof([l,r])),
       ?IMPLIES(Pos /= [],
		begin
		    BinPos = util:pos_to_bin(Pos),
		    T = mtree:new(),
		    Val = Pos,
		    T2 = mtree:insert(T, Val, BinPos),
		    {leaf, Val} == mtree:get_node_val(T2, BinPos)
		end)).
