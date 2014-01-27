-module(mtree_server_tests).
-include_lib("eunit/include/eunit.hrl").

create_mtree() ->
    SL = lists:seq(1,200),
    BinList = erlang:list_to_binary(lists:flatten([SL || _ <- lists:seq(1,142)])),
    mtree_raw:build_tree(BinList).

server_client_get_root_node_test() ->
    MTree = create_mtree(),
    %% pass 'undefined' as client module - server should not use it
    %% when using get_node_val
    {ok, ServerPid} = mtree_server:start_link(MTree, undefined, self()),
    {ok, ServerRootNode} = mtree_server:get_node_val(ServerPid, <<>>),
    RealRoot = mtree:get_node_val(MTree, <<>>),
    RealRoot = ServerRootNode.

server_client_fetch_tree_test() ->
    MTree = create_mtree(),
    mtree:verify(MTree),
    %% pass 'undefined' as client module - server should not use it
    %% when using fetch
    {ok, ServerPid} = mtree_server:start_link(MTree, undefined, self()),
    {ok, FetchedTree} = mtree_fetch:fetch(ServerPid),
    MTree = FetchedTree.

start_server_and_sync_file_test_() ->
    {setup,
     %% setup
     fun() ->
	     Name = test_util:create_rand_file(130000),
	     Tree = mtree_file:build_tree(Name, 4096),
	     {ok, Pid} = mtree_server:start_link(Tree, client_mock, self()),
	     {Pid, Name}
     end,
     %% teardown
     fun({Pid, Name}) ->
	     gen_server:call(Pid, stop),
	     ?cmd("rm " ++ Name)
     end,
     %% test
     fun({Pid, Name}) ->
	     {ok, Tree} = mtree_fetch:fetch(Pid),
	     NewFile = test_util:tmp_file(test_util:rand_file_name()),
	     ok = mtree_file:write(Tree, NewFile),
	     ExpectedSum = test_util:sum_file(Name),
	     ActualSum = test_util:sum_file(NewFile),
	     ?cmd("rm " ++ NewFile),
	     [ ?_assertEqual(ExpectedSum, ActualSum) ]
     end}.

start_server_and_async_file_test_() ->
    {setup,
     %% setup
     fun() ->
	     Name = test_util:create_rand_file(130000),
	     Tree = mtree_file:build_tree(Name, 4096),
	     ClientPid = self(),
	     {ok, ServerPid} = mtree_server:start_link(Tree, client_mock, ClientPid),
	     {ServerPid, Tree, ClientPid, Name}
     end,
     %% teardown
     fun({ServerPid, _ServerTree, _ClientPid, Name}) ->
	     gen_server:call(ServerPid, stop),
	     ?cmd("rm " ++ Name)
     end,
     %% test
     fun({ServerPid, _ServerTree, _ClientPid, Name}) ->
	     ok = mtree_server:start_sync(ServerPid, <<>>),
	     Tree = until_sync_done(ServerPid, mtree:new()),

	     NewFile = test_util:tmp_file(test_util:rand_file_name()),
	     ok = mtree_file:write(Tree, NewFile),
	     ExpectedSum = test_util:sum_file(Name),
	     ActualSum = test_util:sum_file(NewFile),
	     ?cmd("rm " ++ NewFile),
	     [ ?_assertEqual(ExpectedSum, ActualSum) ]
     end}.


until_sync_done(ServerPid, Tree) ->
    receive
	{sync_done, ServerPid} ->
	    %%io:format(user, "Received: sync done~n", []),
	    Tree;
	{leaf, Val, Pos} ->
	    %%io:format(user, "Received leaf at pos: ~p~n", [util:bin_to_pos(Pos)]),
	    Tree1 = mtree:insert(Tree, Val, Pos),
	    until_sync_done(ServerPid, Tree1)
    after
	1000 ->
	    error({sync_failed, timeout})
    end.

