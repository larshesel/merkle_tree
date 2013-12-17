-module(mtree_tests).
-include_lib("eunit/include/eunit.hrl").

%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

create_mtree() ->
    SL = lists:seq(1,200),
    BinList = erlang:list_to_binary(lists:flatten([SL || _ <- lists:seq(1,142)])),
    mtree_raw:build_tree(BinList).

server_client_get_root_node_test() ->
    MTree = create_mtree(),
    {ok, ServerPid} = mtree_server:start_link(MTree),
    {ok, ServerRootNode} = mtree_server:get_node_val(ServerPid, []),
    RealRoot = mtree:get_node_val(MTree, []),
    RealRoot = ServerRootNode.

%% server_client_sync_test() ->
%%     MTree = create_mtree(),
%%     ServerPid = mtree_server:start_link(MTree),
%%     ClientPid = mtree_client:start_link(ServerPid),
%%     mtree_client:start_sync(ClientPid),
%%     ok = wait_for_finish(ClientPid).

%% wait_for_finish(ClientPid) ->
%%     receive
%% 	{sync_ok, ClientPid} ->
%% 	    ok;
%% 	{error, ClientPid, Reason} ->
%% 	    {error, Reason}
%%     end.

