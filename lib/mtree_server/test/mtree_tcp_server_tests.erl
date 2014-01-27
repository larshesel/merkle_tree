-module(mtree_tcp_server_tests).
-include_lib("eunit/include/eunit.hrl").


tcp_server_handshake_test_() ->
    {setup,
     %% setup
     fun() ->
             %% create random file
	     Filename = test_util:create_rand_file(130000),
	     Tree = mtree_file:build_tree(Filename, 4096),

             %% configure application

             Port = 4567,
             IpAddr = "127.0.0.1",
             mtree_app:set_env(single_tree, {<<"name">>, Tree}),
             mtree_app:set_env(port, Port),
             mtree_app:set_env(ip_addr, IpAddr),

             %% start_app
             ok = application:ensure_started(mtree_server),

             {Filename, IpAddr, Port}
     end,
     %% teardown
     fun({Filename, IpAddr, Port}) ->
	     ?cmd("rm " ++ Filename),
             application:stop(mtree_server)
     end,
     %% test
     fun({Filename, IpAddr, Port}) ->
             {ok, Sock} = gen_tcp:connect(IpAddr, Port, [binary, {packet, 4}, {active, false}]),
             HSMsg = merkle_tree_pb_util:create_handshake_req(1,0, []),
             MMsg = merkle_tree_pb_util:create_merkle_msg(HSMsg),
             EMsg = merkle_tree_pb:encode_merklemsg(MMsg),
             ok = gen_tcp:send(Sock, EMsg),
             error_logger:info_msg("send data~n", []),
             {ok, Data} = gen_tcp:recv(Sock, 0, 1000),
             io:format(user, "Recvd data: ~p~n", [Data]),
             RMMsg = merkle_tree_pb:decode_merklemsg(Data),
             io:format(user, "Recvd merkle msg: ~p~n", [RMMsg]),
	     [ ?_assertEqual(true, true) ]
     end}.

create_mtree() ->
    SL = lists:seq(1,200),
    BinList = erlang:list_to_binary(lists:flatten([SL || _ <- lists:seq(1,142)])),
    mtree_raw:build_tree(BinList).

