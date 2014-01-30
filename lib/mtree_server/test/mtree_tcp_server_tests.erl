-module(mtree_tcp_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mtree_core/include/merkle_tree_pb.hrl").

tcp_handshake_test() ->
    F = fun(Sock, Treename) ->
                do_hs(Sock, [{<<"single_tree">>, Treename}])
        end,
    with_def_server(F).

tcp_fetch_test() ->
    F = fun(Sock, Treename) ->
                do_hs(Sock, [{<<"single_tree">>, Treename}]),
                do_fetch_req(Sock, <<>>)
        end,
    with_def_server(F).

tcp_hash_req_test() ->
    F = fun(Sock, Treename) ->
                do_hs(Sock, [{<<"single_tree">>, Treename}]),
                do_hash_req(Sock, <<>>)
        end,
    with_def_server(F).

with_def_server(TestFun) ->
    {Filename, Treename, _IpAddr, _Port, Sock} = default_mtree_server(),
    TestFun(Sock, Treename),
    default_mtree_server_teardown(Sock, Filename).

default_mtree_server_teardown(Sock, Filename) ->
    ok = gen_tcp:close(Sock),
    ?cmd("rm " ++ Filename),
    application:stop(mtree_server).


do_fetch_req(Sock, Pos) ->
    GHMsg = merkle_tree_pb_util:create_fetch_req(Pos),
    MMsg = merkle_tree_pb_util:create_merkle_msg(GHMsg),
    EMsg = merkle_tree_pb:encode_merklemsg(MMsg),
    ok = gen_tcp:send(Sock, EMsg),

    {ok, Data} = gen_tcp:recv(Sock, 0, 1000),
    RMMsg = merkle_tree_pb:decode_merklemsg(Data),

    [ ?_assertMatch(#merklemsg{type='HASH_RESP', hashresp=#hashresp{}}, RMMsg) ].

do_hash_req(Sock, Pos) ->
    GHMsg = merkle_tree_pb_util:create_hash_req(Pos),
    MMsg = merkle_tree_pb_util:create_merkle_msg(GHMsg),
    EMsg = merkle_tree_pb:encode_merklemsg(MMsg),
    ok = gen_tcp:send(Sock, EMsg),

    {ok, Data} = gen_tcp:recv(Sock, 0, 1000),
    RMMsg = merkle_tree_pb:decode_merklemsg(Data),

    [ ?_assertMatch(#merklemsg{type='HASH_RESP', hashresp=#hashresp{}}, RMMsg) ].


do_hs(Sock, Props) ->
    Opts = [ merkle_tree_pb_util:create_pair(K, V) || {K,V} <- Props ],
    HSMsg = merkle_tree_pb_util:create_handshake_req(1,0, Opts),
    MMsg = merkle_tree_pb_util:create_merkle_msg(HSMsg),
    EMsg = merkle_tree_pb:encode_merklemsg(MMsg),
    ok = gen_tcp:send(Sock, EMsg),

    {ok, Data} = gen_tcp:recv(Sock, 0, 1000),
    RMMsg = merkle_tree_pb:decode_merklemsg(Data),

    ?assertMatch(#merklemsg{type='ACK_RESP', ackresp=#ackresp{}}, RMMsg).

default_mtree_server() ->
    %% create random file
    Filename = test_util:create_rand_file(130000),
    Tree = mtree_file:build_tree(Filename, 4096),

    %% configure application
    Port = 4567,
    IpAddr = "127.0.0.1",
    Treename = <<"name">>,
    mtree_app:set_env(single_tree, {Treename, Tree}),
    mtree_app:set_env(port, Port),
    mtree_app:set_env(ip_addr, IpAddr),

    %% start_app
    ok = mtree_app:start(),

    {ok, Sock} = gen_tcp:connect(IpAddr, Port, [binary, {packet, 4}, {active, false}]),
    {Filename, Treename, IpAddr, Port, Sock}.





%% tcp_server_handshake_test_() ->
%%     {foreach,
%%      %% setup
%%      fun() ->
%%              {_Filename, _Treename, _IpAddr, _Port, _Sock} = default_mtree_server()
%%      end,
%%      %% teardown
%%      fun({Filename, _Treename, _IpAddr, _Port, Sock}) ->
%%              ok = gen_tcp:close(Sock),
%% 	     ?cmd("rm " ++ Filename),
%%              application:stop(mtree_server)
%%      end,
%%      %% test
%%      [
%%       fun({_, Treename, _, _, Sock}) -> [do_hs(Sock, [{<<"single_tree">>, Treename}])] end,
%%       fun({_, Treename, _, _, Sock}) -> [do_hs(Sock, [{<<"single_tree">>, Treename}]),
%%                                          do_hash_req(Sock, <<>>)] end,
%%       fun({_, Treename, _, _, Sock}) -> [do_hs(Sock, [{<<"single_tree">>, Treename}]),
%%                                          do_fetch_req(Sock, <<>>),
%%                                          do_hash_req(Sock, <<>>)]
%%      ]
%%     }.
