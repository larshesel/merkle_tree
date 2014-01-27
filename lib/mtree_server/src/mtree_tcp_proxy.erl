-module(mtree_tcp_proxy).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% This module receives the incoming client requests coming in over
%% the tcp connection, decodes them, executes them against the
%% mtree_server instance and forwards responses from the mtree_server
%% instance.
%%
%% Client <- tcp -> mtree_tcp_proxy <- erlang -> mtree_server.
-include_lib("mtree_core/include/merkle_tree_pb.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1, start_supervised/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock, incomplete_frame = <<>>, server_pid}).
-type state() :: #state{}.

-define(FRAME_HDR_LEN, 3*8). %% bits
%%-define(FRAME_MAX_SIZE, 16#ffffff).

%%%===================================================================
%%% API
%%%===================================================================
start_supervised(Sock) ->
    mtree_proxy_sup:add_child(Sock).


start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Sock]) ->
    {ok, #state{sock=Sock}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Sock}, State) ->
    error_logger:info_msg("sock closed: ~p~n", [Sock]),
    {stop, normal, State};

handle_info({tcp_error, _Socket, _}, State) ->
    {stop, normal, State};

handle_info({tcp, Sock, Data}, State) ->
    State1 = add_to_incoming_frame(State, Data),
    State2 = maybe_handle_frame(State1),
    set_active(Sock),
    {noreply, State2};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_active(Sock) ->
    inet:setopts(Sock, [{active,once}]).


-spec maybe_handle_frame(state()) -> state().
maybe_handle_frame(State) ->
    case build_packet(State) of
	{complete, Packet, State1} ->
            case handle_packet(Packet, State1) of
                {error, Reason} ->
                    error_logger:info_msg("Error while handling packet, shutting down. Reason: ~p~n", [Reason]),
                    {stop, normal, Reason, State};
                State2 ->
                    maybe_handle_frame(State2)
            end;
	{incomplete, State1} ->
	    State1
    end.

-spec build_packet(state()) -> {complete, binary(), state()}
				  | {incomplete, state()}.
build_packet(#state{incomplete_frame =
		       <<FrameLen:(?FRAME_HDR_LEN), Frame/binary>>} = State) ->
    case Frame of
	<<CompleteFrame:FrameLen/binary, NextFrame/binary>> ->
            error_logger:info_msg("Received complete frame!", []),
	    {complete, CompleteFrame, State#state{incomplete_frame = NextFrame}};
	 _ ->
	    error_logger:info_msg("Still incomplete: FrameLen: ~p~n", [byte_size(Frame)]),
	    error_logger:info_msg("Need ~p bytes~n", [FrameLen]),
	    {incomplete, State}
    end;
build_packet(State) ->
    {incomplete, State}.

add_to_incoming_frame(#state{incomplete_frame = Inc} = State, Data) ->
    State#state{ incomplete_frame = <<Inc/binary, Data/binary>> }.

handle_packet(FrameData, State) ->
    Msg = merkle_tree_pb:decode_merklemsg(FrameData),
    handle_merklemsg(Msg, State).

-define(MAJOR, 1).
-define(MINOR, 0).


handle_merklemsg(#merklemsg{type = 'HANDSHAKE_REQ', handshakereq = HSReq}, S) ->
    case handle_handshake(HSReq, S) of
        {ok, S} ->
            send(ack, S);
        {error, {wrong_version, supports, ?MAJOR, ?MINOR}, S} ->
            S = send({error, 0, "wrong version"}, S),
            {stop, normal, ok, S}
    end;
handle_merklemsg(_Msg, S) ->
    send({error, 0, "not implemented"}, S).

handle_handshake(#handshakereq{major_version = ?MAJOR,
                               minor_version = ?MINOR,
                               options = Opts}, S) ->
    %% TODO add authentication and authorization here, based on Opts.
    case proplists:get_value(single_tree, Opts) of
        Name ->
            %% TODO, create a tree builder - based on the spec from the
            %% app config.
            case application:get_env(mtree_server, single_tree) of
                {ok, {Name, Tree}} ->
                    ServerPid = mtree_server_sup:add_child(Tree, Opts),
                    {ok, S#state{server_pid = ServerPid}};
                X ->
                    {error, {wrong_version, supports, ?MAJOR, ?MINOR}, S}
            end
    end;
handle_handshake(_, S) ->
    {error, {wrong_version, supports, ?MAJOR, ?MINOR}, S}.


send({error, Code, Msg}, #state{sock=Sock} = S) ->
    ErrMsg = merkle_tree_pb_util:create_error_msg(Code, Msg),
    MMsg = merkle_tree_pb_util:create_merkle_msg(ErrMsg),
    EMsg = erlang:iolist_to_binary(merkle_tree_pb:encode_merklemsg(MMsg)),
    ok = gen_tcp:send(Sock, EMsg),
    S.

start_mtree_server() ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% testing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

fake_frame_data(FrameData) ->
    #state{incomplete_frame = FrameData}.

exact_frame_test() ->
    S = fake_frame_data(<<0,0,3,1,2,3>>),
    Frame = build_packet(S),
    ?assertMatch({complete, <<1,2,3>>, #state{incomplete_frame = <<>>}}, Frame).

incomplete_frame_test() ->
    S = fake_frame_data(<<0,0,3,1,2>>),
    Frame = build_packet(S),
    ?assertMatch({incomplete, #state{incomplete_frame = <<0,0,3,1,2>>}}, Frame).

more_than_one_frame_test() ->
    F1 = <<0,0,3,1,2,3>>,
    F2 = <<0,0,2,3,4>>,
    S = fake_frame_data(<<F1/binary, F2/binary>>),
    Frame = build_packet(S),

    ?assertMatch({complete, <<1,2,3>>, #state{incomplete_frame = <<0,0,2,3,4>>}}, Frame),
    {_, _, S2} = Frame,

    Frame2 = build_packet(S2),

    ?assertMatch({complete, <<3,4>>, #state{incomplete_frame = <<>>}}, Frame2).

-endif.
