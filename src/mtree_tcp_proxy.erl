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

-behaviour(gen_server).

%% API
-export([start_link/1, start_supervised/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock, incomplete_frame = <<>>}).
-type state() :: #state{}.

-define(FRAME_HDR_LEN, 3*8).
%%-define(FRAME_MAX_SIZE, 16#ffffff).

%%%===================================================================
%%% API
%%%===================================================================
start_supervised(Sock) ->
    mtree_proxy_sup:add_child([Sock]).


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
	    State2 = handle_packet(Packet, State1),
	    maybe_handle_frame(State2);
	{incomplete, State1} ->
	    State1
    end.

-spec build_packet(state()) -> {complete, binary(), state()}
				  | {incomplete, state()}.
build_packet(#state{incomplete_frame =
		       <<FrameLen:(?FRAME_HDR_LEN), Frame/binary>>} = State) ->
    case Frame of
	<<CompleteFrame:FrameLen/binary, NextFrame/binary>> ->
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

handle_packet(_FrameData, State) ->
    State.

%% testing
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
