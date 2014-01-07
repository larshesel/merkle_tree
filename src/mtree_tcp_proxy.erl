-module(mtree_tcp_proxy).

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
    case build_frame(State) of
	{complete, FrameData, State1} ->
	    State2 = handle_frame(FrameData, State1),
	    maybe_handle_frame(State2);
	{incomplete, State1} ->
	    State1
    end.

-spec build_frame(state()) -> {complete, binary(), state()}
				  | {incomplete, state()}.
build_frame(#state{incomplete_frame =
		       <<FrameLen:(?FRAME_HDR_LEN), Frame/binary>>} = State) ->
    case Frame of
	<<CompleteFrame:FrameLen/binary, NextFrame/binary>> ->
	    {complete, CompleteFrame, State#state{incomplete_frame = NextFrame}};
	 _ ->
	    error_logger:info_msg("Still incomplete: FrameLen: ~p~n", [byte_size(Frame)]),
	    error_logger:info_msg("Need ~p bytes~n", [FrameLen]), 
	    {incomplete, State}
    end;
build_frame(State) ->
    {incomplete, State}.

add_to_incoming_frame(#state{incomplete_frame = Inc} = State, Data) ->
    State#state{ incomplete_frame = <<Inc/binary, Data/binary>> }.

handle_frame(_FrameData, State) ->
    State.
