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

-record(state, {sock}).

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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


