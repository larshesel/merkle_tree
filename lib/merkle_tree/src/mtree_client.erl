-module(mtree_client).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([start_sync/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("mtree_core/include/mtree.hrl").

-type sync_state() :: idle | syncing.

-record(state, {server=undefined     :: pid() | undefined,
		mtree=undefined      :: mtree() | undefined,
		onbehalfof=undefined :: pid() | undefined,
		sync_state=idle      :: sync_state()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerPid) ->
    gen_server:start_link(?MODULE, [ServerPid], []).

start_sync(OnBehalfOf, ClientPid) ->
    gen_server:call(ClientPid, {start_sync, OnBehalfOf}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerPid]) ->
    {ok, #state{server=ServerPid}}.

handle_call({start_sync, OnBehalfOf}, _From, S) ->
    
    NewState = S#state{onbehalfof = OnBehalfOf},
    {reply, ok, NewState}.

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
