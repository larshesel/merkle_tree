-module(mtree_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([get_node_val/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mtree=undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MerkleTree) ->
    gen_server:start_link(?MODULE, [MerkleTree], []).

-spec get_node_val(pid(), bitstring()) ->
			  {ok, any()}.
get_node_val(Pid, Pos) ->
    gen_server:call(Pid, {get_node_val, Pos}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MerkleTree]) ->
    {ok, #state{mtree=MerkleTree}}.

handle_call({get_node_val, Pos}, _From, S = #state{mtree = T}) ->
    Node = mtree:get_node_val(T, Pos),
    {reply, {ok, Node}, S};
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
