-module(mtree_server).

-behaviour(gen_server).

-include("mtree.hrl").

%% API
-export([start_link/1]).

-export([get_node_val/2]).

%% protocol support
-export([start_sync/2, set_client/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mtree=undefined, client=undefined }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start an asynchronous fetch of the subtree given by Pos.
%% 
-spec start_sync(pid(), pos_bin()) -> ok.
start_sync(Pid, Pos) ->
    gen_server:cast(Pid, {start_sync, Pos}).

-spec set_client(pid(), pid()) -> ok | {error, wont_set_my_own_pid}.
set_client(ServerPid, ClientPid) ->
    gen_server:call(ServerPid, {set_client, ClientPid}).

-spec get_node_val(pid(), pos_bin()) ->
			  {ok, any()}.
get_node_val(Pid, Pos) ->
    gen_server:call(Pid, {get_node_val, Pos}).

start_link(MerkleTree) ->
    gen_server:start_link(?MODULE, [MerkleTree], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MerkleTree]) ->
    {ok, #state{mtree=MerkleTree}}.


handle_call({set_client, Pid}, _From, S) ->
    if self() =:= Pid ->
	    {reply, {error, wont_set_my_own_pid}, S};
       true ->
	    {reply, ok, S#state{client = Pid}}
    end;
handle_call({get_node_val, Pos}, _From, S = #state{mtree = T}) ->
    Node = mtree:get_node_val(T, Pos),
    {reply, {ok, Node}, S};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    error_logger:info_msg("Got unknown msg: ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start_sync, Pos}, S=#state{client=ClientPid, mtree=Root}) ->
    STree= mtree:get_subtree(Root, Pos),
    send_leaves(ClientPid, STree),
    {noreply, S};
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
send_leaves(ClientPid, Tree) ->
    mtree:traverse_preorder(Tree, fun(N, Pos) ->
				    send_leaf(ClientPid, N, Pos)
			    end,
		      <<>>),
    send_sync_done(ClientPid).

send_leaf(ClientPid, {leaf, Val}, Pos) ->
    ClientPid ! {leaf, Val, Pos};
send_leaf(_, _, _) ->
    ok.


send_sync_done(ClientPid) ->
    ClientPid ! {sync_done, self()}.
