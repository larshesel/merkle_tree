-module(mtree_fetch).

-export([fetch/1]).

-include("mtree.hrl").

-spec fetch(pid()) ->
		   {ok, mtree()}.
fetch(Pid) ->
    %%{ok, fetch_tree(Pid, [], )},
    T = mtree:new(),
    {ok, fetch(Pid, T, <<>>)}.

fetch(Pid, T, Pos) ->
    {ok, {Type, Val}} = mtree_server:get_node_val(Pid, Pos),
    T1 = mtree:insert(T, Val, Pos),
    case Type of
	leaf ->
	    T1;
	inner ->
	    %% go left
	    TLeft = fetch(Pid, T1, <<Pos/bitstring, 0:1>>),
	    %% go right
	    fetch(Pid, TLeft, <<Pos/bitstring, 1:1>>)
    end.
