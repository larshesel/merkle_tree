-module(util).

-export([count_nodes/1, count_leaves/1]).
-export([dot_tree/1, dot_tree/2, pos_to_bin/1, bin_to_pos/1]).
-include("mtree.hrl").

dot_tree(Tree, IoDevice) ->
    io:format(IoDevice, "digraph merkeltree {~n"
	      "rankdir=\"LR\";"
	      "splines=true;", []),
    output_nodes(Tree, 0, IoDevice),
    io:format(IoDevice, "}~n", []),
    ok.

dot_tree(Tree) ->
    dot_tree(Tree, standard_io).

output_nodes({leaf, <<>>}, Num, IoDevice) ->
    io:format(IoDevice,
	      "~p [fontsize=14,"
	      "label=\"~w\","
	      "style=filled,"
	      "color=\".7 .3 1.0\","
	      "shape=box];~n", [Num, nil]),
    Num;
output_nodes({leaf, <<Head:1/binary, _/binary>>}, Num, IoDevice) ->
    io:format(IoDevice,
	      "~p [fontsize=14,"
	      "label=\"~w\","
	      "style=filled,"
	      "color=\".7 .3 1.0\","
	      "shape=box];~n", [Num, Head]),
    Num;
output_nodes({inner, <<Head:3/binary, _/binary>>, L, R}, Num, IoDevice) ->
    io:format(IoDevice, "~p [fontsize=14, label=\"~p\\n~w\"];~n", [Num, Num, Head]),
    io:format(IoDevice, "~p -> ~p;~n", [Num, Num + 1]),
    NewNum = output_nodes(R, Num + 1, IoDevice),
    io:format(IoDevice, "~p -> ~p;~n", [Num, NewNum + 1]),
    output_nodes(L, NewNum + 1, IoDevice).

bin_to_pos(Bin) ->
    bin_to_pos(Bin, []).

bin_to_pos(<<>>, Acc) ->
    lists:reverse(Acc);
bin_to_pos(<<0:1, Rest/bitstring>>, Acc) ->
    bin_to_pos(Rest, [l |Acc]);
bin_to_pos(<<1:1, Rest/bitstring>>, Acc) ->
    bin_to_pos(Rest, [r |Acc]).

pos_to_bin(Pos) ->
    pos_to_bin(Pos, <<>>).

pos_to_bin([], Bits) ->
    Bits;
pos_to_bin([l | T], Bits) ->
    pos_to_bin(T, <<Bits/bitstring, 0:1>>);
pos_to_bin([r | T], Bits) ->
    pos_to_bin(T, <<Bits/bitstring, 1:1>>).

-spec count_leaves(mtree()) -> integer().
count_leaves({leaf, _V}) ->
    1;
count_leaves({inner, _V, L, R}) ->
    count_leaves(L) + count_leaves(R).

-spec count_nodes(mtree()) -> integer().
count_nodes({leaf, _Val}) ->
    1;
count_nodes({inner, _Val, L, R}) ->
    1 + count_nodes(L) + count_nodes(R).

