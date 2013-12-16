-module(util).

-export([dot_tree/1]).

dot_tree(Tree) ->
    io:format("digraph merkeltree {~n"
	      "rankdir=\"LR\";"
	      "splines=true;", []),
    output_nodes(Tree, 0),
    io:format("}~n", []),
    ok.

output_nodes({leaf, <<>>}, Num) ->
    io:format("~p [fontsize=14,"
	      "label=\"~w\","
	      "style=filled,"
	      "color=\".7 .3 1.0\","
	      "shape=box];~n", [Num, nil]),
    Num;
output_nodes({leaf, <<Head:1/binary, _/binary>>}, Num) ->
    io:format("~p [fontsize=14,"
	      "label=\"~w\","
	      "style=filled,"
	      "color=\".7 .3 1.0\","
	      "shape=box];~n", [Num, Head]),
    Num;
output_nodes({node, <<Head:3/binary, _/binary>>, L, R}, Num) ->
    io:format("~p [fontsize=14, label=\"~p\\n~w\"];~n", [Num, Num, Head]),
    io:format("~p -> ~p;~n", [Num, Num + 1]),
    NewNum = output_nodes(R, Num + 1),
    io:format("~p -> ~p;~n", [Num, NewNum + 1]),
    output_nodes(L, NewNum + 1).
