-module(util).

-export([dot_tree/1]).

dot_tree(Tree) ->
    io:format("digraph merkeltree {~n", []),
    output_nodes(Tree, 0),
    io:format("}~n", []),
    ok.

output_nodes({leaf, _H, D}, Num) ->
    io:format("~p [label=\"~s\"];~n", [Num, D]),
    Num;
output_nodes({node, L, R, _H}, Num) ->
    io:format("~p -> ~p;~n", [Num, Num + 1]),
    NewNum = output_nodes(L, Num + 1),
    io:format("~p -> ~p;~n", [Num, NewNum + 1]),
    output_nodes(R, NewNum + 1).
