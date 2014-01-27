-module(mtree_graph_util).

-export([write_dot_file/2]).

write_dot_file(Tree, Filename) ->
    {ok, IoDevice} = file:open(Filename, [write]),
    util:dot_tree(Tree, IoDevice),
    ok = file:close(IoDevice).
