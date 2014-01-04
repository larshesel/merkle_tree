-module(mtree_file).

-export([build_tree/1]).
-include("mtree.hrl").

%% A simple but inefficient way to build a merkle tree over a file.
-spec build_tree(string()) -> mtree().
build_tree(Filename) ->
    {ok, BinData} = file:read_file(Filename),
    mtree_raw:build_tree(BinData, 4096).
