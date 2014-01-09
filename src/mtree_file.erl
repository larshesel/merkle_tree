-module(mtree_file).

-export([build_tree/1, build_tree/2, write/2]).
-include("mtree.hrl").

%% A simple but inefficient way to build a merkle tree over a file.
-spec build_tree(string()) -> mtree().
build_tree(Filename) ->
    build_tree(Filename, 4096).

-spec build_tree(string(), integer()) -> mtree().
build_tree(Filename, ChunkSize) ->
    {ok, BinData} = file:read_file(Filename),
    mtree_raw:build_tree(BinData, ChunkSize).

write(Tree, Name) ->
    {ok, IoDev} = file:open(Name, [write, binary]),
    write_to_handle(Tree, IoDev),
    ok = file:close(IoDev).

write_to_handle({leaf, Data}, IoDev) ->
    file:write(IoDev, Data);
write_to_handle({inner, _, L, R}, IoDev) ->
    write_to_handle(L, IoDev),
    write_to_handle(R, IoDev).

