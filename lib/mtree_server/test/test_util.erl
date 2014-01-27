-module(test_util).
-export([create_rand_file/1, rand_file_name/0, tmp_file/1, sum_file/1]).

%% TODO: get rid of these OS dependecies: cut, sha1sum...
-define(SUM_EXEC, "sha1sum").
-define(CUT_EXEC, "cut").

sum_file(File) ->
    os:cmd(?SUM_EXEC " " ++ File ++ " | " ?CUT_EXEC " -d' ' -f1").

create_rand_file(Size) ->
    Name = tmp_file(rand_file_name()),
    Data = [X rem 256 || X <- lists:seq(1,Size)],
    ok = file:write_file(Name, Data),
    Name.

tmp_dir() ->
    Dir = code:lib_dir(mtree_server) ++ "/test/tmp/",
    filelib:ensure_dir(Dir),
    Dir.

tmp_file(Name) ->
    tmp_dir() ++ Name.

rand_file_name() ->
    {A,B,C}=now(),
    N=node(),
    lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C])).
