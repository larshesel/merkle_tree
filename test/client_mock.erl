-module(client_mock).
-export([sync_done/1, send_leaf/3]).

-behaviour(client_behaviour).

sync_done(ClientPid) ->
    ClientPid ! {sync_done, self()},
    ok.

send_leaf(ClientPid, Val, Pos) ->
    ClientPid ! {leaf, Val, Pos},
    ok.
