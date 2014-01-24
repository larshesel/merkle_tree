-module(client_behaviour).

-include("mtree.hrl").

-callback sync_done(Pid::pid()) ->
    ok.

-callback send_leaf(Pid::pid(), Val::hash(), Pos::pos_bin()) ->
    ok.

