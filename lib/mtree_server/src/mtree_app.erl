-module(mtree_app).

-behaviour(application).

-export([get_env/1, get_env/2, set_env/2]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


start(_StartType, _StartArgs) ->
    ok = ensure_started(compiler),
    ok = ensure_started(syntax_tools),
    ok = ensure_started(goldrush),
    ok = ensure_started(lager),
    ok = ensure_started(sasl),
    ok = ensure_started(os_mon),
    %% TODO add these to a application root supervisor!!
    mtree_sup:start_link(),
    mtree_server_sup:start_link().

stop(_State) ->
    ok.


get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Def) ->
    application:get_env(mtree_server, Key, Def).

set_env(Key, Val) ->
    application:set_env(mtree_server, Key, Val).
