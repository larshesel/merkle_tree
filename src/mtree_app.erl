-module(mtree_app).

-behaviour(application).

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
    ok = ensure_started(sasl),
    ok = ensure_started(os_mon),
    ok = ensure_started(compiler),
    ok = ensure_started(syntax_tools),
    ok = ensure_started(goldrush),
    ok = ensure_started(lager),
    mtree_sup:start_link().

stop(_State) ->
    ok.
