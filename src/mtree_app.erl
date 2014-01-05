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
    ensure_started(sasl),
    ensure_started(os_mon),
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(goldrush),
    ensure_started(lager),
    mtree_sup:start_link().

stop(_State) ->
    ok.
