-module(mtree_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

add_child(Args) ->
    supervisor:start_child(?MODULE, [Args]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(mtree_tcp_proxy, worker)]} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
