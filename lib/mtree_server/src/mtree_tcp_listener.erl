-module(mtree_tcp_listener).

-export([start_link/2, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, sock_opts/0, new_connection/2]).

-behavior(gen_nb_server).

start_link() ->
    IP = mtree_app:get_env(ip_addr, "127.0.0.1"),
    Port = mtree_app:get_env(port, 4567),

    start_link(IP, Port).

start_link(IpAddr, Port) ->
    error_logger:info_msg("Starting tcp listener, ~s:~p.~n", [IpAddr, Port]),
    gen_nb_server:start_link(?MODULE, IpAddr, Port, []).

init([]) ->
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    error_logger:info_msg("~p received: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

sock_opts() ->
    [binary, {packet, 4}, {reuseaddr, true}].

new_connection(Sock, State) ->
    error_logger:info_msg("Accepted new connection, starting mtree_tcp_proxy~n",[]),
    {ok, Pid} = mtree_tcp_proxy:start_supervised(Sock),
    gen_tcp:controlling_process(Sock, Pid),
    inet:setopts(Sock, [{active, once}]),
    {ok, State}.


