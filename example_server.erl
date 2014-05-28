-module(example_server).
-export([start/0, client_loop/1]).


start() ->
  {ok, LSock} = gen_tcp:listen(5678, [list, {packet, 0},{active, false}]),
  io:format("Server started!~n"),
  server_loop(LSock).

server_loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  try ws:ws_init(Sock) of
    ok ->
      spawn(?MODULE, client_loop, [Sock]),
      server_loop(LSock)
  catch
    error ->
      gen_tcp:close(Sock),
      server_loop(LSock)
  end.


client_loop(Sock) ->
  case ws:ws_receive(Sock) of
    {close, _} ->
      gen_tcp:close(Sock);
    {text, Con} ->
      ws:ws_send(Sock, text, Con),
      client_loop(Sock);
    error ->
      client_loop(Sock);
    _ ->
      ws:ws_send(Sock, text, "I don't think we should see each other anymore. It just doesn't work... :("),
      client_loop(Sock)
  end.
