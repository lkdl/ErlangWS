-module(example_server).
-export([start/0, client_loop/1]).


start() ->
  io:format("Server started!~n"),
  {ok, LSock} = gen_tcp:listen(5678, [list, {packet, 0},{active, false}]),
  server_loop(LSock).

server_loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  try ws:ws_init(Sock) of
    ok ->
      spawn(?MODULE, client_loop, [Sock]),
      server_loop(LSock)
  catch
    _ ->
      server_loop(LSock)
  end.


client_loop(Sock) ->
  case ws:ws_recv(Sock) of
    {close, _} ->
      gen_tcp:close(Sock);
    {text, Con} ->
      ws:ws_send(Sock, text, Con),
      client_loop(Sock);
    What ->
      io:format("~w", [What]),
      ws:ws_send(Sock, text, "I don't think we should see each other anymore. It just doesn't work... :("),
      client_loop(Sock)
  end.
