-module(ws).
-compile(export_all).


start() ->
  io:format("Server started!~n"),
  {ok, LSock} = gen_tcp:listen(5678, [list, {packet, 0},{active, false}]),
  server_loop(LSock).

server_loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  client_loop(Sock,0).

client_loop(Sock, HS) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
      if
        HS == 1  ->
          {_, text, R} = ws_framing:parseMsg(list_to_binary(B)),
          gen_tcp:send(Sock, ws_framing:buildMsg(text,R)),
          client_loop(Sock, HS);
        true ->
          gen_tcp:send(Sock, ws_handshake:handshake(B)),
          client_loop(Sock, 1)
      end;
    {error, closed} ->
      ok = gen_tcp:close(Sock)
  end.
