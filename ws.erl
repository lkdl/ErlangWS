-module(ws).
-export([ws_init/1, ws_send/3, ws_recv/1]).

ws_init(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Packet} ->
      gen_tcp:send(Sock, ws_handshake:handshake(Packet));
    {error, closed} ->
      throw(connclosed);
    {error,_} ->
      throw(connerr)
  end.

ws_send(Sock, Type, Content) ->
  gen_tcp:send(Sock, ws_framing:buildMsg(Type, Content)).

ws_recv(Sock) ->
    rcv(Sock, [], undefined).

rcv(Sock, Acc, Type) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Packet} ->
      case ws_framing:parseMsg(Packet) of
        {fragment, {Type,Con}} ->
            rcv(Sock, Acc++Con, Type);
        {complete, {ping, Con}} ->
            gen_tcp:send(Sock, ws_framing:buildMsg(pong, Con)),
            rcv(Sock, [], undefined);
        {complete, {pong, _}} ->
            rcv(Sock, [], undefined);
        {complete, {cont, Con}} ->
          {complete, {Type, Acc++Con}};
        {complete, P} ->
          P
      end;
    {error, closed} ->
      throw(connclosed);
    {error,_} ->
      throw(connerr)
  end.
