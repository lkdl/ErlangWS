-module(ws).
-export([ws_init/1, ws_send/3, ws_receive/1, ws_receive/2]).

ws_init(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Packet} ->
      try ws_handshake:handshake(Packet) of
        R ->
          gen_tcp:send(Sock, R),
          ok
      catch
        error ->
          throw(error)
      end;
    {error, closed} ->
      throw(closed);
    {error,_} ->
      throw(error)
  end.

ws_send(Sock, Type, Content) ->
  gen_tcp:send(Sock, ws_framing:buildMsg(Type, Content)).

ws_receive(Sock) -> ws_receive(Sock, normal).

ws_receive(Sock, normal) -> rcv(Sock, [], undefined, normal);
ws_receive(Sock, all) -> rcv(Sock, all);
ws_receive(Sock, raw) -> rcv(Sock, raw).

rcv(Sock, Acc, Type, normal) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Packet} ->
      case ws_framing:parseMsg(Packet) of
        {fragment, {Type,Con}} ->
            rcv(Sock, Acc++Con, Type, normal);
        {complete, {ping, Con}} ->
            gen_tcp:send(Sock, ws_framing:buildMsg(pong, Con)),
            rcv(Sock, [], undefined, normal);
        {complete, {pong, _}} ->
            rcv(Sock, [], undefined, normal);
        {complete, {cont, Con}} ->
          {complete, {Type, Acc++Con}};
        {complete, P} ->
          P
      end;
    {error, closed} ->
      error;
    {error,_} ->
      error
  end.

rcv(Sock, all) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Packet} ->
      case ws_framing:parseMsg(Packet) of
        {complete, {ping, Con}} ->
            gen_tcp:send(Sock,
            ws_framing:buildMsg(pong, Con)),
            rcv(Sock, [], undefined, normal);
        {complete, {pong, _}} ->
            rcv(Sock, [], undefined, normal);
        P ->
          P
      end;
    {error, closed} ->
      error;
    {error,_} ->
      error
  end;

rcv(Sock, raw) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Packet} ->
      case ws_framing:parseMsg(Packet) of
        P ->
          P
      end;
    {error, closed} ->
      error;
    {error,_} ->
      error
  end.
