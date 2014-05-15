-module(ws_handshake).
-export([handshake/1]).

trim(String) ->
    string:strip(String).

appendGUID(String) ->
    string:concat(String, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

sha1(String) ->
  crypto:hash(sha, String).

base64(String) ->
    base64:encode(String).

respKey(Key) ->
    binary_to_list(base64(sha1(appendGUID(trim(Key))))).


getKey(Str) ->
    Bin = list_to_binary(Str),
    Packet = erlang:decode_packet(http, Bin ,[]),
    case Packet of
      {ok, {http_request, 'GET', _, _}, Fields} ->
        getKeyHeader(Fields);
      _ ->
        throw(nohttp)
    end.

getKeyHeader(Bin) ->

  Header = erlang:decode_packet(httph, Bin ,[]),
  case Header of
    {ok, {http_header,_,"Sec-Websocket-Key",_,Key},_} ->
      Key;
    {ok, {http_header,_,_,_,_}, More} ->
      getKeyHeader(More);
    _ ->
      throw(noheader)
  end.

handshake(Bin) ->
  RespKey = respKey(getKey(Bin)),
  P = string:concat("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept:", RespKey),
  string:concat(P, "\r\n\r\n").
