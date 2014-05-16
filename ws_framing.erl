-module(ws_framing).
-export([parseMsg/1, buildMsg/2]).

get(0, Len, Str) ->
	<<V:Len, _/bitstring>> = Str,
	V;
get(Offset, Len, Str) ->
	<<_:Offset, Rest/bitstring>> = Str,
	get(0,Len, Rest).


parseMsg(Str) ->
	Bin = list_to_binary(Str),
	WSFin = get(0,1,Bin),
	if
		WSFin == 1 ->
			Fin = complete;
		true ->
			Fin = fragment
	end,
	WSOpCode = get(4,4, Bin),
	if
		WSOpCode == 0 ->
			Type = cont;
		WSOpCode == 1 ->
			Type = text;
		WSOpCode == 2 ->
			Type = binary;
		WSOpCode == 8 ->
			Type = close;
		WSOpCode == 9 ->
			Type = ping;
		WSOpCode == 10 ->
			Type = pong;
		true ->
			Type = error,
			throw(error)
	end,
	WSMask = get(8,1, Bin),
	LenTest = get(9,7, Bin),
	if
		LenTest =< 125 ->
			WSLength = LenTest,
			LenOffset = 0;
		LenTest == 126 ->
			WSLength = get(9+7,16, Bin),
			LenOffset = 16;
		LenTest == 127 ->
			WSLength = get(9+7,64, Bin),
			LenOffset = 64
	end,
	WSMask = get(8,1, Bin),
	if
		WSMask == 1 ->
			Msg = readPayload(WSLength, 0, 16+LenOffset, 16+LenOffset+32, Bin, []);
		true ->
			Msg = readPayload(WSLength, 0, 0, 16+LenOffset, Bin, [])
	end,
	{Fin, {Type, Msg}}.

readPayload(Len,I, _, _, _, Dec) when I == Len -> Dec;
readPayload(Len,I, 0, PayloadOffset, Bin, Dec) ->
	D = get(PayloadOffset + 8*I,8,Bin),
	readPayload(Len, I+1, 0, PayloadOffset, Bin, Dec++[D]);
readPayload(Len,I, KeyOffset, PayloadOffset, Bin, Dec) ->
	D = get(KeyOffset + 8*(I rem 4),8,Bin) bxor get(PayloadOffset + 8*I,8,Bin),
	readPayload(Len, I+1, KeyOffset, PayloadOffset, Bin, Dec++[D]).


buildMsg(Type, Content) ->
	St = 2#1000,
	BCon = list_to_binary(Content),
	case Type of
		text ->
			OType = 1;
		binary ->
			OType = 2;
		close ->
			OType = 8;
		ping ->
			OType = 9;
		pong ->
			OType = 10;
		_ ->
			OType = -1,
			throw(error)
	end,
	Len = length(Content),
	if
		Len =< 125 ->
			FBLen=Len,
			LenBits=0;
		Len =< 65536 ->
			FBLen=126,
			LenBits=16;
		true ->
			FBLen=127,
			LenBits=64
	end,
	if
		LenBits == 0 ->
			P1 = <<St:4,OType:4, 0:1,FBLen:7>>;
		true ->
			P1 = <<St:4,OType:4, 0:1,FBLen:7,Len:LenBits>>
	end,
	P2 = BCon,
	<<P1/binary, P2/binary>>.
