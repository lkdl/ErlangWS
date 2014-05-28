ErlangWS
========
An Erlang implementation of the Websocket Protocol ([RFC 6455](http://tools.ietf.org/html/rfc6455)) for server usage.

This implementation is quite simple and straightforward since it does not depend on external libraries.

##Remarks

* Websockets will only work with TCP Sockets
* ErlangWS does not handle the connection itself which means that:
  * you have to create, bind and open the socket before you call `ws_init`
 * the Socket has to be closed manually, ErlangWS will throw an error when the connection is closed by the client


## Data Types

According to the RFC the types of messages as listed in `datatype()` are supported and messages can also be fragmented or complete.

Besides of the pure data types there are also control frames specified. If needed these can also be received (see below for

```
datatype() = text
           | binary

msgtype()  = cont
           | close
           | ping
           | pong
           | datatype()

frag()     = complete
           | fragment

rcvmode()  = normal
           | all
           | raw
```

Messages are represented as tuples. These can either have the simplified form

`simple() = {datatype(), Payload}`

or the complete form:

`message () = {frag(), {msgtype(), Payload}}`



## Exported Functions

__ws_init(Socket)__

Types:
 * Socket = gen_tcp:socket()

Initializes the websocket connection by performing a websocket handshake. This function will block the caller since it expects to receive a HTTP packet. If the handshake was successful the function will return `ok`, otherwise `error`.

---

__ws_send(Socket, Type, Content)__

Types:
 * Socket = gen_tcp:Socket()
 * Type = msgtype()
 * Content = String

Sends `Content` with the `Type` over the `Socket`.

---

__ws_receive(Socket)__

Types:
 * Socket = gen_tcp:Socket()

Convenience wrapper for `ws_receive(Socket, normal)`.

---

__ws_receive(Socket, Mode)__

Types:
 * Socket = gen_tcp:Socket()
 * Mode = rcvmode()

Receives a message on the Socket. If the packet is not a valid websocket message the function will throw `error`. If it is valid, the return value depends on the mode.

In the `normal` mode, the function will answer `ping` frames, handle `pong` frames and will throw an error if the connection is closed by the client. Furthermore it will buffer incoming fragmented packages until the whole message is complete.
In this mode, `ws_receive` will return the simple message form.

In the `all` mode, the function will answer `ping` frames, handle `pong` frames. All other frames will be returned as they are. Messages will be returned in the complete form.

The `raw` mode just delivers all valid messages in the complete form including all op frames. 
