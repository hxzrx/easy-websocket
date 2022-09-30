# Easy Websocket

A [`websocket-driver`](https://github.com/fukamachi/websocket-driver) wrapper to make it easy to setup a websocket server.

## Supported Servers

`Hunchentoot` is recommended although `websocket-driver` supports [`Hunchentoot`](https://edicl.github.io/hunchentoot/), [`Woo`](https://github.com/fukamachi/woo) and [`Wookie`](https://github.com/orthecreedence/wookie).

## Usage

There is a full demo in the file `src/example.lisp`. Most of the server code were copied from [Clog](https://github.com/rabbibotton/clog) with some modifications, and the client html src was adapted from [The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/websockets.html).

## APIs

### \[Function] `(start on-open-handler on-message-handler on-error-handler on-close-handler &rest args &key (host "0.0.0.0") (port 8080) (server  :hunchentoot) (workers 2) &allow-other-keys)`

Builds a `clack` app and then starts the webserver. Only websocket requests are allowed, which have upgrade/websocket record in the headers table, all other requests will be responded with code 403.

* `on-open-handler`:    function, accepts conn-obj and the env as its arguments, used to listen to the open event.

* `on-message-handler`: function, accepts conn-obj and the conn-id as its arguments, used to listen to the message event.

* `on-error-handler`:   function, accepts an error object as its argument, used to handle the error.

* `on-close-handler`:   function, accepts conn-obj as its argument, used to listen to the close event.

* `host`: the address this server will listen to.

* `port`: the port opened by this server

* `server`: can be one of :hunchentoot, :woo, :wookie, default to :hunchentoot, bugs occurred for others.

* `workers`: the count of threads which will be used to fork the workers of the webserver.

`on-open-handler`, `on-error-handler` and `on-close-handler` can be nil to ignore the respect events.

Note that the default webserver is `:hunchentoot`, others may have bugs or strange behaviors. For example, for `WOO` server, if we send a message to conn-obj in another thread, the connection will be closed.

### \[Macro] `(stop &body cleanup)`

Stop the websocket server and set `*app*` and `*handler*` to nil, where `*app*` is a `clack` app made by `lack:builder` and `*handler*` is a client handler returned by `clack:clackup`.

* `CLEANUP` is a list of forms which should be processed along with this shutdown.

### \[Macro] `(with-client client address on-message-handler &body body)`

Make a webscoket client and do what you want to, the connection will be closed after you've done the jobs.

* `CLIENT` is some symbol that will bind to a websocket client object, and it should be used in BODY.
* `ADDRESS`: a websocket address string such as \"ws://127.0.0.1:8080\".
* `ON-MESSAGE-HANDLER`: a unary function to handler the incoming messages, its sole argument is the message object.
* `BODY`: a list of forms.

**The following apis are directly exported from `websocket-driver` and hence their documents are directly pasted.**

### \[Method] `(send ws data &key start end type code callback)`

Sends `DATA` over the socket.

### \[Function] `(send-text ws message &key start end callback)`

Sends a text message over the socket.

### \[Function] `(send-binary ws usb8-vector &key start end callback)`

Takes an `(UNSIGNED-BYTE 8)` vector and sends them as a binary message.

### \[Method] `(send-ping ws &optional message callback)`

Sends a ping frame over the socket, queueing it if necessary.

### \[Method] `(close-connection ws)`

Initiates the closing handshake if the socket is still open.

### \[Method] `(version driver)`

Returns the WebSocket version in use as a string (ex. "hybi-13").

### \[Method] `(protocol driver)`

Returns a string containing the selected subprotocol, if any was agreed upon using the `Sec-WebSocket-Protocol` mechanism.

### \[Method] `(ready-state ws)`

Returns the connection state as a keyword, which is one of `:connecting`, `:open`, `:closing` and `:closed`.

## Author

He Xiang-zhi (xz.he@qq.com)
