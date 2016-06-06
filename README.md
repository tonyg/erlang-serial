# erlang-serial

This is a port program with erlang driver for serial communication,
originally written by Johan Bevemyr in 1996 and sporadically
maintained by Tony Garnock-Jones from 2007 onwards.

## Installation

This library is designed to run as an Erlang library, not an application
dependency. To install this library, clone the library to a location of your
choice and run the following from the command line:

```text
make
DESTDIR=/usr/lib make install
```

Adjust the `DESTDIR` path accordingly if Erlang is not installed at
`/usr/lib/erlang`. The `serial` module should now be accessible in Erlang, which
can be verified by running `erl`. `serial:start()` should return a PID.

```text
Erlang/OTP 17 [erts-6.4.1] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V6.4.1  (abort with ^G)
1> serial:start().
<0.35.0>
```

## Examples

The following examples are excerpts from `examples/basic_example.erl`.

Opening a connection to a USB serial adapter at 9600 baud:

```erlang
SerialPort = serial:start([{open, "/dev/ttyUSB0"}, {speed, 9600}])
```

Sending a message out the serial port:

```erlang
SerialPort ! {send, "Hello World\r\n"}
```

Data is received as a message to the process that called `serial:start()`. That
process can handle the data by implementing a function like the following:

```erlang
listen() ->
  receive
    % Receive data from the serial port on the caller's PID.
    {data, Bytes} ->
      io:format("~s", [Bytes]),
      listen()
  after
    % Stop listening after 5 seconds of inactivity.
    5000 ->
      io:format("~n"),
      ok
  end.
```

See `examples/terminal.erl` for more example code (using the now-obsolete `gs` module).

## License

Copyright (c) 1996, 1999 Johan Bevemyr  
Copyright (c) 2007, 2009 Tony Garnock-Jones

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
