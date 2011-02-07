%% Copyright (c) 1996, 1999 Johan Bevemyr
%% Copyright (c) 2007, 2009 Tony Garnock-Jones
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%    -*- Erlang -*- 
%    File:	serial.erl  (~jb/serialport/serial.erl)
%    Author:	Johan Bevemyr
%    Created:	Tue Oct 22 14:07:24 1996
%    Purpose:   

-module(serial).
-author('jb@erix.ericsson.se').

-export([start/0,start/1,init/1,loop/2]).

-include("serial.hrl").

priv_dir() ->
    case code:priv_dir(serial) of
	{error, bad_name} ->
	    "./priv";
	D ->
	    D
    end.

start() ->
    start([]).

start(Options) ->
    Pid = spawn_link(serial, init, [self()]),
    process_options(Pid,Options),
    Pid.

process_options(_Pid,[]) -> done;
process_options(Pid,[Opt|Opts]) ->
    Pid ! Opt,
    process_options(Pid,Opts).

init(Pid) ->
    process_flag(trap_exit,true),
    Port = open_port({spawn,priv_dir()++"/bin/serial -erlang"},[binary,{packet,2}]),
    loop(Pid,Port).

loop(Pid,Port) ->
    receive
	{Port, {data, Bytes}} ->
	    Pid ! {data, Bytes},
	    serial:loop(Pid,Port);
	{send, Bytes} ->
	    send_serial(Port,[?SEND,Bytes]),
	    serial:loop(Pid,Port);
	{connect} ->
	    send_serial(Port,[?CONNECT]),
	    serial:loop(Pid,Port);
	{disconnect} ->
	    send_serial(Port,[?DISCONNECT]),
	    serial:loop(Pid,Port);
	{open, TTY} ->
	    send_serial(Port,[?OPEN,TTY]),
	    serial:loop(Pid,Port);
	{close} ->
	    send_serial(Port,[?CLOSE]),
	    serial:loop(Pid,Port);
	{speed, NewInSpeed, NewOutSpeed} ->
	    send_serial(Port,[?SPEED,integer_to_list(NewInSpeed)," ",
			      integer_to_list(NewOutSpeed),0]),
	    serial:loop(Pid,Port);
	{speed, NewSpeed} ->
	    send_serial(Port,[?SPEED,integer_to_list(NewSpeed)," ",
			      integer_to_list(NewSpeed),0]),
	    serial:loop(Pid,Port);
	{parity_odd} ->
	    send_serial(Port,[?PARITY_ODD]),
	    serial:loop(Pid,Port);
	{parity_even} ->
	    send_serial(Port,[?PARITY_EVEN]),
	    serial:loop(Pid,Port);
	{break} ->
	    send_serial(Port,[?BREAK]),
	    serial:loop(Pid,Port);
	stop ->
	    stopped;
	{'EXIT', Port, Why} ->
	    io:format("Port exited with reason ~w~n", [Why]),
	    exit(Why);
	{'EXIT', Linked, Why} ->
	    io:format("Linked ~w exited with reason ~w~n", [Linked,Why]),
	    exit(Why);
	OtherError ->
	    io:format("Received unknown message ~w~n",[OtherError]),
	    serial:loop(Pid,Port)
    end.
	
send_serial(Port,Message) ->
    Port ! {self(),{command,Message}}.

    
