%    -*- Erlang -*- 
%    File:	slip.erl  (~jb/serialport/slip.erl)
%    Author:	Johan Bevemyr
%    Created:	Thu Oct 24 16:17:28 1996
%    Purpose:   
 
-module(slip).
-author('jb@erix.ericsson.se').

%
%  SLIP (Serial Line IP)
% 
%  The idea behind SLIP is very simple. IP-packets are sent over
%  a serial line. Each packet is terminated with END (digtal 192).
%  If END occurs in the packet the sequence ESC (digital 219) and
%  ESCEND (digital 220) is sent instead. If ESC occurs then the 
%  sequence ESC ESCESC (digital 221) is sent instead.
% 
%  An optimisation is to start all packets with an END token. This
%  has the effect of flushing all line noise that may have occured.
%
%  Both the sender and the reciever need to know each others 
%  IP-addresses. 
%   

-define(END,192).
-define(ESC,219).
-define(ESCEND,220).
-define(ESCESC,221).

-export([start/1,init/2]).

start(IPAddress) when is_list(IPAddress), length(IPAddress) == 4 ->
    spawn_link(slip,init,[IPAddress,self()]);

start(IPAddress) when is_atom(IPAddress) ->
    xk:start(),
    case xk:host2ip(IPAddress) of
	{ok,IpDst} ->
	    spawn_link(slip,init,[IpDst,self()]);
	_ ->
	    {error,unknown_host}
    end.

init(Dest,Who) ->
    Serial = serial:start([{speed,38400},flow,{open,"/dev/ttya"}]),
    loop(Dest,Who,Serial).

loop(Dest,Who,Serial) ->
    receive
	{send,Packet} ->
	    EscPacket = slip_pack(Packet),
	    Serial ! {send,EscPacket},
	    loop(Dest,Who,Serial);
	{data,FromOtherSide} ->
	    UnEscPacket = slip_unpack(FromOtherSide),
	    % Ignore zero length packets.
	    case length(UnEscPacket) of
		0 ->
		    loop(Dest,Who,Serial);
		_NonZero ->
		    Who ! {ip,UnEscPacket},
		    loop(Dest,Who,Serial)
	    end;
	Other ->
	    io:format('Received ~w~n',[Other]),
	    loop(Dest,Who,Serial)
    end.

slip_pack(Packet) ->
    RevEscPack = slip_pack_acc(Packet,[?END]),
    lists:reverse(RevEscPack).

slip_pack_acc([],Acc) -> [?END|Acc];
slip_pack_acc([X|Xs],Acc) ->
    case X of
	?END ->
	    slip_pack_acc(Xs,[?ESCEND,?ESC|Acc]);
	?ESC ->
	    slip_pack_acc(Xs,[?ESCESC,?ESC|Acc]);
	_Other ->
	    slip_pack_acc(Xs,[X|Acc])
    end.


slip_unpack(Packet) ->
    RevUnPack = slip_unpack_acc(Packet,[]),
    lists:reverse(RevUnPack).

slip_unpack_acc([],Acc) -> Acc;
slip_unpack_acc([X|Xs],Acc) ->
    case X of 
	?ESC ->
	    slip_unpack_esc_acc(Xs,Acc);
	?END ->
	    io:format('slip unpacking: premature end of packet~n',[]);
	_Other ->
	    slip_unpack_acc(Xs,[X|Acc])
    end.

slip_unpack_esc_acc([X|Xs],Acc) ->
    case X of
	?ESCEND ->
	    slip_unpack_acc(Xs,[?END|Acc]);
	?ESCESC ->
	    slip_unpack_acc(Xs,[?ESC|Acc])
    end.

	    
	    
    

