-module(core_midi).
-author("Jim Menard, jimm@io.com").
-include("core_midi.hrl").
-export([start/0, stop/0]).
-export([num_destinations/0, num_sources/0, num_devices/0, num_external_devices/0]).

start() ->
    spawn(fun() ->
		  register(core_midi, self()),
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "./core_midi"}, [{packet, 2}]),
		  loop(Port)
	  end).

stop() ->
    core_midi ! stop.

num_destinations() -> call_port({num_destinations}).
num_sources() -> call_port({num_sources}).
num_devices() -> call_port({num_devices}).
num_external_devices() -> call_port({num_external_devices}).

call_port(Msg) ->
    core_midi ! {call, self(), Msg},
    receive
	{core_midi, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {core_midi, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated,Reason})
    end.

encode({num_destinations}) -> [?FUNC_MIDIGetNumberOfDestinations];
encode({num_sources}) -> [?FUNC_MIDIGetNumberOfSources];
encode({num_devices}) -> [?FUNC_MIDIGetNumberOfDevices];
encode({num_external_devices}) -> [?FUNC_MIDIGetNumberOfExternalDevices].

decode([Int]) -> Int.
