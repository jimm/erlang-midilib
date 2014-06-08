-module(midilib).
-author("Jim Menard, jim@jimmenard.com").
-export([note_names/0, note_length/1, bpm_to_mpq/1, mpq_to_bpm/1, quantize/2,
	 note_to_string/1]).

note_names() ->
    ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"].

note_length(whole) -> 4.0;
note_length(half) -> 2.0;
note_length(quarter) -> 1.0;
note_length(eighth) -> 0.5;
note_length('8th') -> 0.5;
note_length(sixteenth) -> 0.25;
note_length('16th') -> 0.25;
note_length(thirtysecond) -> 0.125;
note_length('thirty second') -> 0.125;
note_length('32nd') -> 0.125;
note_length(sixtyfourth) -> 0.0625;
note_length('sixty fourth') -> 0.0625;
note_length('64th') -> 0.0625.
    
-define(MICROSECS_PER_MINUTE, 1000000 * 60).

%% Translates beats per minute to microseconds per quarter note (beat).
bpm_to_mpq(Bpm) ->
    ?MICROSECS_PER_MINUTE / Bpm.

%% Translates microseconds per quarter note (beat) to beats per minute.
mpq_to_bpm(Mpq) ->
    ?MICROSECS_PER_MINUTE / Mpq.

%% Quantize a lists's event's delta times by returning a new list of events
%% where the delta time of each is moved to the nearest multiple of Boundary.
quantize({track, ListOfEvents}, Boundary) ->
    quantize(ListOfEvents, Boundary);
quantize([], _Boundary) ->
    [];
quantize(ListOfEvents, Boundary) ->
    {NewListOfEvents, _} =
	lists:mapfoldl(fun(E, BeatsFromStart) -> 
			       quantized_event(E, BeatsFromStart, Boundary)
		       end,
		       0, ListOfEvents),
    NewListOfEvents.

%% Return a tuple containing a quantized copy of Event and the beats from
%% the start of this event before it was quantized.
quantized_event(Event, BeatsFromStart, Boundary) ->
    io:format("qe ~p, ~p, ~p~n", [Event, BeatsFromStart, Boundary]),
    {Name, DeltaTime, Values} = Event,
    Diff = (BeatsFromStart + DeltaTime) div Boundary,
    NewDeltaTime = if
		       Diff >= Boundary / 2 ->
			   DeltaTime - Diff;
		       true ->
			   DeltaTime - Diff + Boundary
		   end,
    {{Name, NewDeltaTime, Values}, BeatsFromStart + DeltaTime}.

quantized_delta_time(BeatsFromStart, DeltaTime, Boundary) ->
    Diff = (BeatsFromStart + DeltaTime) div Boundary,
    NewDeltaTime = if
		       Diff >= Boundary / 2 ->
			   DeltaTime - Diff;
		       true ->
			   DeltaTime - Diff + Boundary
		   end,

%% Given a MIDI note number, return the name and octave as a string.
note_to_string(Num) ->
    Note = Num rem 12,
    Octave = Num div 12,
    lists:concat([lists:nth(Note + 1, note_names()), Octave - 1]).
