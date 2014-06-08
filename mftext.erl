-module(mftext).
-export([seq_to_text/1, seq_to_text/2, track_to_text/1, track_to_text/2, event_to_text/1, event_to_text/2]).
-author("Jim Menard, jim@jimmenard.com").

seq_to_text(Seq) ->
    seq_to_text(Seq, false).
seq_to_text(Seq, ShowChanEvents) ->
    {seq, _, Tracks} = Seq,
    lists:map(fun(T) -> track_to_text(T, ShowChanEvents) end, Tracks),
    ok.

track_to_text(Track) ->
    track_to_text(Track, false).
track_to_text(Track, ShowChanEvents) ->
    io:format("Track start~n"),
    {track, Events} = Track,
    lists:map(fun(E) -> event_to_text(E, ShowChanEvents) end, Events),
    ok.

event_to_text(Event) ->
    event_to_text(Event, false).
event_to_text(Event, ShowChanEvents) ->
    {Name, _} = Event,
    IsChanEvent = lists:any(fun(X) -> X =:= Name end, 
			    [off, on, poly_press, controller, program, 
			     chan_press, pitch_bend]),
    if
	ShowChanEvents; not IsChanEvent ->
	    io:format("~p~n", [Event])
    end.
