-module(midifile).
-export([read/1, write/2]).
-author("Jim Menard, jim@jimmenard.com").
-include("midi_consts.hrl").

%% This module reads and writes MIDI files.

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(DPRINT(X, Y), io:format(X, Y)).
-else.
-define(DPRINT(X, Y), void).
-endif.


%% Returns
%%   {seq, {header...}, ConductorTrack, ListOfTracks}
%% header is {header, Format, Division}
%% ConductorTrack is the first track of a format 1 MIDI file
%% each track including the conductor track is
%%   {track, ListOfEvents}
%% each event is
%%   {event_name, DeltaTime, [values...]}
%% where values after DeltaTime are specific to each event type. If the value
%% is a string, then the string appears instead of [values...].
read(Path) ->
    case file:open(Path, [read, binary, raw]) of
	{ok, F} ->
	    FilePos = look_for_chunk(F, 0, <<$M, $T, $h, $d>>,
				     file:pread(F, 0, 4)),
	    [Header, NumTracks] = parse_header(file:pread(F, FilePos, 10)),
	    Tracks = read_tracks(F, NumTracks, FilePos + 10, []),
	    file:close(F),
	    [ConductorTrack | RemainingTracks] = Tracks,
	    {seq, Header, ConductorTrack, RemainingTracks};
	Error ->
	    {Path, Error}
    end.

% Look for Cookie in file and return file position after Cookie.
look_for_chunk(_F, FilePos, Cookie, {ok, Cookie}) ->
    FilePos + size(Cookie);
look_for_chunk(F, FilePos, Cookie, {ok, _}) ->
    % This isn't efficient, because we only advance one character at a time.
    % We should really look for the first char in Cookie and, if found,
    % advance that far.
    look_for_chunk(F, FilePos + 1, Cookie, file:pread(F, FilePos + 1,
						      size(Cookie))).

parse_header({ok, <<_BytesToRead:32/integer, Format:16/integer,
		   NumTracks:16/integer, Division:16/integer>>}) ->
   [{header, Format, Division}, NumTracks].

read_tracks(_F, 0, _FilePos, Tracks) ->
    lists:reverse(Tracks);
% TODO: make this distributed. Would need to scan each track to get start
% position of next track.
read_tracks(F, NumTracks, FilePos, Tracks) ->
    ?DPRINT("read_tracks, NumTracks = ~p, FilePos = ~p~n",
	    [NumTracks, FilePos]),
    [Track, NextTrackFilePos] = read_track(F, FilePos),
    ?DPRINT("read_tracks, NextTrackFilePos = ~p~n", [NextTrackFilePos]),
    read_tracks(F, NumTracks - 1, NextTrackFilePos, [Track|Tracks]).

read_track(F, FilePos) ->
    TrackStart = look_for_chunk(F, FilePos, <<$M, $T, $r, $k>>,
				file:pread(F, FilePos, 4)),
    BytesToRead = parse_track_header(file:pread(F, TrackStart, 4)),
    ?DPRINT("reading track, FilePos = ~p, BytesToRead = ~p~n",
	    [TrackStart, BytesToRead]),
    ?DPRINT("next track pos = ~p~n", [TrackStart + 4 + BytesToRead]),
    put(status, 0),
    put(chan, -1),
    [{track, event_list(F, TrackStart + 4, BytesToRead, [])},
     TrackStart + 4 + BytesToRead].

parse_track_header({ok, <<BytesToRead:32/integer>>}) ->
    BytesToRead.

event_list(_F, _FilePos, 0, Events) ->
    lists:reverse(Events);
event_list(F, FilePos, BytesToRead, Events) ->
    [DeltaTime, VarLenBytesUsed] = read_var_len(file:pread(F, FilePos, 4)),
    {ok, ThreeBytes} = file:pread(F, FilePos+VarLenBytesUsed, 3),
    ?DPRINT("reading event, FilePos = ~p, BytesToRead = ~p, ThreeBytes = ~p~n",
	    [FilePos, BytesToRead, ThreeBytes]),
    [Event, EventBytesRead] =
	read_event(F, FilePos+VarLenBytesUsed, DeltaTime, ThreeBytes),
    BytesRead = VarLenBytesUsed + EventBytesRead,
    event_list(F, FilePos + BytesRead, BytesToRead - BytesRead, [Event|Events]).

read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_OFF:4, Chan:4, Note:8, Vel:8>>) ->
    ?DPRINT("off~n", []),
    put(status, ?STATUS_NIBBLE_OFF),
    put(chan, Chan),
    [{off, DeltaTime, [Chan, Note, Vel]}, 3];
% note on, velocity 0 is a note off
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_ON:4, Chan:4, Note:8, 0:8>>) ->
    ?DPRINT("off (using on vel 0)~n", []),
    put(status, ?STATUS_NIBBLE_ON),
    put(chan, Chan),
    [{off, DeltaTime, [Chan, Note, 64]}, 3];
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_ON:4, Chan:4, Note:8, Vel:8>>) ->
    ?DPRINT("on~n", []),
    put(status, ?STATUS_NIBBLE_ON),
    put(chan, Chan),
    [{on, DeltaTime, [Chan, Note, Vel]}, 3];
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_POLY_PRESS:4, Chan:4, Note:8, Amount:8>>) ->
    ?DPRINT("poly press~n", []),
    put(status, ?STATUS_NIBBLE_POLY_PRESS),
    put(chan, Chan),
    [{poly_press, DeltaTime, [Chan, Note, Amount]}, 3];
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_CONTROLLER:4, Chan:4, Controller:8, Value:8>>) ->
    ?DPRINT("controller ch ~p, ctrl ~p, val ~p~n", [Chan, Controller, Value]),
    put(status, ?STATUS_NIBBLE_CONTROLLER),
    put(chan, Chan),
    [{controller, DeltaTime, [Chan, Controller, Value]}, 3];
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_PROGRAM_CHANGE:4, Chan:4, Program:8, _:8>>) ->
    ?DPRINT("prog change~n", []),
    put(status, ?STATUS_NIBBLE_PROGRAM_CHANGE),
    put(chan, Chan),
    [{program, DeltaTime, [Chan, Program]}, 2];
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_CHANNEL_PRESSURE:4, Chan:4, Amount:8, _:8>>) ->
    ?DPRINT("chan pressure~n", []),
    put(status, ?STATUS_NIBBLE_CHANNEL_PRESSURE),
    put(chan, Chan),
    [{chan_press, DeltaTime, [Chan, Amount]}, 2];
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_NIBBLE_PITCH_BEND:4, Chan:4, 0:1, LSB:7, 0:1, MSB:7>>) ->
    ?DPRINT("pitch bend~n", []),
    put(status, ?STATUS_NIBBLE_PITCH_BEND),
    put(chan, Chan),
    [{pitch_bend, DeltaTime, [Chan, <<0:2, MSB:7, LSB:7>>]}, 3];
read_event(_F, _FilePos, DeltaTime,
	   <<?STATUS_META_EVENT:8, ?META_TRACK_END:8, 0:8>>) ->
    ?DPRINT("end of track~n", []),
    put(status, ?STATUS_META_EVENT),
    put(chan, 0),
    [{track_end, DeltaTime, []}, 3];
read_event(F, FilePos, DeltaTime, <<?STATUS_META_EVENT:8, Type:8, _:8>>) ->
    ?DPRINT("meta event~n", []),
    put(status, ?STATUS_META_EVENT),
    put(chan, 0),
    [Length, LengthBytesUsed] = read_var_len(file:pread(F, FilePos + 2, 4)),
    LengthBeforeData = LengthBytesUsed + 2,
    {ok, Data} = file:pread(F, FilePos + LengthBeforeData, Length),
    TotalLength = LengthBeforeData + Length,
    ?DPRINT("  type = ~p, var len = ~p, len before data = ~p, total len = ~p,~n  data = ~p~n",
	      [Type, Length, LengthBeforeData, TotalLength, Data]),
    case Type of
	?META_SEQ_NUM ->
	    [{seq_num, DeltaTime, [Data]}, TotalLength];
	?META_TEXT ->
	    [{text, DeltaTime, binary_to_list(Data)}, TotalLength];
	?META_COPYRIGHT ->
	    [{copyright, DeltaTime, binary_to_list(Data)}, TotalLength];
	?META_SEQ_NAME ->
	    [{seq_name, DeltaTime, binary_to_list(Data)}, TotalLength];
	?META_INSTRUMENT ->
	    [{instrument, DeltaTime, binary_to_list(Data)}, TotalLength];
	?META_LYRIC ->
	    [{lyric, DeltaTime, binary_to_list(Data)}, TotalLength];
	?META_MARKER ->
	    [{marker, DeltaTime, binary_to_list(Data)}, TotalLength];
	?META_CUE ->
	    [{cue, DeltaTime, binary_to_list(Data)}, TotalLength];
	?META_MIDI_CHAN_PREFIX ->
	    [{midi_chan_prefix, DeltaTime, [Data]}, TotalLength];
	?META_SET_TEMPO ->
	    % Data is microseconds per quarter note, in three bytes
	    <<B0:8, B1:8, B2:8>> = Data,
	    [{tempo, DeltaTime, [(B0 bsl 16) + (B1 bsl 8) + B2]}, TotalLength];
	?META_SMPTE ->
	    [{smpte, DeltaTime, [Data]}, TotalLength];
	?META_TIME_SIG ->
	    [{time_signature, DeltaTime, [Data]}, TotalLength];
	?META_KEY_SIG ->
	    [{key_signature, DeltaTime, [Data]}, TotalLength];
	?META_SEQUENCER_SPECIFIC ->
	    [{seq_name, DeltaTime, [Data]}, TotalLength];
	_ ->
	    ?DPRINT("  unknown meta type ~p~n", [Type]),
	    [{unknown_meta, DeltaTime, [Type, Data]}, TotalLength]
    end;
read_event(F, FilePos, DeltaTime, <<?STATUS_SYSEX:8, _:16>>) ->
    ?DPRINT("sysex~n", []),
    put(status, ?STATUS_SYSEX),
    put(chan, 0),
    [Length, LengthBytesUsed] = read_var_len(file:pread(F, FilePos + 1, 4)),
    {ok, Data} = file:pread(F, FilePos + LengthBytesUsed, Length),
    [{sysex, DeltaTime, [Data]}, LengthBytesUsed + Length];
% Handle running status bytes
read_event(F, FilePos, DeltaTime, <<B0:8, B1:8, _:8>>) when B0 < 128 ->
    Status = get(status),
    Chan = get(chan),
    ?DPRINT("running status byte, status = ~p, chan = ~p~n", [Status, Chan]),
    [Event, NumBytes] =
	read_event(F, FilePos, DeltaTime, <<Status:4, Chan:4, B0:8, B1:8>>),
    [Event, NumBytes - 1];
read_event(_F, _FilePos, DeltaTime, <<Unknown:8, _:16>>) ->
    ?DPRINT("unknown byte ~p~n", [Unknown]),
    put(status, 0),
    put(chan, 0),
%    exit("Unknown status byte " ++ Unknown).
    [{unknown_status, DeltaTime, [Unknown]}, 3].

read_var_len({ok, <<0:1, B0:7, _:24>>}) ->
    [B0, 1];
read_var_len({ok, <<1:1, B0:7, 0:1, B1:7, _:16>>}) ->
    [(B0 bsl 7) + B1, 2];
read_var_len({ok, <<1:1, B0:7, 1:1, B1:7, 0:1, B2:7, _:8>>}) ->
    [(B0 bsl 14) + (B1 bsl 7) + B2, 3];
read_var_len({ok, <<1:1, B0:7, 1:1, B1:7, 1:1, B2:7, 0:1, B3:7>>}) ->
    [(B0 bsl 21) + (B1 bsl 14) + (B2 bsl 7) + B3, 4];
read_var_len({ok, <<1:1, B0:7, 1:1, B1:7, 1:1, B2:7, 1:1, B3:7>>}) ->
    ?DPRINT("WARNING: bad var len format; all 4 bytes have high bit set~n", []),
    [(B0 bsl 21) + (B1 bsl 14) + (B2 bsl 7) + B3, 4].

-ifdef(DEBUG).
rvl(<<0:1, B0:7>>) ->
    read_var_len({ok, <<0:1, B0:7, 0:8, 0:8, 0:8>>});
rvl(<<1:1, B0:7, 0:1, B1:7>>) ->
    read_var_len({ok, <<1:1, B0:7, 0:1, B1:7, 0:8, 0:8>>});
rvl(<<1:1, B0:7, 1:1, B1:7, 0:1, B2:7>>) ->
    read_var_len({ok, <<1:1, B0:7, 1:1, B1:7, 0:1, B2:7, 0:8>>});
rvl(<<1:1, B0:7, 1:1, B1:7, 1:1, B2:7, 0:1, B3:7>>) ->
    read_var_len({ok, <<1:1, B0:7, 1:1, B1:7, 1:1, B2:7, 0:1, B3:7>>}).
-endif.

write({seq, Header, ConductorTrack, Tracks}, Path) ->
    L = [header_io_list(Header, length(Tracks) + 1) |
	 lists:map(fun(T) -> track_io_list(T) end, [ConductorTrack | Tracks])],
    ok = file:write_file(Path, L).

header_io_list(Header, NumTracks) ->
    {header, _, Division} = Header,
    ["MThd",
     0, 0, 0, 6,				% header chunk size
     0, 1,					% format,
     (NumTracks bsr 8) band 255,		% num tracks
      NumTracks        band 255,
     (Division bsr 8) band 255,			% division
      Division        band 255].

track_io_list(Track) ->
    {track, Events} = Track,
    put(status, 0),
    put(chan, 0),
    EventList =  lists:map(fun(E) -> event_io_list(E) end, Events),
    ChunkSize = chunk_size(EventList),
    ["MTrk",
     (ChunkSize bsr 24) band 255,
     (ChunkSize bsr 16) band 255,
     (ChunkSize bsr  8) band 255,
      ChunkSize         band 255,
     EventList].

% Return byte size of L, which is an IO list that contains lists, bytes, and
% binaries.
chunk_size(L) ->
    lists:foldl(fun(E, Acc) -> Acc + io_list_element_size(E) end, 0,
		lists:flatten(L)).
io_list_element_size(E) when is_binary(E) ->
    size(E);
io_list_element_size(_E) ->
    1.

event_io_list({off, DeltaTime, [Chan, Note, Vel]}) ->
    RunningStatus = get(status),
    RunningChan = get(chan),
    if
	RunningChan =:= Chan,
	    (RunningStatus =:= ?STATUS_NIBBLE_OFF orelse
	     (RunningStatus =:= ?STATUS_NIBBLE_ON andalso Vel =:= 64)) ->
	    Status = [],
	    OutVel = 0;
	true ->
	    Status = (?STATUS_NIBBLE_OFF bsl 4) + Chan,
	    OutVel = Vel,
	    put(status, ?STATUS_NIBBLE_OFF),
	    put(chan, Chan)
    end,
    [var_len(DeltaTime), Status, Note, OutVel];
event_io_list({on, DeltaTime, [Chan, Note, Vel]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_ON, Chan), Note, Vel];
event_io_list({poly_press, DeltaTime, [Chan, Note, Amount]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_POLY_PRESS, Chan), Note,
     Amount];
event_io_list({controller, DeltaTime, [Chan, Controller, Value]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_CONTROLLER, Chan),
     Controller, Value];
event_io_list({program, DeltaTime, [Chan, Program]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_PROGRAM_CHANGE, Chan),
     Program];
event_io_list({chan_press, DeltaTime, [Chan, Amount]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_CHANNEL_PRESSURE, Chan),
     Amount];
event_io_list({pitch_bend, DeltaTime, [Chan, <<0:2, MSB:7, LSB:7>>]}) ->
    [var_len(DeltaTime), running_status(?STATUS_NIBBLE_PITCH_BEND, Chan),
     <<0:1, LSB:7, 0:1, MSB:7>>];
event_io_list({track_end, DeltaTime}) ->
    ?DPRINT("track_end~n", []),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, ?META_TRACK_END, 0];
event_io_list({seq_num, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_SEQ_NUM, Data);
event_io_list({text, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_TEXT, Data);
event_io_list({copyright, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_COPYRIGHT, Data);
event_io_list({seq_name, DeltaTime, Data}) ->
    put(status, ?STATUS_META_EVENT),
    meta_io_list(DeltaTime, ?META_TRACK_END, Data);
event_io_list({instrument, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_INSTRUMENT, Data);
event_io_list({lyric, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_LYRIC, Data);
event_io_list({marker, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_MARKER, Data);
event_io_list({cue, DeltaTime, Data}) ->
    meta_io_list(DeltaTime, ?META_CUE, Data);
event_io_list({midi_chan_prefix, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_MIDI_CHAN_PREFIX, Data);
event_io_list({tempo, DeltaTime, [Data]}) ->
    ?DPRINT("tempo, data = ~p~n", [Data]),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, ?META_SET_TEMPO, var_len(3),
     (Data bsr 16) band 255,
     (Data bsr  8) band 255,
      Data         band 255];
event_io_list({smpte, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_SMPTE, Data);
event_io_list({time_signature, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_TIME_SIG, Data);
event_io_list({key_signature, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_KEY_SIG, Data);
event_io_list({sequencer_specific, DeltaTime, [Data]}) ->
    meta_io_list(DeltaTime, ?META_SEQUENCER_SPECIFIC, Data);
event_io_list({unknown_meta, DeltaTime, [Type, Data]}) ->
    meta_io_list(DeltaTime, Type, Data).

meta_io_list(DeltaTime, Type, Data) when is_binary(Data) ->
    ?DPRINT("meta_io_list (bin) type = ~p, data = ~p~n", [Type, Data]),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, Type, var_len(size(Data)), Data];
meta_io_list(DeltaTime, Type, Data) ->
    ?DPRINT("meta_io_list type = ~p, data = ~p~n", [Type, Data]),
    put(status, ?STATUS_META_EVENT),
    [var_len(DeltaTime), ?STATUS_META_EVENT, Type, var_len(length(Data)), Data].

running_status(HighNibble, Chan) ->
    RunningStatus = get(status),
    RunningChan = get(chan),
    if
	RunningStatus =:= HighNibble, RunningChan =:= Chan ->
	    ?DPRINT("running status: stat = ~p, rchan = ~p~n",
		    [RunningStatus, RunningChan]),
	    [];
	true ->
	    put(status, HighNibble),
	    put(chan, Chan),
	    (HighNibble bsl 4) + Chan
    end.

var_len(I) when I < (1 bsl 7) ->
    <<0:1, I:7>>;
var_len(I) when I < (1 bsl 14) ->
    <<1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) when I < (1 bsl 21) ->
    <<1:1, (I bsr 14):7, 1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) when I < (1 bsl 28) ->
    <<1:1, (I bsr 21):7, 1:1, (I bsr 14):7, 1:1, (I bsr 7):7, 0:1, I:7>>;
var_len(I) ->
    exit("Value " ++ I ++ " is too big for a variable length number").
