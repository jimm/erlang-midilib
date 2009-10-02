% Channel messages
-define(STATUS_NIBBLE_OFF, 16#8).
-define(STATUS_NIBBLE_ON, 16#9).
-define(STATUS_NIBBLE_POLY_PRESS, 16#A).
-define(STATUS_NIBBLE_CONTROLLER, 16#B).
-define(STATUS_NIBBLE_PROGRAM_CHANGE, 16#C).
-define(STATUS_NIBBLE_CHANNEL_PRESSURE, 16#D).
-define(STATUS_NIBBLE_PITCH_BEND, 16#E).

% System common messages
-define(STATUS_SYSEX, 16#F0).
-define(STATUS_SONG_POINTER, 16#F2).
-define(STATUS_SONG_SELECT, 16#F3).
-define(STATUS_TUNE_REQUEST, 16#F6).
-define(STATUS_EOX, 16#F7).

% System realtime messages
% MIDI clock (24 per quarter note)
-define(STATUS_CLOCK, 16#F8).
% Sequence start
-define(STATUS_START, 16#FA).
% Sequence continue
-define(STATUS_CONTINUE, 16#FB).
% Sequence stop
-define(STATUS_STOP, 16#FC).
% Active sensing (sent every 300 ms when nothing else being sent)
-define(STATUS_ACTIVE_SENSE, 16#FE).
% System reset
-define(STATUS_SYSTEM_RESET, 16#FF).

% Meta events
-define(STATUS_META_EVENT, 16#FF).
-define(META_SEQ_NUM, 16#00).
-define(META_TEXT, 16#01).
-define(META_COPYRIGHT, 16#02).
-define(META_SEQ_NAME, 16#03).
-define(META_INSTRUMENT, 16#04).
-define(META_LYRIC, 16#05).
-define(META_MARKER, 16#06).
-define(META_CUE, 16#07).
-define(META_MIDI_CHAN_PREFIX, 16#20).
-define(META_TRACK_END, 16#2f).
-define(META_SET_TEMPO, 16#51).
-define(META_SMPTE, 16#54).
-define(META_TIME_SIG, 16#58).
-define(META_KEY_SIG, 16#59).
-define(META_SEQUENCER_SPECIFIC, 16#7F).
