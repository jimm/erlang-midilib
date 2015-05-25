CORE_MIDI_LIB = CoreMIDI
MODS = core_midi

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<


all: 	 ${MODS:%=%.beam} core_midi.hrl core_midi

core_midi:  core_midi.c core_midi.h erl_comm.c erl_comm.h types.h
	gcc -framework $(CORE_MIDI_LIB) -o core_midi core_midi.c erl_comm.c

core_midi.erl:	core_midi.hrl

core_midi.hrl:	core_midi.h
	cat core_midi.h | ruby -p -e 'gsub(/#define (\S+)\s+(.*)/, "-define(\\1, \\2)."); gsub(%r{/\*}, "%%"); gsub(%r{ *\*/}, "")' >core_midi.hrl
	echo '#include <stdlib.h>' >void_size.c
	echo '#include <stdio.h>' >>void_size.c
	echo 'int main() { printf("-define(REF_SIZE, %ld).\\n", sizeof(void *)); exit(0); return 0; }' >>void_size.c
	gcc -o void_size void_size.c
	echo >>core_midi.hrl
	./void_size >>core_midi.hrl
	rm void_size void_size.c

clean:
	rm -f core_midi *.beam core_midi.hrl void_size void_size.c
