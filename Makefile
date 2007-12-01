#  File:	 Makefile
#  Author:	 Johan Bevemyr
#  Created:	 Fri Oct 18 09:59:34 1996

WARNING_OPTIONS =
LANGUAGE_OPTIONS = 
COMPILER_OPTIONS = -g 

CFLAGS   = $(WARNING_OPTIONS) $(LANGUAGE_OPTIONS) $(COMPILER_OPTIONS)

######################################################################

HEADER_FILES = c_src/serial.h
SOURCE_FILES = c_src/serial.c

OBJECT_FILES = $(SOURCE_FILES:.c=.o)

######################################################################

ERL_FILES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl, ebin/%.beam, $(ERL_FILES))

######################################################################

all: priv/bin/serial $(BEAM_FILES)

ebin/%.beam: src/%.erl
	erlc -o ebin $<

priv/bin/serial: $(OBJECT_FILES)
	$(CC) -o $@ $(LDFLAGS) $(OBJECT_FILES) $(LDLIBS)

clean:
	rm -f priv/bin/serial $(OBJECT_FILES) $(BEAM_FILES)

serial.o: serial.c serial.h
