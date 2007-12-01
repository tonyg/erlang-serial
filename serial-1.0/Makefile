#  File:	 Makefile
#  Author:	 Johan Bevemyr
#  Created:	 Fri Oct 18 09:59:34 1996
 

WARNING_OPTIONS =
LANGUAGE_OPTIONS = 
COMPILER_OPTIONS = -g 

CFLAGS   = $(WARNING_OPTIONS) $(LANGUAGE_OPTIONS) $(COMPILER_OPTIONS)

######################################################################

HEADER_FILES = serial.h
SOURCE_FILES = serial.c

OBJECT_FILES = $(SOURCE_FILES:.c=.o)

######################################################################

ERL_FILES = $(wildcard *.erl)
BEAM_FILES = $(patsubst %.erl, ../ebin/%.beam, $(ERL_FILES))

######################################################################

all: ../priv/bin/serial $(BEAM_FILES)

../ebin/%.beam: %.erl
	erlc -o ../ebin $<

../priv/bin/serial: serial
	cp serial ../priv/bin

serial : $(OBJECT_FILES)
	$(CC) -o $@ $(LDFLAGS) $(OBJECT_FILES) $(LDLIBS)

clean : 
	/bin/rm -f serial ../priv/bin/serial $(OBJECT_FILES) $(BEAM_FILES)

# roland
serial.o: serial.c serial.h

realclean: clean
	/bin/rm -f serial

$(OBJECT_FILES): $(HEADER_FILE)

