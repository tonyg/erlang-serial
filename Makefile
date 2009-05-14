#  File:	 Makefile
#  Author:	 Johan Bevemyr
#  Created:	 Fri Oct 18 09:59:34 1996

VSN = 1.1
INSTALL_DIR=serial-$(VSN)
FULL_INSTALL_DIR=$(DESTDIR)/erlang/lib/$(INSTALL_DIR)

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

install: all
	@[ -n "$(DESTDIR)" ] || (echo "Set DESTDIR before running the install target."; false)
	install -d $(FULL_INSTALL_DIR)/ebin
	install -d $(FULL_INSTALL_DIR)/priv/bin
	install -d $(FULL_INSTALL_DIR)/src
	install -m 644 ebin/* $(FULL_INSTALL_DIR)/ebin
	install -m 755 priv/bin/* $(FULL_INSTALL_DIR)/priv/bin
	install -m 644 src/* $(FULL_INSTALL_DIR)/src

ebin/%.beam: src/%.erl ebin
	erlc -o ebin $<

ebin:
	mkdir -p ebin

priv/bin:
	mkdir -p priv/bin

priv/bin/serial: $(OBJECT_FILES) priv/bin
	mkdir -p priv/bin
	$(CC) -o $@ $(LDFLAGS) $(OBJECT_FILES) $(LDLIBS)

clean:
	rm -f priv/bin/serial $(OBJECT_FILES) $(BEAM_FILES)

serial.o: serial.c serial.h

echo-version:
	@echo $(VSN)
