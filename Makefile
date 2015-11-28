## Copyright (c) 1996, 1999 Johan Bevemyr
## Copyright (c) 2007, 2009 Tony Garnock-Jones
## 
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
## 
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
## 
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
## THE SOFTWARE.
##
#  File:	 Makefile
#  Author:	 Johan Bevemyr
#  Created:	 Fri Oct 18 09:59:34 1996

VSN = 1.1
INSTALL_DIR=serial-$(VSN)
FULL_INSTALL_DIR=$(DESTDIR)/erlang/lib/$(INSTALL_DIR)

WARNING_OPTIONS =
LANGUAGE_OPTIONS = 
COMPILER_OPTIONS = -g 

CFLAGS += $(WARNING_OPTIONS) $(LANGUAGE_OPTIONS) $(COMPILER_OPTIONS)

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
