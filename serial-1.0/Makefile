#  File:	 Makefile
#  Author:	 Johan Bevemyr
#  Created:	 Fri Oct 18 09:59:34 1996
 

CC = gcc
# roland
LINK.c = gcc

WARNING_OPTIONS =
LANGUAGE_OPTIONS = 
COMPILER_OPTIONS = -g 

CFLAGS   = $(WARNING_OPTIONS) $(LANGUAGE_OPTIONS) $(COMPILER_OPTIONS)

######################################################################

HEADER_FILES = serial.h
SOURCE_FILES = serial.c

OBJECT_FILES = $(SOURCE_FILES:.c=.o)

######################################################################

serial : $(OBJECT_FILES)
	$(LINK.c) -o $@ $(LDFLAGS) $(OBJECT_FILES) $(LDLIBS)

clean : 
	/bin/rm -f $(OBJECT_FILES)

# roland
serial.o: serial.c serial.h

realclean: clean
	/bin/rm -f serial

$(OBJECT_FILES): $(HEADER_FILE)

