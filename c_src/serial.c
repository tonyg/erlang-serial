/*
Copyright (c) 1996, 1999 Johan Bevemyr
Copyright (c) 2007, 2009 Tony Garnock-Jones

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/*    -*- C -*- 
 *    File:	 serial.c  (~jb/serialport/serial.c)
 *    Author:	 Johan Bevemyr
 *    Created:	 Fri Oct 18 09:59:34 1996
 *    Purpose:   Provide Erlang with access to the serial port.
 */ 

/* This program communicates through the following protocoll when
 * run with -erlang:
 *
 *   (all messages starts with a two byte header containing the
 *   size of the message)
 *
 *   SEND DATA
 *        Transmits DATA on the currently open tty
 * 
 *   CONNECT     
 *        Reopens a disconnected connection.
 *
 *   DISCONNECT
 *        Hangs up an open serial line
 * 
 *   OPEN path 
 *        Opens a new tty, closing the old (but not disconnecting). It
 *        is possible to alternate between two connections. 
 *
 *   SPEED inspeed outspeed
 *        Sets the input and output speeds. New connections will
 *        have these speeds. It is also possible to change speed
 *        on the fly.
 *
 *   PARITY ODD/EVEN
 *        Sets the parity of the current connection.
 *
 *   BREAK
 *        Sends break.
 *   
 *   usage: serial [-debug] [-cbreak] [-erlang] [-speed <bit rate>] [-tty <dev>]
 *          bit rate is one of 
 *                  50      75      110
 *                  134     150     200
 *                  300     600     1200
 *                  1800    2400    4800
 *                  9600    19200   38400
 *                  57600   115200  230400
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "serial.h"

int errno;

#define MAXSPEED 23
bit_rate bitrate_table[MAXSPEED] = {
  {0      , B0	   },
  {50     , B50	   },
  {75     , B75	   },
  {110    , B110   },
  {134    , B134   },
  {150    , B150   },
  {200    , B200   },
  {300    , B300   },
  {600    , B600   },
  {1200   , B1200  },
  {1800   , B1800  },
  {2400   , B2400  },
  {4800   , B4800  },
  {9600   , B9600  },
  {19200  , B19200 },	
  {38400  , B38400 },	
  {57600  , B57600 },	
  {115200 , B115200 }, 	
  {230400 , B230400 }
};

/**********************************************************************
 * Name: get_speed
 *
 * Desc: Returns the speed_t value associated with a given bit_rate
 *       according to the bitrate_table. B0 is returned if no matching entry 
 *       is found.
 */

speed_t get_speed(int speed)
{
  int i;
  
  for(i=0 ; i < MAXSPEED ; i++)
    {
      if (speed == bitrate_table[i].rate)
	break;
    }
  
  if (i == MAXSPEED)
    return B0;
  else
    return bitrate_table[i].speed;
}
  
/**********************************************************************
 * Name: set_raw_tty_mode
 *
 * Desc: Configures the given tty for raw-mode.
 */

void set_raw_tty_mode(int fd)
{
  struct termios ttymodes;

  /* Get ttymodes */

  if (tcgetattr(fd,&ttymodes) < 0) 
    {
      perror("tcgetattr");
      exit(1);
    }

  /* Configure for raw mode (see man termios) */
  ttymodes.c_cc[VMIN] = 1;         /* at least one character */
  ttymodes.c_cc[VTIME] = 0;        /* do not wait to fill buffer */

  ttymodes.c_iflag &= ~(ICRNL |    /* disable CR-to-NL mapping */
			INLCR |    /* disable NL-to-CR mapping */
			IGNCR |    /* disable ignore CR */
			ISTRIP |   /* disable stripping of eighth bit */
			IXON |     /* disable output flow control */
			BRKINT |   /* disable generate SIGINT on brk */
			IGNPAR |
			PARMRK |
			IGNBRK |
			INPCK);    /* disable input parity detection */

  ttymodes.c_lflag &= ~(ICANON |   /* enable non-canonical mode */
			ECHO |     /* disable character echo */
			ECHOE |    /* disable visual erase */
			ECHOK |    /* disable echo newline after kill */
			ECHOKE |   /* disable visual kill with bs-sp-bs */
			ECHONL |   /* disable echo nl when echo off */
			ISIG | 	   /* disable tty-generated signals */
			IEXTEN);   /* disable extended input processing */
  
  ttymodes.c_cflag |= CS8;         /* enable eight bit chars */
  ttymodes.c_cflag &= ~PARENB;     /* disable input parity check */

  ttymodes.c_oflag &= ~OPOST;      /* disable output processing */

  /* roland /
  ttymodes.c_cflag |= CLOCAL;



  /* Apply changes */

  if (tcsetattr(fd, TCSAFLUSH, &ttymodes) < 0)
    {
      perror("tcsetattr");
      exit(1);
    }
}

/**********************************************************************
 * Name: set_tty_speed
 *
 * Desc: set input and output speeds of a given connection.
 */

void set_tty_speed(int fd, speed_t new_ispeed, speed_t new_ospeed)
{
  struct termios ttymodes;

  /* Get ttymodes */

  if (tcgetattr(fd,&ttymodes) < 0) 
    {
      perror("tcgetattr");
      exit(1);
    }

  if (cfsetispeed(&ttymodes,new_ispeed) < 0)
    {
      perror("cfsetispeed");
      exit(1);
    }

  if (cfsetospeed(&ttymodes,new_ospeed) < 0)
    {
      perror("cfsetospeed");
      exit(1);
    }

  ttymodes.c_cflag |= CRTSCTS;     /* enable RTS/CTS flow control */

  /* Apply hanges */

  if (tcsetattr(fd, TCSAFLUSH, &ttymodes) < 0)
    {
      perror("tcsetattr");
      exit(1);
    }
}

/**********************************************************************
 * Name: get_tbh_size
 * Desc: returns the size of a two_byte_header message (from Erlang).
 */

int get_tbh_size(unsigned char buf[])
{
  return (((int) buf[0]) << 8) + ((int) buf[1]);
}

/**********************************************************************
 * Name: set_tbh_size
 * Desc: sets the first two bytes of the buffer to its size
 */

void set_tbh_size(unsigned char buf[], int size)
{
  buf[1] = (unsigned char) (size & 0xff);
  buf[0] = (unsigned char) ((size >> 8) & 0xff);
  return;
}

/**********************************************************************
 * Name: tbh_write
 * Desc: writes the buffer to a file descriptor, adding size info
 *       at the beginning.
 */

void tbh_write(int fd, unsigned char buf[], int buffsize)
{
  char header_buf[TBHSIZE];

  Debug1("tbh_write: send message of size %d\r\n", buffsize);

  /* First, write two byte header */
  set_tbh_size(header_buf, buffsize);
  write(fd,header_buf,TBHSIZE);

  /* Second, write original buffer */
  write(fd,buf,buffsize);

  return;
}

/**********************************************************************
 * Name: tbh_read
 * Desc: Reads one message with two-byte-header, filling buffer.
 *       Returns the number of elements used in the buffer, or 0
 *       if the input file has been closed. 
 *
 */

int tbh_read(int fd, unsigned char buf[], int buffsize)
{
  int remaining, msgsize;

  if (read_at_least(fd,buf,TBHSIZE) != TBHSIZE) 
    return 0;

  remaining = get_tbh_size(buf);

  Debug1("tbh_read: got message of size %d\r\n",remaining);

  msgsize = read_at_least(fd, &buf[TBHSIZE],
			  Min(remaining,(buffsize-TBHSIZE)));

  if (msgsize == 0)
    return 0;
  else
    return msgsize + TBHSIZE;
}

/**********************************************************************
 * Name: read_at_least(fd,buf,nr)
 * Desc: Read at least nr bytes and put them into buf. Return the number
 *       of bytes read, i.e. nr.
 * Returns: The number of bytes read, or 0 if stream closed.
 */

int read_at_least(int fd, unsigned char buf[], int nr)
{
  int remaining = nr;
  int nr_read = 0;

  while(remaining > 0)
    {
      int read_this_time;

      read_this_time = read(fd, &buf[nr_read], remaining);

      if (read_this_time == 0) /* Input stream closed? */
	return 0;

      nr_read   += read_this_time;
      remaining -= read_this_time;
    }

  return nr_read;
}

/**********************************************************************
 * Name: write_to_tty
 * Desc: write a number of bytes found in the buffer to the tty, 
 *       filling the buffer from the given fillfd if neccessary.
 *
 */

void write_to_tty(int ttyfd, int fillfd, int totalsize, int buffsize,
		  unsigned char buf[], int buffmaxsize)
{
  write(ttyfd,buf,buffsize);
  totalsize -= buffsize;

  while(totalsize > 0)
    {
      int readmax;

      readmax = Min(totalsize,buffmaxsize);
      buffsize = read(fillfd,buf,readmax);
      write(ttyfd,buf,buffsize);
      totalsize -= buffsize;
    }

  return;
}

/**********************************************************************/

int Debug_Enabled = FALSE;

main(int argc, char *argv[])
{
  int            ttyfd = -1;           /* terminal file descriptor */
  int            stdinfd;              /* user file descriptor     */
  int            stdoutfd;             /* user out file descriptor */
  boolean        cbreak=FALSE;         /* cbreak flag              */
  boolean        erlang=FALSE;         /* talking to erlang flag   */
  speed_t        in_speed=B9600;       /* default in speed         */
  speed_t        out_speed=B9600;      /* default out speed        */
  char           ttyname[MAXPATHLEN];  /* terminal name            */

  strcpy(ttyname,"/dev/ttyS0");

  /****************************************
   * Process command line arguments
   */

  { 
    int i;

    for(i = 1 ; i < argc ; i++)
      {
	if (strcmp(argv[i],"-cbreak") == 0)         /* -cbreak */
	  {
	    cbreak = TRUE;
	  }
	else if (strcmp(argv[i],"-debug") == 0)         /* -debug */
	  {
	    Debug_Enabled = TRUE;
	  }
	else if (strcmp(argv[i],"-speed") == 0)     /* -speed  */
	  {
	    i += 1;
	    if (i < argc)
	      {
		out_speed = in_speed = get_speed(atoi(argv[i]));
		if (in_speed == B0)
		  goto error_usage;
	      }
	    else
	      goto error_usage;
	  }
	else if (strcmp(argv[i],"-tty") == 0)       /* -tty    */
	  {
	    i += 1;
	    if (i < argc)
	      {
		strncpy(ttyname,argv[i],MAXPATHLEN-1);
	      }
	    else
	      goto error_usage;
	  }
	else if (strcmp(argv[i],"-erlang") == 0)    /* -erlang */
	  {
	    erlang = TRUE;
	  }
	else
	  goto error_usage;
      }
  }

  /****************************************
   * Configure serial port (tty)
   */

  if (!erlang)
    {
      ttyfd = open(ttyname,O_RDWR);
      if (!TtyOpen(ttyfd))
	{
	  fprintf(stderr,"Cannot open terminal %s for read and write\n",
		  ttyname);
	  exit(1);
	}
  
      set_raw_tty_mode(ttyfd);
      set_tty_speed(ttyfd,in_speed,out_speed);
    }

  /****************************************
   * Configure user port
   */

  stdinfd = fileno(stdin);
  stdoutfd = fileno(stdout);

  if (cbreak)
    {
      sigset_t sig, savesig;

      /* Use non-cononical mode for input */
      set_raw_tty_mode(stdinfd);
      fprintf(stderr,"Entering non-canonical mode, exit with ---\n");
    }
  
  /****************************************
   * Start processing loop
   */

  {
    fd_set readfds;           /* file descriptor bit field for select */
    int    maxfd;             /* max file descriptor for select */
    unsigned char buf[MAXLENGTH];    /* buffer for transfer between serial-user */
    int    escapes;           /* number of consecutive escapes in cbreak */

    /* Set up initial bit field for select */
    maxfd = Max(stdinfd,ttyfd);
    FD_ZERO(&readfds);

    /* no escapes encountered yet */
    escapes = 0;
    
    while (TRUE)
      {
	int i;

	if(TtyOpen(stdinfd))
	  FD_SET(stdinfd, &readfds);
	if(TtyOpen(ttyfd))
	  FD_SET(ttyfd, &readfds);
	
	i = select(maxfd+1, &readfds, NULLFDS, NULLFDS, NULLTV);

	if (i <= 0)
	  {
	    perror("select");
	    exit(1);
	  }
	
	/******************************
	 * Data from TTY
	 */
	if (TtyOpen(ttyfd) &&
	    FD_ISSET(ttyfd,&readfds))        /* from serial port */
	  {
	    int nr_read;

	    Debug("receiving from TTY\r\n");

	    FD_CLR(ttyfd,&readfds);

	    nr_read = read(ttyfd,buf,MAXLENGTH);

	    if (nr_read <= 0)
	      {
		fprintf(stderr,"problem reading from tty\n");
		exit(1);
	      }

	    if (erlang)
	      tbh_write(stdoutfd,buf,nr_read);
	    else
	      write(stdoutfd,buf,nr_read);
	      
	  }
	
	/******************************
	 * Data from controlling process
	 */
	if (TtyOpen(stdinfd) &&
	    FD_ISSET(stdinfd,&readfds))       /* from user */
	  {
	    int nr_read;
	    int i;

	    FD_CLR(stdinfd,&readfds);

	    /********************
	     * check for escape in cbreak mode
	     */
	    if (cbreak)
	      {
		nr_read = read(stdinfd,buf,MAXLENGTH);

		for(i=0 ; i<nr_read ; i++) 
		  {
		    if(buf[i] == '-')
		      {
			escapes++;
			if(escapes == 3)
			  {
			    close(ttyfd);
			    exit(1);
			  }
		      }
		    else
		      {
			escapes=0;
		      }
		  }
		if (TtyOpen(ttyfd))
		  write(ttyfd,buf,nr_read);
	      }
	    /********************
	     * Erlang mode
	     */
	    else if (erlang)
	      {
		/* Messages from Erlang are structured as:
		 *   Length:16
		 *   PacketType:8
		 *   DATA
		 */

		nr_read = tbh_read(stdinfd,buf,MAXLENGTH);

		/* Check if stdin closed, i.e. controlling
		 * process terminated.
		 */
		if (nr_read == 0)
		  exit(1);

		/* Interpret packets from Erlang
		 */
		switch(PacketType(buf))
		  {
		  case SEND:	   /******************************/
		    Debug("received SEND\r\n");
		    if (TtyOpen(ttyfd))
		      {
			write_to_tty(ttyfd, stdinfd,
				     get_tbh_size(buf) - COMMANDSIZE,
				     nr_read - HEADERSIZE,
				     &(buf[HEADERSIZE]),
				     MAXLENGTH-HEADERSIZE);
		      }
		    break;

		  case CONNECT:    /******************************/
		    Debug("received CONNECT\r\n");
		    /* Reopen the current terminal */
		    goto open;
		    break;

		  case DISCONNECT: /******************************/
		    Debug("received DISCONNECT\r\n");
		    if (TtyOpen(ttyfd))
		      set_tty_speed(ttyfd,B0,B0);
		    goto close;
		    break;

		  case OPEN:	   /******************************/
		    Debug("received OPEN ");
		    /* Terminate string */
		    buf[nr_read] = '\0';
		    strcpy(ttyname,&buf[HEADERSIZE]);

		  open:
		    Debug1("opening %s \r\n",ttyname);

		    if (TtyOpen(ttyfd))
		      close(ttyfd);

		    ttyfd = open(ttyname,O_RDWR);
		    maxfd = Max(stdinfd,ttyfd);

		    if (!TtyOpen(ttyfd))
		      {
			fprintf(stderr,"Cannot open terminal %s for read ",
				&buf[HEADERSIZE]);
			fprintf(stderr,"and write\n");
			exit(1);
		      }

		    set_raw_tty_mode(ttyfd);
		    set_tty_speed(ttyfd,in_speed,out_speed);
		    break;

		  case CLOSE:	   /******************************/
		    Debug("received CLOSE\r\n");
		  close:
		    if (TtyOpen(ttyfd))
		      close(ttyfd);
		    ttyfd = -1;
		    break;

		  case SPEED:	   /******************************/
		    {
		      int off;
 
		      in_speed = get_speed(atoi(&buf[HEADERSIZE]));

		      /* Null-terminate string */
		      buf[nr_read] = '\0';

		      /* Find start of second speed */
		      for(off=HEADERSIZE ;
			  isdigit(buf[off]) && (off < MAXLENGTH) ;
			  off += 1);

		      out_speed = get_speed(atoi(&buf[off]));

		      Debug1("     raw SPEED %s\r\n",&buf[HEADERSIZE]);
		      Debug2("received SPEED %ud %ud\r\n",
			     (unsigned int) in_speed,
			     (unsigned int) out_speed);

		      if(TtyOpen(ttyfd))
			set_tty_speed(ttyfd, in_speed, out_speed);
		      break;
		    }

		  case PARITY_ODD: /******************************/
		    break;

		  case PARITY_EVEN:/******************************/
		    break;

		  case BREAK:      /******************************/
		    if (TtyOpen(ttyfd))
		      (void) tcsendbreak(ttyfd,BREAKPERIOD);
		    break;

		  default:
		    fprintf(stderr,"%s: unknown command from Erlang\n",
			    argv[0]);
		    break;
		  }
	      }
	    else
	      {
		nr_read = read(stdinfd,buf,MAXLENGTH);
		write(ttyfd,buf,nr_read);
	      }

	    if (nr_read <= 0)
	      {
		fprintf(stderr,"problem reading from stdin\n");
		exit(0);
	      }
	  }
      }
  }

  /****************************************
   * Usage errors
   */

 error_usage:
  fprintf(stderr,"usage: %s [-cbreak] [-erlang] [-speed <bit rate>] [-tty <dev>]\n",argv[0]);
  fprintf(stderr,"\tbit rate is one of \n\t\t50\t75\t110\n\t\t");
  fprintf(stderr,"134\t150\t200\n\t\t300\t");
  fprintf(stderr,"600\t1200\n\t\t1800\t2400\t4800\n\t\t");
  fprintf(stderr,"9600\t19200\t38400\n\t\t57600\t");
  fprintf(stderr,"115200\t230400\n");

  exit(0);
}
