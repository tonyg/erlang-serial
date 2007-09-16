/*    -*- C -*- 
 *    File:	 serial.h  (~jb/serialport/serial.h)
 *    Author:	 Johan Bevemyr
 *    Created:	 Sun Oct 20 01:36:03 1996
 *    Purpose:   
 */ 

#ifndef SERIAL_H
#define SERIAL_H

typedef enum {
  FALSE = 0,
  TRUE = 1
} boolean;

#define NULLFDS   ((struct fd_set *) 0)
#define NULLTV    ((struct timeval *) 0)
#define MAXLENGTH 1024

typedef struct {
  int rate;
  speed_t speed;
} bit_rate;

#define Max(A,B) (((A) > (B)) ? (A) : (B))
#define Min(A,B) (((A) < (B)) ? (A) : (B))

#define TtyOpen(TTY) ((TTY) != -1)

#define COMMANDPOS 2
#define COMMANDSIZE 1
#define HEADERSIZE 3
#define TBHSIZE 2

#define PacketType(MESSAGE) (MESSAGE[COMMANDPOS])

#define BREAKPERIOD 0

/* roland */
typedef enum {
  SEND=0,
  CONNECT=1,
  DISCONNECT=2,
  OPEN=3,
  CLOSE=4,
  SPEED=5,
  PARITY_ODD=6,
  PARITY_EVEN=7,
  BREAK=8
} command;

#ifdef DEBUG
# define Debug(STRING)            fprintf(stderr,STRING)
# define Debug1(STRING,Arg)       fprintf(stderr,STRING,Arg)
# define Debug2(STRING,Arg1,Arg2) fprintf(stderr,STRING,Arg1,Arg2)
#else
# define Debug(STRING)            
# define Debug1(STRING,Arg)       
# define Debug2(STRING,Arg1,Arg2) 
#endif

#endif /* SERIAL_H */
