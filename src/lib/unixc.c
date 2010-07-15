/* $Id$ */

#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#ifdef SONYCISC
typedef long time_t;
typedef u_long clock_t;
extern char *getenv();
#else
#include <stdlib.h>
#endif

#ifdef _SC_CLK_TCK
#  define TICKS_PER_SECOND sysconf (_SC_CLK_TCK) /* POSIX 1003.1-1996 */
#else
#  ifdef CLK_TCK
#    define TICKS_PER_SECOND CLK_TCK /* POSIX 1003.1-1988; obsolescent */
#  else
#    define TICKS_PER_SECOND HZ /* traditional UNIX */
#  endif
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

static void (*Callback)(void);


#ifndef UNDERSCORE
void  dvdate (int4 *ndy,int4 *ndm,int4 *ndd,int4 *nth,int4 *ntm,int4 *nts)
#else
void  dvdate_(int4 *ndy,int4 *ndm,int4 *ndd,int4 *nth,int4 *ntm,int4 *nts)
#endif
{
	time_t nseconds;
	struct tm *ptr;
	
	nseconds = time(NULL);
	ptr = localtime(&nseconds);
	*ndy = (int4)(ptr->tm_year);
	*ndm = (int4)(ptr->tm_mon+1);
	*ndd = (int4)(ptr->tm_mday);
	*nth = (int4)(ptr->tm_hour);
	*ntm = (int4)(ptr->tm_min);
	*nts = (int4)(ptr->tm_sec);
}

#ifndef UNDERSCORE
void  dvtime (int4 *i,int4 *tick)
#else
void  dvtime_(int4 *i,int4 *tick)
#endif
{
	struct tms buffer;

 	times(&buffer);
	*i = (int4)buffer.tms_utime;
	*tick = (int4) TICKS_PER_SECOND;
}

#ifndef UNDERSCORE
void  dvtimes (int4 *iu,int4 *is,int4 *icu,int4 *ics,int4 *tick,int4 *ts,int4 *tus)
#else
void  dvtimes_(int4 *iu,int4 *is,int4 *icu,int4 *ics,int4 *tick,int4 *ts,int4 *tus)
#endif
{
	struct tms buffer;
	struct timeval tv;
	struct timezone tz;

 	times(&buffer);
	*iu = (int4)buffer.tms_utime;
	*is = (int4)buffer.tms_stime;
	*icu = (int4)buffer.tms_cutime;
	*ics = (int4)buffer.tms_cstime;
	*tick = (int4) TICKS_PER_SECOND;
	gettimeofday(&tv,&tz);
	*ts = (int4)tv.tv_sec;
	*tus = (int4)tv.tv_usec;
}

#ifndef UNDERSCORE
void  dvsleep (const int4 *it)
#else
void  dvsleep_(const int4 *it)
#endif
{
 	usleep(*it);
}

#ifndef UNDERSCORE
void  dvrand (int4 *i,int4 *k)
#else
void  dvrand_(int4 *i,int4 *k)
#endif
{
#ifndef SONYCISC
 	*i = rand();
	*k = RAND_MAX;
#else
 	*i = random();
	*k = 2147483647;
#endif
}

#ifndef UNDERSCORE
void  dvsrand (const int4 *i)
#else
void  dvsrand_(const int4 *i)
#endif
{
#ifndef SONYCISC
 	srand((unsigned int)*i);
#endif
}

#ifndef UNDERSCORE
void  dvflsh (void)
#else
void  dvflsh_(void)
#endif
{
	fflush(stdout);
}

#ifndef UNDERSCORE
void  dvrenv (int4 *iasc,const int4 *nchar)
#else
void  dvrenv_(int4 *iasc,const int4 *nchar)
#endif
{
	char *str;
	int i,len;
        int4 k;

	for(i = 0; i < *nchar; i++)
		iasc[i] = ' ';
	str = getenv("GSAF_PARM_TEMP");
        if(str) {
		len = strlen(str);
		if(len > *nchar)
			len = *nchar;
		for(i = 0; i < len; ++i) {
			k = (int4)str[i];
			if(k > ' ')
				iasc[i] = k;
		}
	}
}

#ifndef UNDERSCORE
void setquitcallback (void (*callback)(void))
#else
void setquitcallback_(void (*callback)(void))
#endif
{
	Callback = callback;
}

#ifndef UNDERSCORE
void resetquitcallback (void)
#else
void resetquitcallback_(void)
#endif
{
	Callback = NULL;
}

#ifndef UNDERSCORE
void callquitcallback (void)
#else
void callquitcallback_(void)
#endif
{
	if (Callback != NULL)
		Callback();
}
