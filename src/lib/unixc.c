/* $Id$ */
#include <sys/types.h>
#include <sys/times.h>
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

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#ifndef UNDERSCORE
void  dvdate(ndy,ndm,ndd,nth,ntm,nts)
#else
void  dvdate_(ndy,ndm,ndd,nth,ntm,nts)
#endif
int4 *ndy,*ndm,*ndd,*nth,*ntm,*nts;
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
void  dvtime(i,tick)
#else
void  dvtime_(i,tick)
#endif
int4 *i;
int4 *tick;
{
	struct tms buffer;

 	times(&buffer);
	*i = (int4)buffer.tms_utime;
	*tick = (int4)CLK_TCK;
}

#ifndef UNDERSCORE
void  dvsleep(it)
#else
void  dvsleep_(it)
#endif
int4 *it;
{
	long i;

	i=*it;
 	usleep(i);
}

#ifndef UNDERSCORE
void  dvrand(i,k)
#else
void  dvrand_(i,k)
#endif
int4 *i,*k;
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
void  dvsrand(i)
#else
void  dvsrand_(i)
#endif
int4 *i;
{
#ifndef SONYCISC
 	srand((unsigned int)*i);
#endif
}

#ifndef UNDERSCORE
void  dvflsh()
#else
void  dvflsh_()
#endif
{
	fflush(stdout);
}

#ifndef UNDERSCORE
void  dvrenv(iasc,nchar)
#else
void  dvrenv_(iasc,nchar)
#endif
int4 *iasc,*nchar;
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
