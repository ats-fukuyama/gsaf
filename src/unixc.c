#include <sys/types.h>
#include <sys/times.h>
#include <stdio.h>
#include <time.h>

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
  dvdate(ndy,ndm,ndd,nth,ntm,nts)
#else
  dvdate_(ndy,ndm,ndd,nth,ntm,nts)
#endif
int4 *ndy,*ndm,*ndd,*nth,*ntm,*nts;
{
	time_t nseconds;
	struct tm *ptr;
	
	nseconds = time(NULL);
	ptr = localtime(&nseconds);
	*ndy = (int4)(ptr->tm_year);
	*ndm = (int4)(ptr->tm_mon)+1;
	*ndd = (int4)(ptr->tm_mday);
	*nth = (int4)(ptr->tm_hour);
	*ntm = (int4)(ptr->tm_min);
	*nts = (int4)(ptr->tm_sec);
}

#ifndef UNDERSCORE
  dvtime(i,tick)
#else
  dvtime_(i,tick)
#endif
int4 *i;
int4 *tick;
{
	struct tms buffer;
	clock_t j;

 	times(&buffer);
	j =  buffer.tms_utime;
	*i = (int4)j; 
	*tick = (int4)CLK_TCK;
}

#ifndef UNDERSCORE
  dvsleep(it)
#else
  dvsleep_(it)
#endif
int4 *it;
{
	long i;

	i=*it;
 	usleep(i);
}

#ifndef UNDERSCORE
  dvrand(i,k)
#else
  dvrand_(i,k)
#endif
int4 *i,*k;
{
#ifndef SONYCISC
	unsigned j;
 	*i =  rand();
	*k = RAND_MAX;
#else
	long j;
 	*i =  random();
	*k = 2147483647;
#endif
}

#ifndef UNDERSCORE
  dvsrand(i)
#else
  dvsrand_(i)
#endif
int4 *i;
{
#ifndef SONYCISC
	unsigned int k;
	k = *i;
 	srand(k);
#endif
}

#ifndef UNDERSCORE
  dvflsh()
#else
  dvflsh_()
#endif
{
	fflush(stdout);
}

#ifndef UNDERSCORE
  dvrenv(iasc,nchar)
#else
  dvrenv_(iasc,nchar)
#endif
int4 *iasc,*nchar;
{
	char *str;
	int i,len;
        int4 k;

	for(i = 0; i < *nchar; i++)
	   iasc[i] = ' ';
	str = getenv("GSAF_PARM_TEMP");
        if(str){
	   len = strlen(str);
	   if(len > *nchar)
	     len = *nchar;
	   for(i = 0; i < len; ++i)
             {    
               k = (int4)str[i];
               if(k < 32)
                 k = 32;
               iasc[i] = k;
             }
	 }
}

