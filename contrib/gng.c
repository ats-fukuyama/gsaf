/*
--------------------------------------------------------------
Contribution from takeno@eedept.eedept.kobe-u.ac.jp (93/08/12)
--------------------------------------------------------------
*/

#include <stdio.h>
#include <math.h>

#define		M_PI	3.14159265358979323846
#define		deg		*M_PI/180.
#define		X0		1000
#define		YM		14000

int			xpos,ypos;
float		delta;
char		file_basename[256];
FILE		*grafile,*parmfile;

#ifndef UNDERSCORE
  dvopen(ich)
#else
  dvopen_(ich)
#endif
long		*ich;
{	
	char	*strptr;
	char	str[256];
	int		len;
	
	delta = (float)(25600) / (1024*32) * 0.7;

	parmfile = fopen("GSAF.PARM.TEMP","r");
	if(parmfile) {
		fscanf(parmfile,"%s",str);
		if(str[0] == '\0')
		{
			printf("## Illegal gra file name ! \n");
			exit(0);
		}
		len = strlen(str);
		strncpy(file_basename,str+1,len-2);
	}
	else {
		printf(" ## OUTPUT: GRA FILE NAME :\n");
		strptr = gets(file_basename);
		if(*strptr == '\0')
		{
			printf("## Illegal gra file name ! \n");
			exit(0);
		}
	}
	*ich = 0;
}

#ifndef UNDERSCORE
  dvclos(ich)
#else
  dvclos_(ich)
#endif
long		*ich;
{
	*ich = 0;
}

#ifndef UNDERSCORE
  dvoptn(kopt,iopt)
#else
  dvoptn_(kopt,iopt)
#endif
char		*kopt;
long		*iopt;
{
}

#ifndef UNDERSCORE
  dvpags(npage,sizex,sizey,lkeep)
#else
  dvpags_(npage,sizex,sizey,lkeep)
#endif
long		*npage;
float		*sizex;
float		*sizey;
long		*lkeep;
{
	char	filename[256];
	
	sprintf(filename,"%s#%d.gra",file_basename,*npage);
	grafile = fopen(filename,"w");
	fprintf(grafile," Ngraph GRA file\n");
	fprintf(grafile,"Creator: Ngraph  ver 5.2\n");
	fprintf(grafile,"I,3,0,4000,21000,\n");
	fprintf(grafile,"V,4,0,4000,21000,25000,\n");
}

#ifndef UNDERSCORE
  dvpage(ich)
#else
  dvpage_(ich)
#endif
long		*ich;
{
	fprintf(grafile,"E,0,\n");
	fclose(grafile);
	*ich = 0;
}

#ifndef UNDERSCORE
  dvmove(ix,iy)
#else
  dvmove_(ix,iy)
#endif
long		*ix,*iy;
{
	xpos = X0 + *ix * delta;
	ypos = YM - *iy * delta;
	fprintf(grafile,"M,2,%d,%d,\n",xpos,ypos);
}

#ifndef UNDERSCORE
  dvdraw(ix,iy)
#else
  dvdraw_(ix,iy)
#endif
long		*ix,*iy;
{
	int		x,y;
	
	x = X0 + *ix * delta;
	y = YM - *iy * delta;
	fprintf(grafile,"T,2,%d,%d,\n",x,y);
	xpos = x;
	ypos = y;
}

#ifndef UNDERSCORE
  dvtext(ix,iy,iasc,nchar)
#else
  dvtext_(ix,iy,iasc,nchar)
#endif
long		*ix,*iy,*iasc,*nchar;
{
	fprintf(grafile,"%%,-1,call dvtext,\n");
}

#ifndef UNDERSCORE
  dvstln(iln,ibl,icl)
#else
  dvstln_(iln,ibl,icl)
#endif
long		*iln,*ibl,*icl;
{
	int		ils,ibs,ics;
	
	fprintf(grafile,"%%,-1,call dvstln,\n");
	
	switch(*iln)
	{
		case 0: ils = 0; break;
		case 1: ils = 1; break;
		case 2: ils = 2; break;
		case 3: ils = 2; break;
		case 4: ils = 3; break;
		case 5: ils = 3; break;
		case 6: ils = 3; break;
		case 7: ils = 3; break;
	}

	switch(*ibl)
	{
		case 0: ibs = 10; break;
		case 1: ibs = 20; break;
		case 2: ibs = 30; break;
		case 3: ibs = 40; break;
		case 4: ibs = 50; break;
		case 5: ibs = 60; break;
		case 6: ibs = 80; break;
		case 7: ibs = 100; break;
	}

	switch(*icl)
	{
		case 0: ics = 7; break;
		case 1: ics = 6; break;
		case 2: ics = 3; break;
		case 3: ics = 5; break;
		case 4: ics = 2; break;
		case 5: ics = 4; break;
		case 6: ics = 1; break;
		case 7: ics = 0; break;
	}

	fprintf(grafile,"A,4,%d,%d,%d,0,\n",ils,ibs,ics);

}

#ifndef UNDERSCORE
  dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
  dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
long		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
	fprintf(grafile,"%%,-1,call dvstch,\n");
	*ind = 1;
}

#ifndef UNDERSCORE
  dvchin(iasc,nchar)
#else
  dvchin_(iasc,nchar)
#endif
long		*iasc,*nchar;
{
	int		i;

	for(i=0; i < *nchar; i++) iasc[i]=32;
	fprintf(grafile,"%%,-1,call dvchin,\n");
}

#ifndef UNDERSCORE
  dvxyin(ix,iy)
#else
  dvxyin_(ix,iy)
#endif
long		*ix,*iy;
{
	*ix = (xpos - X0) / delta;
	*iy = (YM - ypos) / delta;
	fprintf(grafile,"%%,-1,call dvxyin,\n");
}

#ifndef UNDERSCORE
  dvgrmd()
#else
  dvgrmd_()
#endif
{
	fprintf(grafile,"%%,-1,call dvgrmd,\n");
}

#ifndef UNDERSCORE
  dvchmd()
#else
  dvchmd_()
#endif
{
	fprintf(grafile,"%%,-1,call dvchmd,\n");
}

#ifndef UNDERSCORE
  dveras()
#else
  dveras_()
#endif
{
	fprintf(grafile,"%%,-1,call dveras,\n");
}

#ifndef UNDERSCORE
  dvprnt()
#else
  dvprnt_()
#endif
{
	fprintf(grafile,"%%,-1,call dvprnt,\n");
}

#ifndef UNDERSCORE
  dvbell()
#else
  dvbell_()
#endif
{
	fprintf(grafile,"%%,-1,call dvbell,\n");
}
