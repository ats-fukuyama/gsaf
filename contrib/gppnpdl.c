/*
--------------------------------------------------------------
Contribution from takeno@eedept.eedept.kobe-u.ac.jp (93/08/12)
--------------------------------------------------------------
*/

#include <stdio.h>
#include <math.h>

#define 	M_PI	3.14159265358979323846
#define 	deg	*M_PI/180.
#define 	X0		144
#define 	YM		1872

int			xpos,ypos;
float		delta;
int			dxch,dych;
int			init;
int			icls;

putnum(i)
int		i;
{
	printf("%d",i);
}

#ifndef UNDERSCORE
  dvopen(ich)
#else
  dvopen_(ich)
#endif
long		*ich;
{	
	delta = (float)(25600)/(1024*32)/2540*240; /* 1in=25.40mm=240 dot */
	init = 1;
	*ich = 0;
}

#ifndef UNDERSCORE
  dvclos(ich)
#else
  dvclos_(ich)
#endif
long		*ich;
{
	fflush(stdout);
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
	if(init)
	{
		init = 0;
	}
	else
	{
		putchar(0x0c);
		putchar(0x1b);
		putchar('c');
		putchar('1');
	}
	
	putchar(0x1c);
	putchar('d');
	putchar('2');
	putchar('4');
	putchar('0');
	putchar('.');
	
	putchar(0x1c);
	putchar('f');
	putchar('L');
	putchar('A');
	putchar('4');
	putchar('.');
	
	putchar(0x1c);
	putchar('Y');
	
	putchar('I');
	putchar('N');
	putchar(';');
	
	putchar('P');
	putchar('D');
	putchar(';');
}

#ifndef UNDERSCORE
  dvpage(ich)
#else
  dvpage_(ich)
#endif
long		*ich;
{
	putchar(0x1c);
	putchar('Z');
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
	putchar('M');
	putchar('A');
	putnum(xpos);
	putchar(',');
	putnum(ypos);
	putchar(';');
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
	if(icls != 0)
	{	
		putchar('P');
		putchar('R');
		putnum(x-xpos);
		putchar(',');
		putnum(y-ypos);
		putchar(';');
	}
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
}

#ifndef UNDERSCORE
  dvstln(iln,ibl,icl)
#else
  dvstln_(iln,ibl,icl)
#endif
long		*iln,*ibl,*icl;
{
	putchar('L');
	putchar('T');
	switch(*iln)
	{
		case 0:               break;
		case 1: putchar('1');
				putchar(',');
				putchar('1'); break;
		case 2: putchar('2');
				putchar(',');
				putchar('1'); break;
		case 3: putchar('4');
				putchar(',');
				putchar('2'); break;
		case 4: putchar('5');
				putchar(',');
				putchar('1'); break;
		case 5: putchar('5');
				putchar(',');
				putchar('2'); break;
		case 6: putchar('6');
				putchar(',');
				putchar('1'); break;
		case 7: putchar('6');
				putchar(',');
				putchar('2'); break;
	}
    putchar(';');

	putchar('L');
	putchar('W');
	switch(*ibl)
	{
		case 0: putnum(1); break;
		case 1: putnum(2); break;
		case 2: putnum(3); break;
		case 3: putnum(4); break;
		case 4: putnum(5); break;
		case 5: putnum(6); break;
		case 6: putnum(7); break;
		case 7: putnum(8); break;
	}
	putchar(';');
	icls = *icl;
}

#ifndef UNDERSCORE
  dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
  dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
long		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
	dxch = *ichsp * cos(*angl deg) * delta;
	dych = *ichsp * sin(*angl deg) * delta;
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
}

#ifndef UNDERSCORE
  dvgrmd()
#else
  dvgrmd_()
#endif
{
}

#ifndef UNDERSCORE
  dvchmd()
#else
  dvchmd_()
#endif
{
}

#ifndef UNDERSCORE
  dveras()
#else
  dveras_()
#endif
{
}

#ifndef UNDERSCORE
  dvprnt()
#else
  dvprnt_()
#endif
{
}

#ifndef UNDERSCORE
  dvbell()
#else
  dvbell_()
#endif
{
}
