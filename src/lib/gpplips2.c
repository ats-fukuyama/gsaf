/* $Id$ */
/* 
   GSAF printer driver for LIPS II 
   Version V3.51  1999/07/27
*/
#include <stdio.h>
#include <math.h>

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#ifdef SONYCISC
#define M_PI            3.14159265358979323846
#endif

#define 	deg	*M_PI/180.

int		xpos,ypos;
float		delta,factorX,factorY,offsetX,offsetY;
float           deltad;
int		dxch,dych;
int		init;
int		icls;

putnum(i)
int		i;
{
	int		iabs;
	
	iabs = abs(i);
	if(iabs >= 0x10000)
	{
		putchar(0x40 | (iabs >> 16));
		iabs = iabs & 0xffff;
		putchar(0x40 | (iabs >> 10));
		iabs = iabs & 0x03ff;
		putchar(0x40 | (iabs >> 4));
	}
	else
	{
		if(iabs >= 0x400)
		{
			putchar(0x40 | (iabs >> 10));
			iabs = iabs & 0x03ff;
			putchar(0x40 | (iabs >> 4));
		}
		else
		{
			if(iabs >= 0x10)
				putchar(0x40 | (iabs >> 4));
		}
	}
	iabs = iabs & 0x000f;
	if(i >= 0)
	{
		putchar(0x30 | iabs);
	}
	else
	{
		putchar(0x20 | iabs);
	}
}
			
#ifndef UNDERSCORE
  dvtype(ich)
#else
  dvtype_(ich)
#endif
int4		*ich;
{
	*ich=0;
}

#ifndef UNDERSCORE
  dvopen(ich)
#else
  dvopen_(ich)
#endif
int4		*ich;
{	
	delta = (float)(25600) / (1024*32);
	deltad = (float)(256) / (1024*32) * 240/25.4;
	factorX = 1.0;
	factorY = 1.0;
	offsetX = 1.5;
	offsetY = 1.0;
	init = 1;
	*ich = 0;
}

#ifndef UNDERSCORE
  dvclos(ich)
#else
  dvclos_(ich)
#endif
int4		*ich;
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
int4		*iopt;
{
}

#ifndef UNDERSCORE
  dvpags(npage,sizex,sizey,lkeep)
#else
  dvpags_(npage,sizex,sizey,lkeep)
#endif
int4		*npage;
float		*sizex;
float		*sizey;
int4		*lkeep;
{
	int	ioffsetX,ioffsetY;
	
	if(init)
	{
		init = 0;
	}
	else
	{
		putchar(0x0c);
		putchar(0x1b);
		putchar('<');
	}
	
	putchar(0x1b);
	putchar(0x5b);
	putchar('1');
	putchar('5');
	putchar(0x70);

	putchar(0x1b);
	putchar(0x5b);
	putchar('2');
	putchar(0x26);
	putchar(0x7a);
	
	putchar(0x1b);
	putchar(0x5b);
	putchar(0x26);
	putchar(0x7d);
	
	putchar(0x23);
	putchar(0x1e);

	putchar(0x21);
	putchar(0x30);
	putnum(-2);
	putnum(1);
	putchar(0x1e);

	putchar(0x24);
	putchar(0x1e);
	
	putchar(0x28);
	putnum(0);
	putnum(0);
	putnum(28600);
	putnum(19900);
	putchar(0x1e);
	
	putchar(0x7d);
	putchar(0x22);
	ioffsetX = 1000*offsetX;
	ioffsetY = 1000*offsetY;
	putnum(ioffsetX);
	putnum(ioffsetY);
	putchar(0x1e);
	
}

#ifndef UNDERSCORE
  dvpage(ich)
#else
  dvpage_(ich)
#endif
int4		*ich;
{
	putchar(0x25);
	putchar(0x1e);
	
	putchar(0x7d);
	putchar(0x70);
	putnum(0);
	putnum(0);
	putchar(0x1e);
	*ich = 0;
}

#ifndef UNDERSCORE
  dvgrps()
#else
  dvgrps_()
#endif
{
}

#ifndef UNDERSCORE
  dvgrpe()
#else
  dvgrpe_()
#endif
{
}

#ifndef UNDERSCORE
  dvmove(ix,iy)
#else
  dvmove_(ix,iy)
#endif
int4		*ix,*iy;
{
	xpos = *ix * delta;
	ypos = *iy * delta;
}

#ifndef UNDERSCORE
  dvdraw(ix,iy)
#else
  dvdraw_(ix,iy)
#endif
int4		*ix,*iy;
{
	int		x,y;
	
	x = *ix * delta;
	y = *iy * delta;
	if(icls != 0)
	{	
		putchar(0x31);
		putnum(xpos);
		putnum(ypos);
		putnum(x-xpos);
		putnum(y-ypos);
		putchar(0x1e);
	}
	xpos = x;
	ypos = y;
}

#ifndef UNDERSCORE
  dvlins(ixn,iyn,np)
#else
  dvlins_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
{
	int		i;
        int            x,y;
	
	xpos = ixn[0] * delta;
	ypos = iyn[0] * delta;

	if(icls != 0)
	{	
	  putchar(0x31);
	  putnum(xpos);
	  putnum(ypos);
	  for (i = 1; i < *np; i++)
	    {
	      x = ixn[i] * delta;
	      y = iyn[i] * delta;
	      putnum(x-xpos);
	      putnum(y-ypos);
	      xpos = x;
	      ypos = y;
	    }
	  putchar(0x1e);
	}
}

#ifndef UNDERSCORE
  dvpoly(ixn,iyn,np)
#else
  dvpoly_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
{
	int		i;
        int            x,y;
	
	xpos = ixn[0] * delta;
	ypos = iyn[0] * delta;

	if(icls != 0)
	{	
	  putchar(0x32);
	  putnum(xpos);
	  putnum(ypos);
	  for (i = 1; i < *np; i++)
	    {
	      x = ixn[i] * delta;
	      y = iyn[i] * delta;
	      putnum(x-xpos);
	      putnum(y-ypos);
	      xpos = x;
	      ypos = y;
	    }
	  putchar(0x1e);
	}
}

#ifndef UNDERSCORE
  dvtext(ix,iy,iasc,nchar)
#else
  dvtext_(ix,iy,iasc,nchar)
#endif
int4		*ix,*iy,*iasc,*nchar;
{
	int		i,x,y;
	
	x = *ix * delta;
	y = *iy * delta;
	if(icls != 0)
	{	putchar(0x7d);
		putchar(0x70);
		putnum(x);
		putnum(y);
		putchar(0x1e);
		for(i=0; i<*nchar; i++) putchar(iasc[i]);
		putchar(0x1b);
		putchar(0x5b);
		putchar(0x26);
		putchar(0x7d);
	}
	xpos = x + *nchar * dxch;
	ypos = y + *nchar * dych;
}

#ifndef UNDERSCORE
  dvstln(iln,ibl,icl)
#else
  dvstln_(iln,ibl,icl)
#endif
int4		*iln,*ibl,*icl;
{
      if (*iln != -1) {
	putchar(0x45);
	putnum(1);
	switch(*iln)
	{
		case 0: putnum(0); break;
		case 1: putnum(-2); break;
		case 2: putnum(-1); break;
		case 3: putnum(1); break;
		case 4: putnum(3); break;
		case 5: putnum(3); break;
		case 6: putnum(4); break;
		case 7: putnum(4); break;
	}
	putchar(0x1e);
      }

      if (*ibl != -1) {
	putchar(0x46);
	putnum(1);
	switch(*ibl)
	{
		case 0: putnum(-1); break;
		case 1: putnum(-1); break;
		case 2: putnum(-2); break;
		case 3: putnum(-2); break;
		case 4: putnum(-3); break;
		case 5: putnum(-3); break;
		case 6: putnum(-4); break;
		case 7: putnum(-4); break;
	}
	putchar(0x1e);
      }

      if (*icl != -1) {
	putchar(0x49);
	switch(*icl)
	{
		case 0: putnum(-29); break;
		case 1: putnum(-30); break;
		case 2: putnum(-31); break;
		case 3: putnum(-32); break;
		case 4: putnum(-41); break;
		case 5: putnum(-33); break;
		case 6: putnum(-34); break;
		case 7: putnum(-35); break;
	}
	putnum(0);
	putchar(0x1e);
	icls = *icl;
      }
}

#ifndef UNDERSCORE
  dvcrgb(ir,ig,ib)
#else
  dvcrgb_(ir,ig,ib)
#endif
int4		*ir,*ig,*ib;
{
  float cred,cgreen,cblue,cblack;

  cred   = *ir/255.0;
  cgreen = *ig/255.0;
  cblue  = *ib/255.0;
  cblack = 1.0 - (0.15 * cblue + 0.30 * cred + 0.55 * cgreen);
  icls = cblack * 8 - 0.5;
  if (icls < 0) icls = 0;

  putchar(0x49);
  switch(icls)
    {
    case 0: putnum(-29); break;
    case 1: putnum(-30); break;
    case 2: putnum(-31); break;
    case 3: putnum(-32); break;
    case 4: putnum(-41); break;
    case 5: putnum(-33); break;
    case 6: putnum(-34); break;
    case 7: putnum(-35); break;
    }
  putnum(0);
  putchar(0x1e);
}

#ifndef UNDERSCORE
  dvlwdt(iw)
#else
  dvlwdt_(iw)
#endif
int4		*iw;
{
  int ibl;

  ibl = *iw * deltad;
  if (ibl > 7) ibl = 7;

  putchar(0x46);
  putnum(1);
  switch(ibl)
    {
    case 0: putnum(-1); break;
    case 1: putnum(-1); break;
    case 2: putnum(-1); break;
    case 3: putnum(-2); break;
    case 4: putnum(-2); break;
    case 5: putnum(-3); break;
    case 6: putnum(-3); break;
    case 7: putnum(-4); break;
    }
  putchar(0x1e);
}

#ifndef UNDERSCORE
  dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
  dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
int4		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
	dxch = *ichsp * cos(*angl deg) * delta;
	dych = *ichsp * sin(*angl deg) * delta;
	*ind = 1;
}

#ifndef UNDERSCORE
  dvfont(ifnt,ind)
#else
  dvfont_(ifnt,ind)
#endif
int4		*ifnt,*ind;
{
	*ind = 1;
}

#ifndef UNDERSCORE
  dvchin(iasc,nchar)
#else
  dvchin_(iasc,nchar)
#endif
int4		*iasc,*nchar;
{
	int		i;

	for(i=0; i < *nchar; i++) iasc[i]=32;
}

#ifndef UNDERSCORE
  dvxyin(ix,iy)
#else
  dvxyin_(ix,iy)
#endif
int4		*ix,*iy;
{
	*ix = xpos / delta;
	*iy = ypos / delta;
}

#ifndef UNDERSCORE
  dvsetv(id)
#else
  dvsetv_(id)
#endif
int4		*id;
{
}

#ifndef UNDERSCORE
  dvgetv(id,ix,iy,kd,kid)
#else
  dvgetv_(id,ix,iy,kd,kid)
#endif
int4		*id,*ix,*iy,*kd,*kid;
{
	*id = 0;
        *ix = 0;
        *iy = 0;
        *kd = 0;
        *kid= 0;
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

#ifndef UNDERSCORE
  dvsync()
#else
  dvsync_()
#endif
{
}

#ifndef UNDERSCORE
  dvgcfunc(id)
#else
  dvgcfunc_(id)
#endif
int4		*id;
{
}  

#ifndef UNDERSCORE
  dvrgbtrg(ixn,iyn,ir,ig,ib)
#else
  dvrgbtrg_(ixn,iyn,ir,ig,ib)
#endif
int4		ixn[],iyn[];
int4		ir[],ig[],ib[];
{
}

