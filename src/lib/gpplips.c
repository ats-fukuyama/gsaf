/* $Id$ */
/* 
   GSAF printer driver for LIPS III
   Version V3.51  1999/07/27 
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#ifdef SONYCISC
#define M_PI            3.14159265358979323846
#endif

#define DEG	(M_PI/180)

static int		xpos,ypos;
static float		delta,factorX,factorY,offsetX,offsetY;
static int		dxch,dych;
static int		init;
static int		icls;

static void putnum(i)
int		i;
{
	int		iabs;
	
	iabs = abs(i);
	if (iabs >= 0x10000) {
		putchar(0x40 | (iabs >> 16));
		iabs &= 0xffff;
		putchar(0x40 | (iabs >> 10));
		iabs &= 0x03ff;
		putchar(0x40 | (iabs >> 4));
	} else {
		if (iabs >= 0x400) {
			putchar(0x40 | (iabs >> 10));
			iabs &= 0x03ff;
			putchar(0x40 | (iabs >> 4));
		} else {
			if (iabs >= 0x10)
				putchar(0x40 | (iabs >> 4));
		}
	}
	iabs &= 0x000f;
	if (i >= 0)
		putchar(0x30 | iabs);
	else
		putchar(0x20 | iabs);
}
			
#ifndef UNDERSCORE
void dvtype(ich)
#else
void dvtype_(ich)
#endif
int4		*ich;
{
	*ich=0;
}

#ifndef UNDERSCORE
void dvopen(ich)
#else
void dvopen_(ich)
#endif
int4		*ich;
{	
	delta = 25600.0 / (1024*32);
	factorX = 1.0;
	factorY = 1.0;
	offsetX = 1.5;
	offsetY = 1.0;
	init = 1;
	*ich = 0;

	putchar(0x1b);
	putchar(0x50);
	putchar('3');
	putchar('1');
	putchar(0x3b);
	putchar('3');
	putchar('0');
	putchar('0');
	putchar(0x3b);
	putchar('0');
	putchar(0x4a);
	putchar('G');
	putchar('S');
	putchar('A');
	putchar('F');
	putchar(0x1b);
	putchar(0x5c);

	putchar(0x1b);
	putchar('<');

	putchar(0x1b);
	putchar(0x5b);
	putchar('1');
	putchar('5');
	putchar(0x70);
}

#ifndef UNDERSCORE
void dvclos(ich)
#else
void dvclos_(ich)
#endif
int4		*ich;
{
	putchar(0x1b);
	putchar(0x50);
	putchar('0');
	putchar(0x4a);
	putchar(0x1b);
	putchar(0x5c);

	fflush(stdout);
	*ich = 0;
}

#ifndef UNDERSCORE
void dvoptn(kopt,iopt)
#else
void dvoptn_(kopt,iopt)
#endif
char		*kopt;
int4		*iopt;
{
}

#ifndef UNDERSCORE
void dvpags(npage,sizex,sizey,lkeep)
#else
void dvpags_(npage,sizex,sizey,lkeep)
#endif
int4		*npage;
float		*sizex;
float		*sizey;
int4		*lkeep;
{
	int	ioffsetX,ioffsetY;
	
	if (init)
		init = 0;
	else
		putchar(0x0c);
	
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
	
	putchar(0x7d);
	putchar(0x45);
	putnum(0);
	putchar(0x1e);

	putchar(0x7d);
	putchar(0x46);
	putnum(2);
	putchar(0x1e);
}

#ifndef UNDERSCORE
void dvpage(ich)
#else
void dvpage_(ich)
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
void dvgrps()
#else
void dvgrps_()
#endif
{
}

#ifndef UNDERSCORE
void dvgrpe()
#else
void dvgrpe_()
#endif
{
}

#ifndef UNDERSCORE
void dvmove(ix,iy)
#else
void dvmove_(ix,iy)
#endif
int4		*ix,*iy;
{
	xpos = *ix * delta;
	ypos = *iy * delta;
}

#ifndef UNDERSCORE
void dvdraw(ix,iy)
#else
void dvdraw_(ix,iy)
#endif
int4		*ix,*iy;
{
	int		x,y;
	
	x = *ix * delta;
	y = *iy * delta;
	if (icls != 0) {	
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
void dvlins(ixn,iyn,np)
#else
void dvlins_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
{
	int		i;
        int            x,y;
	
	xpos = ixn[0] * delta;
	ypos = iyn[0] * delta;

	if (icls != 0) {	
		putchar(0x31);
		putnum(xpos);
		putnum(ypos);
		for (i = 1; i < *np; i++) {
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
void dvpoly(ixn,iyn,np)
#else
void dvpoly_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
{
	int		i;
        int            x,y;
	
	xpos = ixn[0] * delta;
	ypos = iyn[0] * delta;

	if (icls != 0) {	
		putchar(0x32);
		putnum(xpos);
		putnum(ypos);
		for (i = 1; i < *np; i++) {
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
void dvtext(ix,iy,iasc,nchar)
#else
void dvtext_(ix,iy,iasc,nchar)
#endif
int4		*ix,*iy,*iasc,*nchar;
{
	int		i,x,y;
	
	x = *ix * delta;
	y = *iy * delta;
	if (icls != 0) {
		putchar(0x7d);
		putchar(0x70);
		putnum(x);
		putnum(y);
		putchar(0x1e);

		for (i=0; i<*nchar; i++)
			putchar(iasc[i]);

		putchar(0x1b);
		putchar(0x5b);
		putchar(0x26);
		putchar(0x7d);
	}
	xpos = x + *nchar * dxch;
	ypos = y + *nchar * dych;
}

#ifndef UNDERSCORE
void dvstln(iln,ibl,icl)
#else
void dvstln_(iln,ibl,icl)
#endif
int4		*iln,*ibl,*icl;
{
      if (*iln != -1) {
	      putchar(0x45);
	      putnum(1);
	      switch(*iln) {
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
		switch(*ibl) {
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
		switch(*icl) {
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
void dvcrgb(ir,ig,ib)
#else
void dvcrgb_(ir,ig,ib)
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
	switch(icls) {
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
void dvlwdt(iw)
#else
void dvlwdt_(iw)
#endif
int4		*iw;
{
	int ibl;

	ibl = *iw * delta;
	if (ibl > 255)
		ibl = 255;
	if (ibl < 2)
		ibl = -1;

	putchar(0x46);
	putnum(1);
	putnum(ibl);
	putchar(0x1e);
}

#ifndef UNDERSCORE
void dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
void dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
int4		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
	dxch = *ichsp * cos(*angl * DEG) * delta;
	dych = *ichsp * sin(*angl * DEG) * delta;
	*ind = 1;
}

#ifndef UNDERSCORE
void dvfont(ifnt,ind)
#else
void dvfont_(ifnt,ind)
#endif
int4		*ifnt,*ind;
{
	*ind = 1;
}

#ifndef UNDERSCORE
void dvchin(iasc,nchar)
#else
void dvchin_(iasc,nchar)
#endif
int4		*iasc,*nchar;
{
	int		i;

	for (i=0; i < *nchar; i++)
		iasc[i]=' ';
}

#ifndef UNDERSCORE
void dvxyin(ix,iy)
#else
void dvxyin_(ix,iy)
#endif
int4		*ix,*iy;
{
	*ix = xpos / delta;
	*iy = ypos / delta;
}

#ifndef UNDERSCORE
void dvsetv(id)
#else
void dvsetv_(id)
#endif
int4		*id;
{
}

#ifndef UNDERSCORE
void dvgetv(id,ix,iy,kd,kid)
#else
void dvgetv_(id,ix,iy,kd,kid)
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
void dvgrmd()
#else
void dvgrmd_()
#endif
{
}

#ifndef UNDERSCORE
void dvchmd()
#else
void dvchmd_()
#endif
{
}

#ifndef UNDERSCORE
void dveras()
#else
void dveras_()
#endif
{
}

#ifndef UNDERSCORE
void dvprnt()
#else
void dvprnt_()
#endif
{
}

#ifndef UNDERSCORE
void dvbell()
#else
void dvbell_()
#endif
{
}

#ifndef UNDERSCORE
void dvsync()
#else
void dvsync_()
#endif
{
}

#ifndef UNDERSCORE
void dvgcfunc(id)
#else
void dvgcfunc_(id)
#endif
int4		*id;
{
}  

#ifndef UNDERSCORE
void dvrgbtrg(ixn,iyn,ir,ig,ib)
#else
void dvrgbtrg_(ixn,iyn,ir,ig,ib)
#endif
int4		ixn[],iyn[];
int4		ir[],ig[],ib[];
{
}
