/* $Id$ */
#include <stdio.h>

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#ifndef UNDERSCORE
void dvtype(ich)
#else
void dvtype_(ich)
#endif
int4		*ich;
{
	printf(" # GSAF V3.5 : GDUM V3.50 : Copyright (C) 1999 A. Fukuyama\n");
	*ich=0;
}

#ifndef UNDERSCORE
void dvopen(ich)
#else
void dvopen_(ich)
#endif
int4		*ich;
{
	*ich = -1;
}

#ifndef UNDERSCORE
void dvclos(ich)
#else
void dvclos_(ich)
#endif
int4		*ich;
{
	*ich = 1;
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
void dvinqres(int4 *width, int4 *height)
#else
void dvinqres_(int4 *width, int4 *height)
#endif
{
  *width =  3024; /* 300 DPI : 300 dot * 256 mm / 25.4 mm */
  *height = 2268; /* 300 DPI : 300 dot * 192 mm / 25.4 mm */
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
}

#ifndef UNDERSCORE
void dvpage(ich)
#else
void dvpage_(ich)
#endif
int4		*ich;
{
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
}

#ifndef UNDERSCORE
void dvdraw(ix,iy)
#else
void dvdraw_(ix,iy)
#endif
int4		*ix,*iy;
{
}

#ifndef UNDERSCORE
void dvlins(ixn,iyn,np)
#else
void dvlins_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4		*np;
{
}

#ifndef UNDERSCORE
void dvpoly(ixn,iyn,np)
#else
void dvpoly_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4		*np;
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

#ifndef UNDERSCORE
void dvtext(ix,iy,iasc,nchar)
#else
void dvtext_(ix,iy,iasc,nchar)
#endif
int4		*ix,*iy,*iasc,*nchar;
{
}

#ifndef UNDERSCORE
void dvstln(iln,ibl,icl)
#else
void dvstln_(iln,ibl,icl)
#endif
int4		*iln,*ibl,*icl;
{
}

#ifndef UNDERSCORE
void dvlwdt(iw)
#else
void dvlwdt_(iw)
#endif
int4		*iw;
{
}

#ifndef UNDERSCORE
void dvcrgb(ir,ig,ib)
#else
void dvcrgb_(ir,ig,ib)
#endif
int4		*ir,*ig,*ib;
{
}

#ifndef UNDERSCORE
void dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
void dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
int4		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
	*ich = 0;
}

#ifndef UNDERSCORE
void dvfont(ifnt,ind)
#else
void dvfont_(ifnt,ind)
#endif
int4		*ifnt,*ind;
{
	*ind = 0;
}

#ifndef UNDERSCORE
void dvchin(iasc,nchar)
#else
void dvchin_(iasc,nchar)
#endif
int4		*iasc,*nchar;
{
	int		i;

	for(i = 0; i < *nchar; i++)
		iasc[i] = ' ';
}

#ifndef UNDERSCORE
void dvxyin(ix,iy)
#else
void dvxyin_(ix,iy)
#endif
int4		*ix,*iy;
{
	*ix = 0;
	*iy = 0;
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
void dvdefimage(const int4 *id, const int4 *x, const int4 *y, const int4 imagedata[])
#else
void dvdefimage_(const int4 *id, const int4 *x, const int4 *y, const int4 imagedata[])
#endif
{
}

#ifndef UNDERSCORE
void dvundefimage(const int4 *id)
#else
void dvundefimage_(const int4 *id)
#endif
{
}

#ifndef UNDERSCORE
void dvputimage(const int4 *id, const int4 *x, const int4 *y)
#else
void dvputimage_(const int4 *id, const int4 *x, const int4 *y)
#endif
{
}

#ifndef UNDERSCORE
void dvxflush()
#else
void dvxflush_()
#endif
{
}

