#include <stdio.h>
#include <math.h>

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#ifndef UNDERSCORE
  dvtype(ich)
#else
  dvtype_(ich)
#endif
int4		*ich;
{
	printf(" # GSAF V3.5 : GDUM V3.50 : Copyright (C) 1999 A. Fukuyama\n");
	*ich=0;
}

#ifndef UNDERSCORE
  dvopen(ich)
#else
  dvopen_(ich)
#endif
int4		*ich;
{
	*ich = -1;
}

#ifndef UNDERSCORE
  dvclos(ich)
#else
  dvclos_(ich)
#endif
int4		*ich;
{
	*ich = 1;
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
  dvpags(npage,sizex,sizey,lkeep)
#else
  dvpags_(npage,sizex,sizey,lkeep)
#endif
int4		*npage;
float		*sizex;
float		*sizey;
int4		*lkeep;
{
}

#ifndef UNDERSCORE
  dvpage(ich)
#else
  dvpage_(ich)
#endif
int4		*ich;
{
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
}

#ifndef UNDERSCORE
  dvdraw(ix,iy)
#else
  dvdraw_(ix,iy)
#endif
int4		*ix,*iy;
{
}

#ifndef UNDERSCORE
  dvlins(ixn,iyn,np)
#else
  dvlins_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
{
}

#ifndef UNDERSCORE
  dvpoly(ixn,iyn,np)
#else
  dvpoly_(ixn,iyn,np)
#endif
int4		ixn[],iyn[];
int4            *np;
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

#ifndef UNDERSCORE
  dvtext(ix,iy,iasc,nchar)
#else
  dvtext_(ix,iy,iasc,nchar)
#endif
int4		*ix,*iy,*iasc,*nchar;
{
}

#ifndef UNDERSCORE
  dvstln(iln,ibl,icl)
#else
  dvstln_(iln,ibl,icl)
#endif
int4		*iln,*ibl,*icl;
{
}

#ifndef UNDERSCORE
  dvlwdt(iw)
#else
  dvlwdt_(iw)
#endif
int4		*iw;
{
}

#ifndef UNDERSCORE
  dvcrgb(ir,ig,ib)
#else
  dvcrgb_(ir,ig,ib)
#endif
int4		*ir,*ig,*ib;
{
}

#ifndef UNDERSCORE
  dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
  dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
int4		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
  	*ich = 0;
}

#ifndef UNDERSCORE
  dvfont(ifnt,ind)
#else
  dvfont_(ifnt,ind)
#endif
int4		*ifnt,*ind;
{
	*ind = 0;
}

#ifndef UNDERSCORE
  dvchin(iasc,nchar)
#else
  dvchin_(iasc,nchar)
#endif
int4		*iasc,*nchar;
{
	int		i;

	for(i = 0; i < *nchar; i++)
	   iasc[i] = ' ';
}

#ifndef UNDERSCORE
  dvxyin(ix,iy)
#else
  dvxyin_(ix,iy)
#endif
int4		*ix,*iy;
{
	*ix = 0;
	*iy = 0;
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
