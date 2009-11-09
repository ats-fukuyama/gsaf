/* $Id$
 * SVG driver
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#ifdef SONYCISC
#define M_PI	3.14159265358979323846
extern char *getenv();
#else
#include <stdlib.h>
#endif

#define DEG	(M_PI/180)

#ifndef NULL
#define NULL ((void *)0)
#endif

#define MAX_STR	256
#define MAX_FUSED	13

static double		xpos,ypos;
static double		delta;
static double		lwidth;
static int		ired,igreen,iblue,iwhite;
static int		imv,ilns,ifont,ifused[MAX_FUSED];
static double		dchs,dxch,dych,dangle;
static char		file_basename[MAX_STR];
static char		filename_tail[MAX_STR];
static FILE		*svgfile;
static int		interactive,istart,iend,inum,ititle,irotate,icolor;
static int		psmode,igouraud,ipage;
static const char 	*fontname[MAX_FUSED]={
	"serif","serif","serif","serif",
	"sans-serif","sans-serif","sans-serif","sans-serif",
	"Courier","Courier","Courier","Courier",
	"Symbol"
};
static const char 	*fontstyle[MAX_FUSED]={
	"normal","italic","normal","italic",
	"normal","oblique","normal","oblique",
	"normal","oblique","normal","oblique",
	"normal"
};
static const char 	*fontweight[MAX_FUSED]={
	"normal","normal","bold","bold",
	"normal","normal","bold","bold",
	"normal","normal","bold","bold",
	"normal"
};
	static const char	*dash_list[7] = {
		"1.333,4","3.333,2","8,2.667","6.667,1.333,1.333,1.333",
		"11.333,1.333,2,1.333","4,1.333,1.333,1.333,1.333,1.333",
		"8,1.333,2,1.333,2,1.333"};

static void getposition(int4 xin,int4 yin,double *xout,double *yout)
{
	if (irotate) {
		*xout = 380 - yin*delta;
		*yout = 512 - xin*delta;
	} else {
		*xout = xin*delta;
		*yout = 380 - yin*delta;
	}
}

static void getpositioninv(double xin,double yin,int4 *xout,int4 *yout)
{
	if (irotate) {
		*xout = (380 - yin)/delta;
		*yout = (512 - xin)/delta;
	} else {
		*xout = xin/delta;
		*yout = (380 - yin)/delta;
	}
}

static void dvheader(int4 npage)
{
	char	filename[MAX_STR];

	sprintf(filename,"%s-%d.svg",file_basename,(int)npage);
	svgfile = fopen(filename,"w");
	fprintf(svgfile,"<?xml version=\"1.0\" standalone=\"no\"?>\n");
	fprintf(svgfile,"<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n");

	if (irotate)
	  fprintf(svgfile,"<svg width=\"190mm\" height=\"256mm\" viewBox=\"0 0 380 512\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\"> \n");
	else
	  fprintf(svgfile,"<svg width=\"256mm\" height=\"190mm\" viewBox=\"0 0 512 380\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\"> \n");
        
	sprintf(filename,"%s#%d",file_basename,(int)npage);
	fprintf(svgfile,"<desc>%s</desc>\n",filename);
}

static void dvtrailer(int4 ipage)
{
	fprintf(svgfile,"</svg>\n");
}

static void dvstrokecolor()
{
	fprintf(svgfile," stroke=");
  
	switch(icolor) {
/* black and white */
	case 0:
		fprintf(svgfile,"\"rgb(0,0,0)\"");
		break;
/* gray and white */
	case 1:
		fprintf(svgfile,"\"rgb(%d,%d,%d)\"",iwhite,iwhite,iwhite);
		break;
/* color */
	case 2:
		fprintf(svgfile,"\"rgb(%d,%d,%d)\"",ired,igreen,iblue);
		break;
	}
}

static void dvfillcolor()
{
	fprintf(svgfile," fill=");
  
	switch(icolor) {
/* black and white */
	case 0:
		fprintf(svgfile,"\"rgb(0,0,0)\"");
		break;
/* gray and white */
	case 1:
		fprintf(svgfile,"\"rgb(%d,%d,%d)\"",iwhite,iwhite,iwhite);
		break;
/* color */
	case 2:
		fprintf(svgfile,"\"rgb(%d,%d,%d)\"",ired,igreen,iblue);
		break;
	}
}

static void dvlinestyle()
{
	switch(ilns) {
	case 0: break;
	default:
		fprintf(svgfile," stroke-dasharray=");
		fprintf(svgfile,"\"%s\"",dash_list[ilns-1]);
		break;
	}
}

#ifndef UNDERSCORE
void dvtype(int4 *ich)
#else
void dvtype_(int4 *ich)
#endif
{
	*ich=0;
}

#ifndef UNDERSCORE
void dvopen(int4 *ich)
#else
void dvopen_(int4 *ich)
#endif
{	
	char	*parm,*env;
	char	str[MAX_STR];
	int	len; 

/*	delta = (72.0/25.4)*(256.0/(1024*32)); 72 pt = 1 in = 25.4 mm */
	delta = 512.0/(1024*32); /* width 512 px */
	psmode = 0;
	igouraud = 0;

	parm = getenv("GSAF_PARM_TEMP");
	if (parm) {
		sscanf(parm,"%s %d %d %d %d %d %d %d",
		       str,&interactive,&istart,&iend,
		       &inum,&ititle,&irotate,&icolor);
		if (str[0] == '\0') {
			fprintf(stderr,"## Illegal ps file name ! \n");
			exit(1);
		}
		len = strlen(str);
		strncpy(file_basename,str+1,len-2);
		if (len > 5) {
			strncpy(filename_tail,str+len-4,3);
			if (!strcmp(filename_tail,".gs"))
				file_basename[len-5]='\0';
		}
		if (interactive >= 4)
			igouraud = 1;
		interactive %= 4;
		if (interactive >= 2)
			psmode = 1;
	}
	else {
		fprintf(stderr," ## Parameter env var not found ! \n");
		exit(1);
	}
	ipage = 0;
	if (psmode == 1)
		dvheader(1);
	*ich = 0;
}

#ifndef UNDERSCORE
void dvclos(int4 *ich)
#else
void dvclos_(int4 *ich)
#endif
{
	*ich = 0;
}

#ifndef UNDERSCORE
void dvoptn(const char *kopt,const int4 *iopt)
#else
void dvoptn_(const char *kopt,const int4 *iopt)
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
void dvpags(const int4 *npage,const float *sizex,const float *sizey,const int4 *lkeep)
#else
void dvpags_(const int4 *npage,const float *sizex,const float *sizey,const int4 *lkeep)
#endif
{
	ipage=ipage+1;
	dvheader(*npage);
	ifont = 0;
	imv = 0;
}

#ifndef UNDERSCORE
void dvpage(int4 *ich)
#else
void dvpage_(int4 *ich)
#endif
{
	if (imv)
		fprintf(svgfile,"\" />\n");
	dvtrailer(1);
	fclose(svgfile);
	*ich = 0;
}

#ifndef UNDERSCORE
void dvgrps(void)
#else
void dvgrps_(void)
#endif
{
	if (imv)
		fprintf(svgfile,"\" />\n");
	fprintf(svgfile,"<g>\n");
	imv = 0;
}


#ifndef UNDERSCORE
void dvgrpe(void)
#else
void dvgrpe_(void)
#endif
{
	if (imv)
		fprintf(svgfile,"\" />\n");
	fprintf(svgfile,"</g>\n");
	imv = 0;
}


#ifndef UNDERSCORE
void dvmove(const int4 *ix,const int4 *iy)
#else
void dvmove_(const int4 *ix,const int4 *iy)
#endif
{
	if (imv)
		fprintf(svgfile,"\" />\n");
	fprintf(svgfile,"<path");
	fprintf(svgfile," stroke-width=\"%1.3f\"",lwidth);
	dvlinestyle();
	fprintf(svgfile," fill=\"none\"");
	dvstrokecolor();

	getposition(*ix,*iy,&xpos,&ypos);
	fprintf(svgfile," d=\"M %1.3f %1.3f\n",xpos,ypos);
	imv = 1;
}

#ifndef UNDERSCORE
void dvdraw(const int4 *ix,const int4 *iy)
#else
void dvdraw_(const int4 *ix,const int4 *iy)
#endif
{
	if (!imv) {
		fprintf(svgfile,"<path");
		fprintf(svgfile," stroke-width=\"%1.3f\"",lwidth);
		dvlinestyle();
		fprintf(svgfile," fill=\"none\"");
		dvstrokecolor();
		fprintf(svgfile," d=\"M %1.3f %1.3f\n",xpos,ypos);
		imv = 1;
	}

	getposition(*ix,*iy,&xpos,&ypos);
	fprintf(svgfile," L %1.3f %1.3f\n",xpos,ypos);
}

#ifndef UNDERSCORE
void dvlins(const int4 ixn[],const int4 iyn[],const int4 *np)
#else
void dvlins_(const int4 ixn[],const int4 iyn[],const int4 *np)
#endif
{
	int		i;
	
	if (imv)
		fprintf(svgfile,"\" />\n");
	fprintf(svgfile,"<polyline");
	fprintf(svgfile," stroke-width=\"%1.3f\"",lwidth);
	dvlinestyle();
	fprintf(svgfile," fill=\"none\"");
	dvstrokecolor();
	fprintf(svgfile," points=\"");

	for (i = 0; i < *np; i++) {
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		fprintf(svgfile," %1.3f,%1.3f",xpos,ypos);
	}

	fprintf(svgfile,"\" />\n");
	imv = 0;
}

#ifndef UNDERSCORE
void dvpoly(const int4 ixn[],const int4 iyn[],const int4 *np)
#else
void dvpoly_(const int4 ixn[],const int4 iyn[],const int4 *np)
#endif
{
	int		i;

	if (imv)
		fprintf(svgfile,"\" />\n");
	fprintf(svgfile,"<polygon");
	fprintf(svgfile," stroke-width=\"%1.3f\"",lwidth);
	dvlinestyle();
	dvstrokecolor();
	dvfillcolor();
	fprintf(svgfile," points=\"");

	for (i = 0; i < *np; i++) {
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		fprintf(svgfile," %1.3f,%1.3f",xpos,ypos);
	}

	fprintf(svgfile,"\" />\n");
	imv = 0;
}

#ifndef UNDERSCORE
void dvcrgb(const int4 *ir,const int4 *ig,const int4 *ib)
#else
void dvcrgb_(const int4 *ir,const int4 *ig,const int4 *ib)
#endif
{
	ired   = *ir;
	igreen = *ig;
	iblue  = *ib;
	/* ITU-R BT.709 (sRGB) */
	iwhite = 0.213 * ired + 0.715 * igreen + 0.072 * iblue;
}

#ifndef UNDERSCORE
void dvrgbtrg(const int4 ixn[],const int4 iyn[],
	      const int4 ir[],const int4 ig[],const int4 ib[])
#else
void dvrgbtrg_(const int4 ixn[],const int4 iyn[],
	       const int4 ir[],const int4 ig[],const int4 ib[])
#endif
{
	int	i;

	if (imv)
		fprintf(svgfile,"\" />\n");
	fprintf(svgfile,"<polyline");
	fprintf(svgfile," stroke-width=\"%1.3f\"",lwidth);
	fprintf(svgfile," fill=\"rgb(%d,%d,%d)\"",ired,igreen,iblue);
	fprintf(svgfile," points=\"");

	for (i = 0; i < 3; i++) {
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		fprintf(svgfile," %1.3f,%1.3f",xpos,ypos);
	}

	fprintf(svgfile,"\" />\n");
	imv = 0;
}


#ifndef UNDERSCORE
void dvtext(const int4 *ix,const int4 *iy,const int4 *iasc,const int4 *nchar)
#else
void dvtext_(const int4 *ix,const int4 *iy,const int4 *iasc,const int4 *nchar)
#endif
{
	int i;

	if (imv)
		fprintf(svgfile,"\" />\n");

	getposition(*ix,*iy,&xpos,&ypos);

	fprintf(svgfile,"<g>\n");
	fprintf(svgfile,"<text x=\"%1.3f\" y=\"%1.3f\"",xpos,ypos);
	fprintf(svgfile," font-family=\"%s\"\n",fontname[ifont]);
	fprintf(svgfile," font-style=\"%s\"\n",fontstyle[ifont]);
	fprintf(svgfile," font-weight=\"%s\"\n",fontweight[ifont]);
	fprintf(svgfile," font-size=\"%1.3f\"\n",dchs);
	dvfillcolor();
	fprintf(svgfile," rotate=\"%1.3f\" >\n",dangle);

	for(i=0; i<*nchar; i++) {
		if(iasc[i] < 0x20)
			putc(0x20,svgfile);
		else
		{
			switch(iasc[i]) {
			case '<': fprintf(svgfile,"&lt;"); break;
			case '>': fprintf(svgfile,"&gt;"); break;
			case '&': fprintf(svgfile,"&amp;"); break;
			default:  putc(iasc[i],svgfile);
			}
		}
	}
	fprintf(svgfile,"</text>\n");
	fprintf(svgfile,"</g>\n");
	xpos += *nchar * dxch;
	ypos += *nchar * dych;
	imv = 0;
}

#ifndef UNDERSCORE
void dvstln(const int4 *iln,const int4 *ibl,const int4 *icl)
#else
void dvstln_(const int4 *iln,const int4 *ibl,const int4 *icl)
#endif
{
	if (*iln != -1)
		ilns = *iln;
	if (*ibl != -1) {
		if (*ibl)
			lwidth = *ibl*0.5;
		else
			lwidth = 0.25;
	}
	if (*icl != -1) {
		switch(*icl) { 
		case 7: ired = 0;   igreen = 0;   iblue = 0; break;
		case 5: ired = 0;   igreen = 0;   iblue = 255; break;
		case 6: ired = 255; igreen = 0;   iblue = 0; break;
		case 3: ired = 255; igreen = 0;   iblue = 255; break;
		case 4: ired = 0;   igreen = 255; iblue = 0; break;
		case 1: ired = 0;   igreen = 255; iblue = 255; break;
		case 2: ired = 255; igreen = 255; iblue = 0; break;
		case 0: ired = 255; igreen = 255; iblue = 255; break;
		}
		/* ITU-R BT.709 (sRGB) */
		iwhite = 0.213 * ired + 0.715 * igreen + 0.072 * iblue;
	}
}

#ifndef UNDERSCORE
void dvlwdt(const int4 *iw)
#else
void dvlwdt_(const int4 *iw)
#endif
{
	lwidth = *iw * delta;
}

#ifndef UNDERSCORE
void dvstch(const int4 *ichh,const int4 *ichw,const int4 *ichsp,
	    const float *angl,const float *tilt,int4 *ind)
#else
void dvstch_(const int4 *ichh,const int4 *ichw,const int4 *ichsp,
	     const float *angl,const float *tilt,int4 *ind)
#endif
{
	int		jchh;
	float		cosch,sinch;

	dangle=*angl;
	if (*ichh == 0)
		jchh = 1;
	else
		jchh = *ichh;

	if (! irotate) {
		cosch = cos((*angl+90) * DEG);
		sinch = sin((*angl+90) * DEG);
	} else {
		cosch = cos((*angl) * DEG);
		sinch = sin((*angl) * DEG);
	}

	dxch = *ichsp * cosch * delta;
	dych = *ichsp * sinch * delta;
	if ((ifont >= 8) && (ifont <= 11))
		dchs = jchh * delta * (5.0/3);
	else
		dchs = jchh * delta * (22.0/15);
	*ind = 0;
}

#ifndef UNDERSCORE
void dvfont(const int4 *ifnt,int4 *ind)
#else
void dvfont_(const int4 *ifnt,int4 *ind)
#endif
{
	if (*ifnt == 0) {
		ifont = 8;
		ifused[ifont] = 1;
		*ind = 0;
	} else if ((*ifnt >= 32) && (*ifnt <= 44)) {
		ifont = *ifnt - 32;
		ifused[ifont] = 1;
		*ind = 0;
	} else
		*ind = 1;
}

#ifndef UNDERSCORE
void dvchin(int4 *iasc,int4 *nchar)
#else
void dvchin_(int4 *iasc,const int4 *nchar)
#endif
{
	int		i;

	for(i=0; i < *nchar; i++)
		iasc[i]=' ';
}

#ifndef UNDERSCORE
void dvxyin(int4 *ix,int4 *iy)
#else
void dvxyin_(int4 *ix,int4 *iy)
#endif
{
	getpositioninv(xpos,ypos,ix,iy);
}

#ifndef UNDERSCORE
void dvsetv(const int4 *id)
#else
void dvsetv_(const int4 *id)
#endif
{
}

#ifndef UNDERSCORE
void dvgetv(int4 *id,int4 *ix,int4 *iy,int4 *kd,int4 *kid)
#else
void dvgetv_(int4 *id,int4 *ix,int4 *iy,int4 *kd,int4 *kid)
#endif
{
	*id = 0;
	*ix = 0;
	*iy = 0;
	*kd = 0;
	*kid= 0;
}

#ifndef UNDERSCORE
void dvcheckv(int4 *id,int4 *ix,int4 *iy,int4 *kd,int4 *kid)
#else
void dvcheckv_(int4 *id,int4 *ix,int4 *iy,int4 *kd,int4 *kid)
#endif
{
	*id = 0;
	*ix = 0;
	*iy = 0;
	*kd = 0;
	*kid= 0;
}

#ifndef UNDERSCORE
void dvgrmd(void)
#else
void dvgrmd_(void)
#endif
{
}

#ifndef UNDERSCORE
void dvchmd(void)
#else
void dvchmd_(void)
#endif
{
}

#ifndef UNDERSCORE
void dveras(void)
#else
void dveras_(void)
#endif
{
}

#ifndef UNDERSCORE
void dvprnt(void)
#else
void dvprnt_(void)
#endif
{
}

#ifndef UNDERSCORE
void dvbell(void)
#else
void dvbell_(void)
#endif
{
}

#ifndef UNDERSCORE
void dvsync(void)
#else
void dvsync_(void)
#endif
{
}

#ifndef UNDERSCORE
void dvgcfunc(const int4 *id)
#else
void dvgcfunc_(const int4 *id)
#endif
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

