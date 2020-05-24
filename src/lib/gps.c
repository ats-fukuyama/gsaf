/* $Id$
 * PostScript driver
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

#ifndef PSHEADER1
#define PSHEADER1 "/usr/local/lib/gsaf_header1.ps"
#endif

#ifndef PSHEADER2
#define PSHEADER2 "/usr/local/lib/gsaf_header2.ps"
#endif

#define MAX_STR	256
#define MAX_FUSED	13

static double		xpos,ypos;
static double		delta;
static double		dxch,dych,fxxch,fyxch,fxych,fyych;
static double		lwidth,cred,cgreen,cblue,cblack;
static int		imv,ilns,ifont,ifused[MAX_FUSED];
static int		irgb;
static double		dchs,dchss,dchsp;
static char		file_basename[MAX_STR];
static char		filename_tail[MAX_STR];
static FILE		*psfile;
static int		interactive,istart,iend,inum,ititle,irotate,icolor;
static int		psmode,igouraud,ipage;
static double		cgamma;
static const char 	*fontname[MAX_FUSED]={
	"Times-Roman ","Times-Italic ","Times-Bold ","Times-BoldItalic ",
	"Helvetica ","Helvetica-Oblique ","Helvetica-Bold ","Helvetica-BoldOblique ",
	"Courier ","Courier-Oblique ","Courier-Bold ","Courier-BoldOblique ",
	"Symbol"
};

static void getposition(int4 xin,int4 yin,double *xout,double *yout)
{
	if (! irotate) {
		*xout = 570 - yin*delta;
		*yout = xin*delta + 56;
	} else {
		*xout = xin*delta + 56;
		*yout = yin*delta + 56;
	}
}

static void getpositioninv(double xin,double yin,int4 *xout,int4 *yout)
{
	if (! irotate) {
		*xout = (yin - 56)/delta;
		*yout = (570 - xin)/delta;
	} else {
		*xout = (xin - 56)/delta;
		*yout = (yin - 56)/delta;
	}
}

static void dvheader(int4 npage)
{
	char	filename[MAX_STR],buffer[MAX_STR];
	time_t	nseconds;
	char	*timestr;
	int	i;
	FILE	*hdfile;

	if (psmode == 1) {
		if (interactive == 2) {
			psfile = stdout;
			fprintf(psfile,"%%!PS-Adobe-2.0\n");
		} else {
			sprintf(filename,"%s.ps",file_basename);
			psfile = fopen(filename,"w");
			fprintf(psfile,"%%!PS-Adobe-2.0\n");
		}
	} else {
		sprintf(filename,"%s-%d.eps",file_basename,(int)npage);
		psfile = fopen(filename,"w");
		fprintf(psfile,"%%!PS-Adobe-2.0 EPSF-1.2\n");
	}
	fprintf(psfile,"%%%%Creator: GSAF gps.c V4.07\n");
	sprintf(filename,"%s#%d",file_basename,(int)npage);
	fprintf(psfile,"%%%%Title: (%s)\n",filename);
	nseconds = time(NULL);
	timestr = ctime(&nseconds);
	timestr[strlen(timestr)-1] = 0;
	fprintf(psfile,"%%%%Creation date: (%s)\n",timestr); 
	fprintf(psfile,"%%%%DocumentProcSets: Adobe_Illustrator_1.1 0 0\n");
	fprintf(psfile,"%%%%DocumentSuppliedProcSets: Adobe_Illustrator_1.1 0 0\n");
	fprintf(psfile,"%%%%Pages: (atend)\n");
	if (irotate == 1)
		fprintf(psfile,"%%%%BoundingBox: 56 28 782 570\n");
	else
		fprintf(psfile,"%%%%BoundingBox: 28 56 570 782\n");
	fprintf(psfile,"%%%%DocumentFonts: (atend)\n");
	if (icolor == 2)
		fprintf(psfile,"%%%%DocumentProcessColors: Color\n");
	else
		fprintf(psfile,"%%%%DocumentProcessColors: Black\n");
	fprintf(psfile,"%%%%EndComments\n");

	hdfile = fopen(PSHEADER1,"r");
	if (!hdfile) {
		printf("file gsaf_header1.ps cannot be found.\n");
		exit(0);
	}
 	while(fgets(buffer,MAX_STR,hdfile))
		fputs(buffer,psfile);
	fclose(hdfile);

	if (igouraud == 1) {
		fprintf(psfile,"/threshold 0.03 def \n");
		hdfile = fopen(PSHEADER2,"r");
		if (!hdfile) {
			printf("file gsaf_header2.ps cannot be found.\n");
			exit(0);
		}
		while(fgets(buffer,MAX_STR,hdfile))
			fputs(buffer,psfile);
		fclose(hdfile);
	}

	for (i = 0; i < MAX_FUSED; i++)
		ifused[i] = 0;
}

static void dvtrailer(int4 ipage)
{
	int i;

	fprintf(psfile,"%%%%Trailer\n");
	fprintf(psfile,"%%%%DocumentFonts:");
	for (i=0;i<MAX_FUSED;i++) {
		if(ifused[i])
		  fprintf(psfile," %s",fontname[i]);
	}
	fprintf(psfile,"\n");

	fprintf(psfile,"%%%%Pages: %d\n",(int)ipage);
	fprintf(psfile,"_E end\n%%%%EOF\n");
}

static void dvcolor(int ind)
{
	switch(ind) {
/* move, draw */
	case 0:
		if (icolor == 2) {
/*			fprintf(psfile,"%1.3f G\n",1-cblack);*/
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
				1-cred,1-cgreen,1-cblue);
		} else if (icolor == 0) {
			fprintf(psfile,"0 G\n");
/*			fprintf(psfile,"0 0 0 1 K\n");*/
		} else {
			fprintf(psfile,"%1.3f G\n",1-cblack);
/*			fprintf(psfile,"0 0 0 %1.3f K\n",cblack);*/
		}
		break;
/* lines */
	case 1:
		if (icolor == 2) {
/*			fprintf(psfile,"%1.3f G\n",1-cblack);*/
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
				1-cred,1-cgreen,1-cblue);
			fprintf(psfile,"1 g\n");
/*			fprintf(psfile,"0 0 0 0 k\n");*/
		} else if (icolor == 0) {
			fprintf(psfile,"0 G\n");
/*			fprintf(psfile,"0 0 0 1 K\n");*/
			fprintf(psfile,"1 g\n");
/*			fprintf(psfile,"0 0 0 0 k\n");*/
		} else {
			fprintf(psfile,"%1.3f G\n",1-cblack);
/*			fprintf(psfile,"0 0 0 %1.3f K\n",cblack);*/
			fprintf(psfile,"1 g\n");
/*			fprintf(psfile,"0 0 0 0 k\n");*/
		}
		break;
/* polygon */
	case 2:
		if (icolor == 2) {
/*			fprintf(psfile,"%1.3f G\n",1-cblack);*/
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
				1-cred,1-cgreen,1-cblue);
/*			fprintf(psfile,"%1.3f g\n",1-cblack);*/
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 k\n",
				1-cred,1-cgreen,1-cblue);
		} else if (icolor == 0) {
			fprintf(psfile,"0 G\n");
/*			fprintf(psfile,"0 0 0 1 K\n");*/
			fprintf(psfile,"0 g\n");
/*			fprintf(psfile,"0 0 0 1 k\n");*/
		} else {
			fprintf(psfile,"%1.3f G\n",1-cblack);
/*			fprintf(psfile,"0 0 0 %1.3f K\n",cblack);*/
			fprintf(psfile,"%1.3f g\n",1-cblack);
/*			fprintf(psfile,"0 0 0 %1.3f k\n",cblack);*/
		}
		break;
/* text */
	case 3:
		if (icolor == 2) {
/*			fprintf(psfile,"%1.3f g\n",1-cblack);*/
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 k\n",
				1-cred,1-cgreen,1-cblue);
		} else if (icolor == 0) {
			fprintf(psfile,"0 g\n");
/*			fprintf(psfile,"0 0 0 1 k\n");*/
		} else {
			fprintf(psfile,"%1.3f g\n",1-cblack);
/*			fprintf(psfile,"0 0 0 %1.3f k\n",cblack);*/
		}
		break;
	}
	irgb = 0;
}

static double cconv(double c)
{
	if (cgamma != 1 && cgamma > 0)
		return pow(c/255.0, 1/cgamma);
	else
		return c/255.0;
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

	env = getenv("GSGAMMAPS");
	if(env)
		cgamma = atof(env);
	else
	        cgamma = 1;

	delta = (72.0/25.4)*(256.0/(1024*32)); /* 72 pt = 1 in = 25.4 mm */
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
	if (psmode == 1) {
		dvtrailer(ipage);
		fclose(psfile);
	}
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
	if (psmode != 1)
		dvheader(*npage);
	fprintf(psfile,"%%%%Page: %d %d\n",(int)*npage,ipage);
	fprintf(psfile,"0 i 2 J 0 j 1 w 4 M []0 d\n");
	ifont = 0;
	imv = 0;
	irgb = 0;
}

#ifndef UNDERSCORE
void dvpage(int4 *ich)
#else
void dvpage_(int4 *ich)
#endif
{
	if (imv)
		fprintf(psfile,"S\n");
	if (psmode == 1)
		fprintf(psfile,"showpage\n");
	else {
		dvtrailer(1);
		fclose(psfile);
	}
	*ich = 0;
}

#ifndef UNDERSCORE
void dvgrps(void)
#else
void dvgrps_(void)
#endif
{
	if (imv)
		fprintf(psfile,"S\n");
	fprintf(psfile,"u\n");
	imv = 0;
}


#ifndef UNDERSCORE
void dvgrpe(void)
#else
void dvgrpe_(void)
#endif
{
	if (imv)
		fprintf(psfile,"S\n");
	fprintf(psfile,"U\n");
	imv = 0;
}


#ifndef UNDERSCORE
void dvmove(const int4 *ix,const int4 *iy)
#else
void dvmove_(const int4 *ix,const int4 *iy)
#endif
{
	if (imv)
		fprintf(psfile,"S\n");
	fprintf(psfile,"%% Line\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(0);

	getposition(*ix,*iy,&xpos,&ypos);
	fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
	imv = 1;
}

#ifndef UNDERSCORE
void dvdraw(const int4 *ix,const int4 *iy)
#else
void dvdraw_(const int4 *ix,const int4 *iy)
#endif
{
  if(irgb)
    fprintf(psfile,"S\n");
  if(!imv || irgb){
    fprintf(psfile,"%% Line\n");
    fprintf(psfile,"%1.3f w\n",lwidth);
    dvcolor(0);
    fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
    imv = 1;
  }

  getposition(*ix,*iy,&xpos,&ypos);
  fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
}

#ifndef UNDERSCORE
void dvlins(const int4 ixn[],const int4 iyn[],const int4 *np)
#else
void dvlins_(const int4 ixn[],const int4 iyn[],const int4 *np)
#endif
{
	int		i;
	
	if (imv)
		fprintf(psfile,"S\n");
	fprintf(psfile,"%% Lines\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(1);

	for (i = 0; i < *np; i++) {
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		if (i == 0)
			fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
		else
			fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
	}

	if ((ixn[0] == ixn[*np-1]) && (iyn[0] == iyn[*np-1]))
		fprintf(psfile,"s\n");
	else
		fprintf(psfile,"S\n");
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
		fprintf(psfile,"S\n");
	fprintf(psfile,"%% Poly\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(2);

	for (i = 0; i < *np; i++) {
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		if (i == 0)
			fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
		else
			fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
	}
	fprintf(psfile,"f\n");
	imv = 0;
}

#ifndef UNDERSCORE
void dvcrgb(const int4 *ir,const int4 *ig,const int4 *ib)
#else
void dvcrgb_(const int4 *ir,const int4 *ig,const int4 *ib)
#endif
{
	cred   = cconv(*ir);
	cgreen = cconv(*ig);
	cblue  = cconv(*ib);
	/* ? */
	/* cblack = 1.0 - (0.15 * cblue + 0.30 * cred + 0.55 * cgreen); */
	/* ITU-R BT.601-2 (NTSC) */
	/* cblack = 1 - (0.299 * cred + 0.587 * cgreen + 0.114 * cblue); */
	/* ITU-R BT.709 (sRGB) */
	cblack = 1 - cconv(0.213 * *ir + 0.715 * *ig + 0.072 * *ib);
	irgb=1;
}

#ifndef UNDERSCORE
void dvrgbtrg(const int4 ixn[],const int4 iyn[],
	      const int4 ir[],const int4 ig[],const int4 ib[])
#else
void dvrgbtrg_(const int4 ixn[],const int4 iyn[],
	       const int4 ir[],const int4 ig[],const int4 ib[])
#endif
{
	if (igouraud == 1) {
		int	i;
/*		int	smooth;*/
		int4	ired,igreen,iblue;
		int4	ixtmp[3],iytmp[3];

/*		smooth=0;*/
		ired=ir[0]; igreen=ig[0]; iblue=ib[0];

/*		for (i = 1; i < 3; i++) {
			if (ired != ir[i] || igreen != ig[i] || iblue != ib[i]) {
				smooth = 1;
				break;
			}
		}
 */
		for (i = 0; i < 3; i++) {
			getposition(ixn[i],iyn[i],&xpos,&ypos);
			ixtmp[i] = (int4)floor(xpos+0.5);
			iytmp[i] = (int4)floor(ypos+0.5);
		}
		xpos=ixtmp[2]; ypos=iytmp[2];

		if (imv)
			fprintf(psfile,"S\n");
		fprintf(psfile,"%% gouraudtriangle\n");
		fprintf(psfile,"%1.3f w\n",lwidth);
		dvcolor(2);
		fprintf(psfile, "[%d %d %d %d %d %d]",
			(int)ixtmp[0], (int)ixtmp[1], (int)ixtmp[2],
			(int)iytmp[0], (int)iytmp[1], (int)iytmp[2]);
		fprintf(psfile, "[%f %f %f] [%f %f %f] [%f %f %f] gouraudtriangle\n",
			ir[0]/255.0, ig[0]/255.0, ib[0]/255.0, 
			ir[1]/255.0, ig[1]/255.0, ib[1]/255.0,
			ir[2]/255.0, ig[2]/255.0, ib[2]/255.0);
		fprintf(psfile,"b\n");
		imv = 0;
	} else {
		int	i;
		int4	ired,igreen,iblue;
	
		if (imv)
			fprintf(psfile,"S\n");
		fprintf(psfile,"%% Poly\n");
		fprintf(psfile,"%1.3f w\n",lwidth);

		ired   = (ir[0]+ir[1]+ir[2])/3;
		igreen = (ig[0]+ig[1]+ig[2])/3;
		iblue  = (ib[0]+ib[1]+ib[2])/3;
#ifndef UNDERSCORE
		dvcrgb(&ired,&igreen,&iblue);
#else
		dvcrgb_(&ired,&igreen,&iblue);
#endif
		dvcolor(2);

		for (i = 0; i < 3; i++) {
			getposition(ixn[i],iyn[i],&xpos,&ypos);
			if (i == 0)
				fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
			else
				fprintf(psfile,"%1.3f %1.3f l\n",xpos,ypos);
		}
		i = 0;
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		fprintf(psfile,"%1.3f %1.3f l\n",xpos,ypos);
		fprintf(psfile,"f\n");
		imv = 0;
	}
}


#ifndef UNDERSCORE
void dvtext(const int4 *ix,const int4 *iy,const int4 *iasc,const int4 *nchar)
#else
void dvtext_(const int4 *ix,const int4 *iy,const int4 *iasc,const int4 *nchar)
#endif
{
	int i;

	if (imv)
		fprintf(psfile,"S\n");

	getposition(*ix,*iy,&xpos,&ypos);

	fprintf(psfile,"u\n");
	dvcolor(3);
	fprintf(psfile,"/%s %f %f %f 0 z\n",fontname[ifont],dchs,dchss,dchsp);
	fprintf(psfile,"[%1.3f %1.3f %1.3f %1.3f %1.3f %1.3f ] e\n(",
		fxxch,fyxch,fxych,fyych,xpos,ypos);
	for(i=0; i<*nchar; i++) {
		switch(iasc[i]) {
		case '(': fprintf(psfile,"\\050"); break;
		case ')': fprintf(psfile,"\\051"); break;
		case '<': fprintf(psfile,"\\074"); break;
		case '>': fprintf(psfile,"\\076"); break;
		case '[': fprintf(psfile,"\\133"); break;
		case ']': fprintf(psfile,"\\135"); break;
		case '{': fprintf(psfile,"\\173"); break;
		case '}': fprintf(psfile,"\\175"); break;
		case '%': fprintf(psfile,"\\045"); break;
		default:  putc(iasc[i],psfile);
		}
	}
	fprintf(psfile,") t T\nU\n");
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
		case 7: cred = 0; cgreen = 0; cblue = 0; break;
		case 5: cred = 0; cgreen = 0; cblue = 1; break;
		case 6: cred = 1; cgreen = 0; cblue = 0; break;
		case 3: cred = 1; cgreen = 0; cblue = 1; break;
		case 4: cred = 0; cgreen = 1; cblue = 0; break;
		case 1: cred = 0; cgreen = 1; cblue = 1; break;
		case 2: cred = 1; cgreen = 1; cblue = 0; break;
		case 0: cred = 1; cgreen = 1; cblue = 1; break;
		}
		/* ? */
		/* cblack = 1 - (0.15 * cblue + 0.30 * cred + 0.55 * cgreen); */
		/* ITU-R BT.601-2 (NTSC) */
		/* cblack = 1 - (0.299 * cred + 0.587 * cgreen + 0.114 * cblue); */
		/* ITU-R BT.709 (sRGB) */
		cblack = 1 - (0.213 * cred + 0.715 * cgreen + 0.072 * cblue);
		irgb = 1;
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
	int		jchh,jchw;
	float		wfact,tfact,cosch,sinch;

	if (*ichh == 0)
		jchh = 1;
	else
		jchh = *ichh;
	if (*ichw == 0)
		jchw = 1;
	else
		jchw = *ichw;
	wfact = (jchw * 3.0) / (jchh * 2.0);
	tfact = 1.0 / cos (*tilt * DEG);
	if (! irotate) {
		cosch = cos((*angl+90) * DEG);
		sinch = sin((*angl+90) * DEG);
		fxxch = cos((*angl+90) * DEG) * wfact;
		fyxch = sin((*angl+90) * DEG) * wfact;
		fxych =-sin((*angl+*tilt+90) * DEG) * tfact;
		fyych = cos((*angl+*tilt+90) * DEG) * tfact;
	} else {
		cosch = cos((*angl) * DEG);
		sinch = sin((*angl) * DEG);
		fxxch = cos((*angl) * DEG) * wfact;
		fyxch = sin((*angl) * DEG) * wfact;
		fxych =-sin((*angl+*tilt) * DEG) * tfact;
		fyych = cos((*angl+*tilt) * DEG) * tfact;
	}

	dxch = *ichsp * cosch * delta;
	dych = *ichsp * sinch * delta;
	if ((ifont >= 8) && (ifont <= 11))
		dchs = jchh * delta * (5.0/3);
	else
		dchs = jchh * delta * (22.0/15);
	dchss = dchs * 0.8;
	dchsp = (*ichsp - 1.5* *ichw) * delta ;
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

