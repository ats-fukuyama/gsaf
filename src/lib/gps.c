/* $Id$ */
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

static float		xpos,ypos;
static float		delta;
static float		dxch,dych,fxxch,fyxch,fxych,fyych;
static float		lwidth,cred,cgreen,cblue,cblack;
static int		imv,ilns,ifont,ifused[MAX_FUSED];
static float		dchs,dchss,dchsp;
static char		file_basename[MAX_STR];
static char		filename_tail[MAX_STR];
static FILE		*psfile;
static int		interactive,istart,iend,inum,ititle,irotate,icolor;
static int		psmode,igouraud,ipage;
static double		cgamma;

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
	fprintf(psfile,"%%%%Creator: GSAF gps.c V3.86\n");
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
	fgets(buffer,MAX_STR,hdfile);
 	while(!feof(hdfile)){
		fputs(buffer,psfile);
		fgets(buffer,MAX_STR,hdfile);
	}
	fclose(hdfile);

	if (igouraud == 1) {
		fprintf(psfile,"/threshold 0.03 def \n");
		hdfile = fopen(PSHEADER2,"r");
		if (!hdfile) {
			printf("file gsaf_header2.ps cannot be found.\n");
			exit(0);
		}
		fgets(buffer,MAX_STR,hdfile);
		while(!feof(hdfile)){
			fputs(buffer,psfile);
			fgets(buffer,MAX_STR,hdfile);
		}
		fclose(hdfile);
	}

	for (i = 0; i < MAX_FUSED; i++)
		ifused[i] = 0;
}

static void dvtrailer(int4 ipage)
{
	fprintf(psfile,"%%%%Trailer\n");
	fprintf(psfile,"%%%%DocumentFonts: ");
	if (ifused[0]) fprintf(psfile,"Times-Roman ");
	if (ifused[1]) fprintf(psfile,"Times-Italic ");
	if (ifused[2]) fprintf(psfile,"Times-Bold ");
	if (ifused[3]) fprintf(psfile,"Times-BoldItalic ");
	if (ifused[4]) fprintf(psfile,"Helvetica ");
	if (ifused[5]) fprintf(psfile,"Helvetica-Oblique ");
	if (ifused[6]) fprintf(psfile,"Helvetica-Bold ");
	if (ifused[7]) fprintf(psfile,"Helvetica-BoldOblique ");
	if (ifused[8]) fprintf(psfile,"Courier ");
	if (ifused[9]) fprintf(psfile,"Courier-Oblique ");
	if (ifused[10]) fprintf(psfile,"Courier-Bold ");
	if (ifused[11]) fprintf(psfile,"Courier-BoldOblique ");
	if (ifused[12]) fprintf(psfile,"Symbol ");
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
			fprintf(psfile,"%1.3f G\n",1.0-cblack);
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
				1.0-cred,1.0-cgreen,1.0-cblue);
		} else if (icolor == 0) {
			fprintf(psfile,"0 G\n");
			fprintf(psfile,"0 0 0 1 K\n");
		} else {
			fprintf(psfile,"%1.3f G\n",1.0-cblack);
			fprintf(psfile,"0 0 0 %1.3f K\n",cblack);
		}
		break;
/* lines */
	case 1:
		if (icolor == 2) {
			fprintf(psfile,"%1.3f G\n",1.0-cblack);
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
				1.0-cred,1.0-cgreen,1.0-cblue);
			fprintf(psfile,"1 g\n");
			fprintf(psfile,"0 0 0 0 k\n");
		} else if (icolor == 0) {
			fprintf(psfile,"0 G\n");
			fprintf(psfile,"0 0 0 1 K\n");
			fprintf(psfile,"1 g\n");
			fprintf(psfile,"0 0 0 0 k\n");
		} else {
			fprintf(psfile,"%1.3f G\n",1.0-cblack);
			fprintf(psfile,"0 0 0 %1.3f K\n",cblack);
			fprintf(psfile,"1 g\n");
			fprintf(psfile,"0 0 0 0 k\n");
		}
		break;
/* polygon */
	case 2:
		if (icolor == 2) {
			fprintf(psfile,"%1.3f G\n",1.0-cblack);
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
				1.0-cred,1.0-cgreen,1.0-cblue);
			fprintf(psfile,"%1.3f g\n",1.0-cblack);
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 k\n",
				1.0-cred,1.0-cgreen,1.0-cblue);
		} else if (icolor == 0) {
			fprintf(psfile,"0 G\n");
			fprintf(psfile,"0 0 0 1 K\n");
			fprintf(psfile,"0 g\n");
			fprintf(psfile,"0 0 0 1 k\n");
		} else {
			fprintf(psfile,"%1.3f G\n",1.0-cblack);
			fprintf(psfile,"0 0 0 %1.3f K\n",cblack);
			fprintf(psfile,"%1.3f g\n",1.0-cblack);
			fprintf(psfile,"0 0 0 %1.3f k\n",cblack);
		}
		break;
/* text */
	case 3:
		if (icolor == 2) {
			fprintf(psfile,"%1.3f g\n",1.0-cblack);
			fprintf(psfile,"%1.3f %1.3f %1.3f 0 k\n",
				1.0-cred,1.0-cgreen,1.0-cblue);
		} else if (icolor == 0) {
			fprintf(psfile,"0 g\n");
			fprintf(psfile,"0 0 0 1 k\n");
		} else {
			fprintf(psfile,"%1.3f g\n",1.0-cblack);
			fprintf(psfile,"0 0 0 %1.3f k\n",cblack);
		}
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
void dvoptn(char *kopt,int4 *iopt)
#else
void dvoptn_(char *kopt,int4 *iopt)
#endif
{
}

#ifndef UNDERSCORE
void dvpags(int4 *npage,float *sizex,float *sizey,int4 *lkeep)
#else
void dvpags_(int4 *npage,float *sizex,float *sizey,int4 *lkeep)
#endif
{
	ipage=ipage+1;
	if (psmode != 1)
		dvheader(*npage);
	fprintf(psfile,"%%%%Page: %d %d\n",(int)*npage,ipage);
	fprintf(psfile,"0 i 2 J 0 j 1 w 4 M []0 d\n");
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
void dvmove(int4 *ix,int4 *iy)
#else
void dvmove_(int4 *ix,int4 *iy)
#endif
{
	if (imv)
		fprintf(psfile,"S\n");
	fprintf(psfile,"%% Line\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(0);

	if (! irotate) {
		xpos = 570 - *iy * delta;
		ypos = *ix * delta + 56;
	} else {
		xpos = *ix * delta + 56;
		ypos = *iy * delta + 56;
	}

	fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
	imv = 1;
}

#ifndef UNDERSCORE
void dvdraw(int4 *ix,int4 *iy)
#else
void dvdraw_(int4 *ix,int4 *iy)
#endif
{
	if (!imv) {
		fprintf(psfile,"%% Line\n");
		fprintf(psfile,"%1.3f w\n",lwidth);
		dvcolor(0);
		fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
		imv = 1;
	}

	if (! irotate) {
		xpos = 570 - *iy * delta;
		ypos = *ix * delta + 56;
	} else {
		xpos = *ix * delta + 56;
		ypos = *iy * delta + 56;
	}

	fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
}

#ifndef UNDERSCORE
void dvlins(int4 ixn[],int4 iyn[],int4 *np)
#else
void dvlins_(int4 ixn[],int4 iyn[],int4 *np)
#endif
{
	int		i;
	
	if (imv)
		fprintf(psfile,"S\n");
	fprintf(psfile,"%% Lines\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(1);

	for (i = 0; i < *np; i++) {
		if (! irotate) {
			xpos = 570 - iyn[i] * delta;
			ypos = ixn[i] * delta + 56;
		} else {
			xpos = ixn[i] * delta + 56;
			ypos = iyn[i] * delta + 56;
		}
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
void dvpoly(int4 ixn[],int4 iyn[],int4 *np)
#else
void dvpoly_(int4 ixn[],int4 iyn[],int4 *np)
#endif
{
	int		i;

	if (imv)
		fprintf(psfile,"S\n");
	fprintf(psfile,"%% Poly\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(2);

	for (i = 0; i < *np; i++) {
		if (! irotate) {
			xpos = 570 - iyn[i] * delta;
			ypos = ixn[i] * delta + 56;
		} else {
			xpos = ixn[i] * delta + 56;
			ypos = iyn[i] * delta + 56;
		}
		if (i == 0)
			fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
		else
			fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
	}
	fprintf(psfile,"b\n");
	imv = 0;
}

static double cconv(double c)
{
	if (cgamma != 1 && cgamma > 0)
		return pow(c/255.0, 1/cgamma);
	else
		return c/255.0;
}

#ifndef UNDERSCORE
void dvcrgb(int4 *ir,int4 *ig,int4 *ib)
#else
void dvcrgb_(int4 *ir,int4 *ig,int4 *ib)
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
}

#ifndef UNDERSCORE
void dvrgbtrg(int4 ixn[],int4 iyn[],int4 ir[],int4 ig[],int4 ib[])
#else
void dvrgbtrg_(int4 ixn[],int4 iyn[],int4 ir[],int4 ig[],int4 ib[])
#endif
{
	if (igouraud == 1) {
		int	i;
		int	smooth;
		int4	ired,igreen,iblue;
		int4	ixtmp[3],iytmp[3];

		smooth=0;
		ired=ir[0]; igreen=ig[0]; iblue=ib[0];

		for (i = 1; i < 3; i++) {
			if (ired != ir[i] || igreen != ig[i] || iblue != ib[i]) {
				smooth = 1;
				break;
			}
		}
		for (i = 0; i < 3; i++) {
			if (! irotate) {
				ixtmp[i] = 570 - iyn[i] * delta;
				iytmp[i] = ixn[i] * delta + 56;
			} else {
				ixtmp[i] = ixn[i] * delta + 56;
				iytmp[i] = iyn[i] * delta + 56;
			}
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
			if (! irotate) {
				xpos = 570 - iyn[i] * delta;
				ypos = ixn[i] * delta + 56;
			} else {
				xpos = ixn[i] * delta + 56;
				ypos = iyn[i] * delta + 56;
			}
			if (i == 0)
				fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
			else
				fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
		}
		fprintf(psfile,"b\n");
		imv = 0;
	}
}


#ifndef UNDERSCORE
void dvtext(int4 *ix,int4 *iy,int4 *iasc,int4 *nchar)
#else
void dvtext_(int4 *ix,int4 *iy,int4 *iasc,int4 *nchar)
#endif
{
	int		i;
	
	if (imv)
		fprintf(psfile,"S\n");

	if (! irotate) {
		xpos = 570 - *iy * delta;
		ypos = *ix * delta + 56;
	} else {
		xpos = *ix * delta + 56;
		ypos = *iy * delta + 56;
	}

	fprintf(psfile,"u\n");
	dvcolor(3);
	switch(ifont) {
	case  0: fprintf(psfile,"/Times-Roman %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  1: fprintf(psfile,"/Times-Italic %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  2: fprintf(psfile,"/Times-Bold %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  3: fprintf(psfile,"/Times-BoldItalic %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  4: fprintf(psfile,"/Helvetica %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  5: fprintf(psfile,"/Helvetica-Oblique %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  6: fprintf(psfile,"/Helvetica-Bold %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  7: fprintf(psfile,"/Helvetica-BoldOblique %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  8: fprintf(psfile,"/Courier %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case  9: fprintf(psfile,"/Courier-Oblique %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case 10: fprintf(psfile,"/Courier-Bold %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case 11: fprintf(psfile,"/Courier-BoldOblique %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	case 12: fprintf(psfile,"/Symbol %f %f %f 0 z\n",dchs,dchss,dchsp); break;
	}
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
void dvstln(iln,ibl,icl)
#else
void dvstln_(iln,ibl,icl)
#endif
int4		*iln,*ibl,*icl;
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
		case 7: cred = 0.0; cgreen = 0.0; cblue = 0.0; break;
		case 5: cred = 0.0; cgreen = 0.0; cblue = 1.0; break;
		case 6: cred = 1.0; cgreen = 0.0; cblue = 0.0; break;
		case 3: cred = 1.0; cgreen = 0.0; cblue = 1.0; break;
		case 4: cred = 0.0; cgreen = 1.0; cblue = 0.0; break;
		case 1: cred = 0.0; cgreen = 1.0; cblue = 1.0; break;
		case 2: cred = 1.0; cgreen = 1.0; cblue = 0.0; break;
		case 0: cred = 1.0; cgreen = 1.0; cblue = 1.0; break;
		}
		/* ? */
		/* cblack = 1.0 - (0.15 * cblue + 0.30 * cred + 0.55 * cgreen); */
		/* ITU-R BT.601-2 (NTSC) */
		/* cblack = 1 - (0.299 * cred + 0.587 * cgreen + 0.114 * cblue); */
		/* ITU-R BT.709 (sRGB) */
		cblack = 1 - (0.213 * cred + 0.715 * cgreen + 0.072 * cblue);
	}
}

#ifndef UNDERSCORE
void dvlwdt(iw)
#else
void dvlwdt_(iw)
#endif
int4		*iw;
{
	lwidth = *iw * delta;
}

#ifndef UNDERSCORE
void dvstch(int4 *ichh,int4 *ichw,int4 *ichsp,float *angl,float *tilt,int4 *ind)
#else
void dvstch_(int4 *ichh,int4 *ichw,int4 *ichsp,float *angl,float *tilt,int4 *ind)
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
void dvfont(int4 *ifnt,int4 *ind)
#else
void dvfont_(int4 *ifnt,int4 *ind)
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
void dvchin_(int4 *iasc,int4 *nchar)
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
	if (! irotate) {
		*iy = (570 - xpos) / delta;
		*ix = (ypos - 56) / delta;
	} else {
		*ix = (xpos - 56) / delta;
		*iy = (ypos - 56) / delta;
	}
}

#ifndef UNDERSCORE
void dvsetv(int4 *id)
#else
void dvsetv_(int4 *id)
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
void dvgcfunc(int4 *id)
#else
void dvgcfunc_(int4 *id)
#endif
{
}
