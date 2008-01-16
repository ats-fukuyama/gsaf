/* $Id$
 * TGIF driver
 *   TGIF version 2.16-p12 or higher is required.
 *   Rotated text objects are supported on version 3 or higher.
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

#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif

#ifdef SONYCISC
extern char *getenv();
#else
#include <stdlib.h>
#endif

#define DEG	(M_PI/180)

#ifndef NULL
#define NULL ((void *)0)
#endif

#define MAX_STR	256
#define MAX_POLY	4096

static double		xpos,ypos;
static double		delta;
static double		dxch,dych;
static double		lwidth,cred,cgreen,cblue,cblack;
static int		ilns,ifont;
static double		dchs;
static FILE		*tgiffile;
static int		interactive,istart,iend,inum,ititle,irotate,icolor;
static int		igouraud,ipage;
static double		cgamma,charwidth;
static int		id,charangle,groupid,ondraw,glevel,gnext,terminated;
static int4		polyx[MAX_POLY],polyy[MAX_POLY],polyn;


static void terminate(void)
{
	if (!terminated) {
		if (!glevel)
			fprintf(tgiffile,".\n");
		else if (gnext)
			fprintf(tgiffile,",\n");
		else
			fprintf(tgiffile,"\n");
		terminated = 1;
	}
}

static char *getcolor(void)
{
	static char color[8];
	static const char *name[8]={
		"black","blue","green","cyan",
		"red","magenta","yellow","white"
	};
	int gray,rgb;

	switch(icolor) {
	case 0:
		strcpy(color,name[0]);
		break;
	case 1:
		if (cblack == 0)
			strcpy(color,name[7]);
		else if (cblack == 1)
			strcpy(color,name[0]);
		else {
			gray=floor((1-cblack)*255+0.5);
			sprintf(color,"#%02x%02x%02x",gray,gray,gray);
		}
		break;
	case 2:
		if (cred==(int)cred
		    && cgreen==(int)cgreen
		    && cblue==(int)cblue) {
			    rgb = (int)cred<<2 | (int)cgreen<<1 | (int)cblue;
			    strcpy(color,name[rgb]);
		} else {
		    sprintf(color,"#%02x%02x%02x",
			    (int)floor(cred*255+0.5),
			    (int)floor(cgreen*255+0.5),
			    (int)floor(cblue*255+0.5));
		}
	}

	return color;
}

static int getgrid(double pt)
{
	return (int)floor(pt/72*128+0.5);
}

static int getlinewidth(void)
{
	int linewidth;

	linewidth = getgrid(lwidth);
	if(linewidth <= 0)
		linewidth = 1;
	else if(linewidth >= 8)
		linewidth = 7;

	return linewidth;
}

static int getah(int lw)
{
	return lw*2+6+(((lw-1)|4)>>1);
}

static int getaw(int lw)
{
	return lw+2;
}

static void getposition(int4 xin,int4 yin,double *xout,double *yout)
{
	if (! irotate) {
		*xout = 570 - yin*delta;
		*yout = xin*delta + 56;
		*yout = 1497.0/128*72 - *yout;
	} else {
		*xout = xin*delta + 56;
		*yout = yin*delta + 56;
		*yout = 1056.0/128*72 - *yout;
	}
}

static void getpositioninv(double xin,double yin,int4 *xout,int4 *yout)
{
	if (! irotate) {
		yin = 1497.0/128*72 - yin;
		*xout = (yin - 56)/delta;
		*yout = (570 - xin)/delta;
	} else {
		yin = 1056.0/128*72 - yin;
		*xout = (xin - 56)/delta;
		*yout = (yin - 56)/delta;
	}
}

static double cconv(double c)
{
	if (cgamma != 1 && cgamma > 0)
		return pow(c/255.0, 1/cgamma);
	else
		return c/255.0;
}

#ifndef UNDERSCORE
void dvlins(const int4 ixn[],const int4 iyn[],const int4 *np);
#else
void dvlins_(const int4 ixn[],const int4 iyn[],const int4 *np);
#endif
static void draw(void)
{
	terminate();

	ondraw = 0;
#ifndef UNDERSCORE
	dvlins(polyx,polyy,&polyn);
#else
	dvlins_(polyx,polyy,&polyn);
#endif
	polyn = 0;
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
	char	filename[MAX_STR],*parm,*str;
	int	len;

	str = getenv("GSGAMMATGIF");
	cgamma = (str!=NULL)? atof(str) : 1;

	delta = (72.0/25.4)*(256.0/32768); /* 72pt = 1in = 2.54cm, xsize=25.6cm */

	parm = getenv("GSAF_PARM_TEMP");
	if (parm) {
		sscanf(parm,"'%[^']' %d %d %d %d %d %d %d",
		       filename,&interactive,&istart,&iend,
		       &inum,&ititle,&irotate,&icolor);
		if (filename[0] == '\0') {
			fprintf(stderr,"## Invalid file name!\n");
			exit(1);
		}
		len = strlen(filename);
		if (len > 3) {
			if (!strcmp(filename+len-3,".gs"))
				filename[len-3]='\0';
		}
		strcat(filename,".obj");
		igouraud = interactive&4;
	} else {
		fprintf(stderr," ## Parameter env var not found!\n");
		exit(1);
	}
	ipage = 0;
	*ich = 0;

	tgiffile = fopen(filename,"w");
	fprintf(tgiffile,"%192c",'\n');
}

#ifndef UNDERSCORE
void dvclos(int4 *ich)
#else
void dvclos_(int4 *ich)
#endif
{
	time_t	nseconds;

	if (ondraw)
		draw();
	terminate();

	rewind(tgiffile);
	nseconds = time(NULL);
	fprintf(tgiffile,"%%TGIF 2.16-p12 created by GSAF gtgif.c on %s",ctime(&nseconds));
	fprintf(tgiffile,"state(%d,32,100,0,0,0,16,1,9,1,1,0,0,1,0,1,0,"
		"'Courier',0,17,0,0,0,10,0,0,1,1,0,16,0,0,1,%d,1,%d,%d,%d).",
		(irotate)?1:0,ipage,(icolor)?1:0,(irotate)?1497:1056,(irotate)?1056:1497);
	fclose(tgiffile);
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
	ipage++;
	if (ipage != 1)
		terminate();
	fprintf(tgiffile,"page(%d,\"\")",ipage);
	terminated = 0;
	ifont = 0;
}

#ifndef UNDERSCORE
void dvpage(int4 *ich)
#else
void dvpage_(int4 *ich)
#endif
{
	if (ondraw)
		draw();
	terminate();

	*ich = 0;
}

#ifndef UNDERSCORE
void dvgrps(void)
#else
void dvgrps_(void)
#endif
{
	terminate();

	fprintf(tgiffile,"group([\n");
	groupid = id;
	glevel++;
	gnext = 0;
}


#ifndef UNDERSCORE
void dvgrpe(void)
#else
void dvgrpe_(void)
#endif
{
	int x,y;

	if (ondraw)
		draw();

	if (groupid == id) {
		x = getgrid(xpos);
		y = getgrid(ypos);
		fprintf(tgiffile,"poly('white',2,[\n"
			"\t%d,%d,%d,%d],0,1,0,%d,0,0,0,0,8,3,0,\n"
			"    \"0\",[\n])",x,y,x,y,id++);
	}

	glevel--;
	fprintf(tgiffile,"\n],\n%d,0,[\n])",id++);
	gnext = 1;
	terminated = 0;
}

#ifndef UNDERSCORE
void dvmove(const int4 *ix,const int4 *iy)
#else
void dvmove_(const int4 *ix,const int4 *iy)
#endif
{
	if (ondraw)
		draw();

	getposition(*ix,*iy,&xpos,&ypos);
}

#ifndef UNDERSCORE
void dvdraw(const int4 *ix,const int4 *iy)
#else
void dvdraw_(int4 *ix,int4 *iy)
#endif
{
	if (ondraw && polyn >= MAX_POLY)
		draw();
	if (!ondraw) {
		getpositioninv(xpos,ypos,&polyx[0],&polyy[0]);
		polyn = 1;
		ondraw = 1;
	}
	polyx[polyn] = *ix;
	polyy[polyn] = *iy;
	polyn++;
}

#ifndef UNDERSCORE
void dvlins(const int4 ixn[],const int4 iyn[],const int4 *np)
#else
void dvlins_(const int4 ixn[],const int4 iyn[],const int4 *np)
#endif
{
	static const int style[8]={0,5,2,7,4,4,6,6};
	int i,lw;
	
	if (ondraw)
		draw();
	terminate();

	fprintf(tgiffile,"poly('%s',%d,[",getcolor(),(int)*np);
	for (i = 0; i < *np; i++) {
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		if (i==0)
			fprintf(tgiffile,"\n\t%d,%d",getgrid(xpos),getgrid(ypos));
		else {
			fprintf(tgiffile,"%s%d,%d",(i&7)? ",":",\n\t",
				getgrid(xpos),getgrid(ypos));
		}
	}
	lw = getlinewidth();
	fprintf(tgiffile,"],0,%d,1,%d,0,0,%d,0,%d,%d,0,\n    \"",
		lw,id++,(ilns>=0 && ilns<=7)?style[ilns]:0,
		getah(lw),getaw(lw));
	for (i = 0; i < (*np-1)/4+1; i++) {
		if((i&0x3f)==0 && i!=0)
			fprintf(tgiffile,"\n     ");
		putc('0',tgiffile);
	}
	fprintf(tgiffile,"\",[\n])");
	gnext = 1;
	terminated = 0;
}

#ifndef UNDERSCORE
void dvpoly(const int4 ixn[],const int4 iyn[],const int4 *np)
#else
void dvpoly_(const int4 ixn[],const int4 iyn[],const int4 *np)
#endif
{
	int i,close;
	
	if (ondraw)
		draw();
	terminate();

	if (ixn[0]!=ixn[*np-1] || iyn[0]!=iyn[*np-1])
		close = 1;
	else
		close = 0;
	fprintf(tgiffile,"polygon('%s',%d,[",getcolor(),(int)*np+close);
	for (i = 0; i < *np; i++) {
		getposition(ixn[i],iyn[i],&xpos,&ypos);
		if (i==0)
			fprintf(tgiffile,"\n\t%d,%d",getgrid(xpos),getgrid(ypos));
		else {
			fprintf(tgiffile,"%s%d,%d",(i&7)? ",":",\n\t",
				getgrid(xpos),getgrid(ypos));
		}
	}
	if (close) {
		getposition(ixn[0],iyn[0],&xpos,&ypos);
		fprintf(tgiffile,"%s%d,%d",(i&7)? ",":",\n\t",
			getgrid(xpos),getgrid(ypos));
	}
	fprintf(tgiffile,"],1,%d,0,0,%d,0,0,0,\n"
		"    \"",getlinewidth(),id++);
	for (i = 0; i < (*np-1)/4+1; i++)
		putc('0',tgiffile);
	fprintf(tgiffile,"\",[\n])");
	gnext = 1;
	terminated = 0;
}

#ifndef UNDERSCORE
void dvcrgb(const int4 *ir,const int4 *ig,const int4 *ib)
#else
void dvcrgb_(const int4 *ir,const int4 *ig,const int4 *ib)
#endif
{
	if (ondraw)
		draw();

	cred   = cconv(*ir);
	cgreen = cconv(*ig);
	cblue  = cconv(*ib);
	/* ? */
	/* cblack = 1 - (0.15 * cblue + 0.30 * cred + 0.55 * cgreen); */
	/* ITU-R BT.601-2 (NTSC) */
	/* cblack = 1 - (0.299 * cred + 0.587 * cgreen + 0.114 * cblue); */
	/* ITU-R BT.709 (sRGB) */
	cblack = 1 - cconv(0.213 * *ir + 0.715 * *ig + 0.072 * *ib);
}

#ifndef UNDERSCORE
void dvrgbtrg(const int4 ixn[],const int4 iyn[],
	      const int4 ir[],const int4 ig[],const int4 ib[])
#else
void dvrgbtrg_(const int4 ixn[],const int4 iyn[],
	       const int4 ir[],const int4 ig[],const int4 ib[])
#endif
{
	int4	ired,igreen,iblue,three,x[3],y[3];

	if (ondraw)
		draw();

	three = 3;
	if (!igouraud) {
		ired   = (ir[0]+ir[1]+ir[2]+1)/3;
		igreen = (ig[0]+ig[1]+ig[2]+1)/3;
		iblue  = (ib[0]+ib[1]+ib[2]+1)/3;
#ifndef UNDERSCORE
		dvcrgb(&ired,&igreen,&iblue);
#else
		dvcrgb_(&ired,&igreen,&iblue);
#endif
#ifndef UNDERSCORE
		dvpoly(ixn,iyn,&three);
#else
		dvpoly_(ixn,iyn,&three);
#endif
	} else {
		x[0] = ixn[0];
		x[1] = (ixn[1]+ixn[0])/2;
		x[2] = (ixn[2]+ixn[0])/2;
		y[0] = iyn[0];
		y[1] = (iyn[1]+iyn[0])/2;
		y[2] = (iyn[2]+iyn[0])/2;
		ired   = (ir[0]*4+ir[1]+ir[2]+3)/6;
		igreen = (ig[0]*4+ig[1]+ig[2]+3)/6;
		iblue  = (ib[0]*4+ib[1]+ib[2]+3)/6;
#ifndef UNDERSCORE
		dvcrgb(&ired,&igreen,&iblue);
#else
		dvcrgb_(&ired,&igreen,&iblue);
#endif
#ifndef UNDERSCORE
		dvpoly(x,y,&three);
#else
		dvpoly_(x,y,&three);
#endif
		x[0] = (ixn[0]+ixn[1])/2;
		x[1] = ixn[1];
		x[2] = (ixn[2]+ixn[1])/2;
		y[0] = (iyn[0]+iyn[1])/2;
		y[1] = iyn[1];
		y[2] = (iyn[2]+iyn[1])/2;
		ired   = (ir[0]+ir[1]*4+ir[2]+3)/6;
		igreen = (ig[0]+ig[1]*4+ig[2]+3)/6;
		iblue  = (ib[0]+ib[1]*4+ib[2]+3)/6;
#ifndef UNDERSCORE
		dvcrgb(&ired,&igreen,&iblue);
#else
		dvcrgb_(&ired,&igreen,&iblue);
#endif
#ifndef UNDERSCORE
		dvpoly(x,y,&three);
#else
		dvpoly_(x,y,&three);
#endif
		x[0] = (ixn[0]+ixn[2])/2;
		x[1] = (ixn[1]+ixn[2])/2;
		x[2] = ixn[2];
		y[0] = (iyn[0]+iyn[2])/2;
		y[1] = (iyn[1]+iyn[2])/2;
		y[2] = iyn[2];
		ired   = (ir[0]+ir[1]+ir[2]*4+3)/6;
		igreen = (ig[0]+ig[1]+ig[2]*4+3)/6;
		iblue  = (ib[0]+ib[1]+ib[2]*4+3)/6;
#ifndef UNDERSCORE
		dvcrgb(&ired,&igreen,&iblue);
#else
		dvcrgb_(&ired,&igreen,&iblue);
#endif
#ifndef UNDERSCORE
		dvpoly(x,y,&three);
#else
		dvpoly_(x,y,&three);
#endif
		x[0] = (ixn[0]+ixn[1])/2;
		x[1] = (ixn[1]+ixn[2])/2;
		x[2] = (ixn[2]+ixn[0])/2;
		y[0] = (iyn[0]+iyn[1])/2;
		y[1] = (iyn[1]+iyn[2])/2;
		y[2] = (iyn[2]+iyn[0])/2;
		ired   = (ir[0]+ir[1]+ir[2]+1)/3;
		igreen = (ig[0]+ig[1]+ig[2]+1)/3;
		iblue  = (ib[0]+ib[1]+ib[2]+1)/3;
#ifndef UNDERSCORE
		dvcrgb(&ired,&igreen,&iblue);
#else
		dvcrgb_(&ired,&igreen,&iblue);
#endif
#ifndef UNDERSCORE
		dvpoly(x,y,&three);
#else
		dvpoly_(x,y,&three);
#endif
	}
}


#ifndef UNDERSCORE
void dvtext(const int4 *ix,const int4 *iy,const int4 *iasc,const int4 *nchar)
#else
void dvtext_(const int4 *ix,const int4 *iy,const int4 *iasc,const int4 *nchar)
#endif
{
	int i,rotate;
	double xoffset,yoffset,size;
	static const char *fname[13]={
		"Times-Roman","Times-Italic","Times-Bold","Times-BoldItalic",
		"Helvetica","Helvetica-Oblique","Helvetica-Bold","Helvetica-BoldOblique",
		"Courier","Courier-Oblique","Courier-Bold","Courier-BoldOblique",
		"Symbol"
	};
	static const int fstyle[13]={
		0,2,1,3,
		0,2,1,3,
		0,2,1,3,
		0
	};
	
	if (ondraw)
		draw();
	terminate();

	getposition(*ix,*iy,&xpos,&ypos);
	rotate = (8-(charangle+45+360)/90)%4;
/* tgif version 2
	switch (rotate) {
	case 0:
		xoffset = 0;
		yoffset = -dchs*0.75;
		break;
	case 1:
		xoffset = +dchs*0.75;
		yoffset = 0;
		break;
	case 2:
		xoffset = 0;
		yoffset = +dchs*0.75;
		break;
	case 3:
		xoffset = -dchs*0.75;
		yoffset = 0;
		break;
	default:
		xoffset = 0;
		yoffset = 0;
	}
 */
/* tgif version 3 */
	size = *nchar*charwidth;
	switch (rotate) {
	case 0:
		xoffset = -dchs/16;
		yoffset = -dchs*3/4;
		break;
	case 1:
		xoffset = -size/2 + dchs*2/8;
		yoffset = +size/2 - dchs*3/8;
		break;
	case 2:
		xoffset = -size - dchs/8;
		yoffset = -dchs*3/16;
		break;
	case 3:
		xoffset = -size/2 - dchs*3/8;
		yoffset = -size/2 - dchs*9/16;
		break;
	default:
		xoffset = 0;
		yoffset = 0;
		break;
	}

	fprintf(tgiffile,"text('%s',%d,%d,'%s',%d,%d,1,0,"
		"%d,1,0,0,%d,0,0,0,0,0,0,0,[\n\t\"",
		getcolor(),
		getgrid(xpos+xoffset),
		getgrid(ypos+yoffset),
		fname[ifont],fstyle[ifont],(int)floor(dchs/72*128+0.5),
		rotate,id++);
	for(i=0; i<*nchar; i++) {
		if (iasc[i] == '\"')
			putc('\\',tgiffile);
		putc(iasc[i],tgiffile);
	}
	fprintf(tgiffile,"\"])");
	gnext = 1;
	terminated = 0;

	xpos += *nchar * dxch;
	ypos += *nchar * dych;
}

#ifndef UNDERSCORE
void dvstln(const int4 *iln,const int4 *ibl,const int4 *icl)
#else
void dvstln_(const int4 *iln,const int4 *ibl,const int4 *icl)
#endif
{
	if (ondraw)
		draw();

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
	double		cosch,sinch;

	if (*ichh == 0)
		jchh = 1;
	else
		jchh = *ichh;
	if (*ichw == 0)
		jchw = 1;
	else
		jchw = *ichw;
	if (! irotate) {
		cosch = cos((*angl+90) * DEG);
		sinch = sin((*angl+90) * DEG);
		charangle = (int)floor(*angl+90+0.5);
	} else {
		cosch = cos((*angl) * DEG);
		sinch = sin((*angl) * DEG);
		charangle = (int)floor(*angl+0.5);
	}
	dxch = *ichsp * cosch * delta;
	dych = *ichsp * sinch * delta;
	charwidth = *ichsp * delta;
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
		*ind = 0;
	} else if ((*ifnt >= 32) && (*ifnt <= 44)) {
		ifont = *ifnt - 32;
		*ind = 0;
	} else
		*ind = 1;
}

#ifndef UNDERSCORE
void dvchin(int4 *iasc,const int4 *nchar)
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
