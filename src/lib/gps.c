/* $Id$ */
#include <stdio.h>
#include <math.h>
#include <time.h>

#ifdef LONGINT
#define		int4	int
#else
#define		int4	long
#endif

#ifdef SONYCISC
#define M_PI            3.14159265358979323846
extern char *getenv();
#else
#include <stdlib.h>
#endif
#define 	deg	*M_PI/180.

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifndef PSHEADER1
#define PSHEADER1 "/usr/local/lib/gsaf_header1.ps"
#endif

#ifndef PSHEADER2
#define PSHEADER2 "/usr/local/lib/gsaf_header2.ps"
#endif


float		xpos,ypos;
float		delta;
float		dxch,dych,fxxch,fyxch,fxych,fyych;
float		lwidth,cred,cgreen,cblue,cblack;
int		imv,ilns,ifont,ifused[13];
float		dchs,dchss,dchsp;
char		file_basename[256];
char		filename_tail[256];
FILE		*psfile,*hdfile;
int             interactive,istart,iend,inum,ititle,irotate,icolor;
int		psmode,igouraud,ipage;

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
        char    *strptr,*parm;
  	char	str[256];
	int     len; 
        
        delta = (72.0/25.4)*(256.0/(1024*32)); /* 72 pt = 1 in = 25.4 mm */
	psmode = 0;
	igouraud = 0;

	parm = getenv("GSAF_PARM_TEMP");
	if(parm) {
          sscanf(parm,"%s %d %d %d %d %d %d %d",
                str,&interactive,&istart,&iend,&inum,&ititle,&irotate,&icolor);
	  if(str[0] == '\0')
	    {
	      fprintf(stderr,"## Illegal ps file name ! \n");
	      exit(1);
	    }
	  len = strlen(str);
	  strncpy(file_basename,str+1,len-2);
          if(len > 5)
	    {
	      strncpy(filename_tail,str+len-4,3);
	      if(!strcmp(filename_tail,".gs")) file_basename[len-5]='\0';
	    }
	  if (interactive >= 4) igouraud = 1;
	  interactive = interactive % 4;
	  if (interactive >= 2) psmode = 1;
	}
	else {
	  fprintf(stderr," ## Parameter env var not found ! \n");
	  exit(1);
	}
	ipage = 0;
	if(psmode == 1) dvheader(1);
	*ich = 0;
}

#ifndef UNDERSCORE
  dvclos(ich)
#else
  dvclos_(ich)
#endif
int4		*ich;
{
	if(psmode == 1)
	  {
	    dvtrailer(ipage);
	    fclose(psfile);
	  }
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

dvheader(npage)
int4		npage;
{
  	char	filename[256],buffer[256];
  	time_t	nseconds;
  	char	*timestr;
	int     i;

	if(psmode == 1)
	  {
	    if(interactive == 2)
	      {
		psfile = stdout;
	      }
	    else
	      {
		sprintf(filename,"%s.ps",file_basename);
		psfile = fopen(filename,"w");
	      }
	  }
	else
	  {
	    sprintf(filename,"%s-%d.eps",file_basename,npage);
	    psfile = fopen(filename,"w");
	  }
	fprintf(psfile,"%%!PS-Adobe-2.0 EPSF-1.2\n");
	fprintf(psfile,"%%%%Creator: GSAF gps.c V3.74\n");
	sprintf(filename,"%s#%d",file_basename,npage);
	fprintf(psfile,"%%%%Title: (%s)\n",filename);
	nseconds = time(NULL);
	timestr = ctime(&nseconds);
	timestr[strlen(timestr)-1] = 0;
	fprintf(psfile,"%%%%Creation date: (%s)\n",timestr); 
	fprintf(psfile,"%%%%DocumentProcSets: Adobe_Illustrator_1.1 0 0\n");
	fprintf(psfile,"%%%%DocumentSuppliedProcSets: Adobe_Illustrator_1.1 0 0\n");
	fprintf(psfile,"%%%%Pages: (atend)\n");
	if (irotate == 1)
	  {       
	    fprintf(psfile,"%%%%BoundingBox: 56 28 782 570\n");
	  }
	else
	  {
            fprintf(psfile,"%%%%BoundingBox: 28 56 570 782\n");
          }
	fprintf(psfile,"%%%%DocumentFonts: (atend)\n");
	if (icolor == 2)
	  {       
	    fprintf(psfile,"%%%%DocumentProcessColors: Color\n");
	  }
	else
	  {
	    fprintf(psfile,"%%%%DocumentProcessColors: Black\n");
	  }
	fprintf(psfile,"%%%%EndComments\n");

	hdfile = fopen(PSHEADER1,"r");
	if(!hdfile)
	{
		printf("file gsaf_header1.ps cannot be found.\n");
		exit(0);
	}
	fgets(buffer,256,hdfile);
 	while(!feof(hdfile)){
 		fputs(buffer,psfile);
		fgets(buffer,256,hdfile);
 	      }
	fclose(hdfile);

	if(igouraud == 1)
	  {
	    fprintf(psfile,"/threshold 0.03 def \n");
	    hdfile = fopen(PSHEADER2,"r");
	    if(!hdfile)
	      {
		printf("file gsaf_header2.ps cannot be found.\n");
		exit(0);
	      }
	    fgets(buffer,256,hdfile);
	    while(!feof(hdfile)){
	      fputs(buffer,psfile);
	      fgets(buffer,256,hdfile);
	    }
	    fclose(hdfile);
	  }

        for (i = 0; i < 13; i++) ifused[i] = 0;
}

dvtrailer(ipage)
int4    ipage;
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

	fprintf(psfile,"%%%%Pages: %d\n",ipage);
	fprintf(psfile,"_E end\n%%%%EOF\n");
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
	ipage=ipage+1;
	if(psmode != 1) dvheader(*npage);
	fprintf(psfile,"%%%%Page: %d %d\n",*npage,ipage);
	fprintf(psfile,"0 i 2 J 0 j 1 w 4 M []0 d\n");
	ifont = 0;
	imv = 0;
}

#ifndef UNDERSCORE
  dvpage(ich)
#else
  dvpage_(ich)
#endif
int4		*ich;
{
	if(imv)
	  fprintf(psfile,"S\n");
	if(psmode == 1)
	  {
	    fprintf(psfile,"showpage\n");
	  }
	else
	  {
	    dvtrailer(1);
	    fclose(psfile);
	  }
	*ich = 0;
}

#ifndef UNDERSCORE
  dvgrps()
#else
  dvgrps_()
#endif
{
	if(imv)
	  fprintf(psfile,"S\n");
	fprintf(psfile,"u\n");
	imv = 0;
}


#ifndef UNDERSCORE
  dvgrpe()
#else
  dvgrpe_()
#endif
{
	if(imv)
	  fprintf(psfile,"S\n");
	fprintf(psfile,"U\n");
	imv = 0;
}


dvcolor(ind)
int       ind;
{
  switch(ind) 
    {
/* move, draw */
    case 0:
      if(icolor == 2)
	{
	  fprintf(psfile,"%1.3f G\n",1.0-cblack);
	  fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
		  1.0-cred,1.0-cgreen,1.0-cblue);
	}
      else if(icolor == 0)
	{
	  fprintf(psfile,"0 G\n");
	  fprintf(psfile,"0 0 0 1 K\n");
	}
      else
	{
	  fprintf(psfile,"%1.3f G\n",1.0-cblack);
	  fprintf(psfile,"0 0 0 %1.3f K\n",cblack);
	}
      break;
/* lines */
    case 1:
      if(icolor == 2)
	{
	  fprintf(psfile,"%1.3f G\n",1.0-cblack);
	  fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
		  1.0-cred,1.0-cgreen,1.0-cblue);
	  fprintf(psfile,"1 g\n");
	  fprintf(psfile,"0 0 0 0 k\n");
	}
      else if(icolor == 0)
	{
	  fprintf(psfile,"0 G\n");
	  fprintf(psfile,"0 0 0 1 K\n");
	  fprintf(psfile,"1 g\n");
	  fprintf(psfile,"0 0 0 0 k\n");
	}
      else
	{
	  fprintf(psfile,"%1.3f G\n",1.0-cblack);
	  fprintf(psfile,"0 0 0 %1.3f K\n",cblack);
	  fprintf(psfile,"1 g\n");
	  fprintf(psfile,"0 0 0 0 k\n");
	}
      break;
/* polygon */
    case 2:
      if(icolor == 2)
	{
	  fprintf(psfile,"%1.3f G\n",1.0-cblack);
	  fprintf(psfile,"%1.3f %1.3f %1.3f 0 K\n",
		  1.0-cred,1.0-cgreen,1.0-cblue);
	  fprintf(psfile,"%1.3f g\n",1.0-cblack);
	  fprintf(psfile,"%1.3f %1.3f %1.3f 0 k\n",
		  1.0-cred,1.0-cgreen,1.0-cblue);
	}
      else if(icolor == 0)
	{
	  fprintf(psfile,"0 G\n");
	  fprintf(psfile,"0 0 0 1 K\n");
	  fprintf(psfile,"0 g\n");
	  fprintf(psfile,"0 0 0 1 k\n");
	}
      else
	{
	  fprintf(psfile,"%1.3f G\n",1.0-cblack);
	  fprintf(psfile,"0 0 0 %1.3f K\n",cblack);
	  fprintf(psfile,"%1.3f g\n",1.0-cblack);
	  fprintf(psfile,"0 0 0 %1.3f k\n",cblack);
	}
      break;
/* text */
    case 3:
      if(icolor == 2)
	{
	  fprintf(psfile,"%1.3f g\n",1.0-cblack);
	  fprintf(psfile,"%1.3f %1.3f %1.3f 0 k\n",
		  1.0-cred,1.0-cgreen,1.0-cblue);
	}
      else if(icolor == 0)
	{
	  fprintf(psfile,"0 g\n");
	  fprintf(psfile,"0 0 0 1 k\n");
	}
      else
	{
	  fprintf(psfile,"%1.3f g\n",1.0-cblack);
	  fprintf(psfile,"0 0 0 %1.3f k\n",cblack);
	}
      break;
    }
}

#ifndef UNDERSCORE
  dvmove(ix,iy)
#else
  dvmove_(ix,iy)
#endif
int4		*ix,*iy;
{
	if(imv)
	  fprintf(psfile,"S\n");
	fprintf(psfile,"%% Line\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(0);

        if (! irotate)
	  {
	    xpos = 570 - *iy * delta;
	    ypos = *ix * delta + 56;
	  }
	else
	  {
	    xpos = *ix * delta + 56;
	    ypos = *iy * delta + 56;
	  }

	fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
	imv = 1;
}

#ifndef UNDERSCORE
  dvdraw(ix,iy)
#else
  dvdraw_(ix,iy)
#endif
int4		*ix,*iy;
{
	if(!imv)
	  {	fprintf(psfile,"%% Line\n");
		fprintf(psfile,"%1.3f w\n",lwidth);
		dvcolor(0);
		fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
		imv = 1;
	  }
	
        if (! irotate)
	  {
	    xpos = 570 - *iy * delta;
	    ypos = *ix * delta + 56;
	  }
	else
	  {
	    xpos = *ix * delta + 56;
	    ypos = *iy * delta + 56;
	  }

	fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
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
        int4            ix,iy;
	
	if(imv)
	  fprintf(psfile,"S\n");
	fprintf(psfile,"%% Lines\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(1);

        for (i = 0; i < *np; i++)
	{
	  if (! irotate)
	    {
	      xpos = 570 - iyn[i] * delta;
	      ypos = ixn[i] * delta + 56;
	    }
	  else
	    {
	      xpos = ixn[i] * delta + 56;
	      ypos = iyn[i] * delta + 56;
	    }
	  if (i == 0)
	    {
	      fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
	    }
	  else
	    {
	      fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
	    }
	}

        if ((ixn[0] == ixn[*np-1]) && (iyn[0] == iyn[*np-1])) {
	  fprintf(psfile,"s\n");
	} else {
	  fprintf(psfile,"S\n");
	}
        imv = 0;
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
        int4            ix,iy;
	
	if(imv)
	  fprintf(psfile,"S\n");
	fprintf(psfile,"%% Poly\n");
	fprintf(psfile,"%1.3f w\n",lwidth);
	dvcolor(2);

        for (i = 0; i < *np; i++)
	{
	  if (! irotate)
	    {
	      xpos = 570 - iyn[i] * delta;
	      ypos = ixn[i] * delta + 56;
	    }
	  else
	    {
	      xpos = ixn[i] * delta + 56;
	      ypos = iyn[i] * delta + 56;
	    }
	  if (i == 0)
	    {
	      fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
	    }
	  else
	    {
	      fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
	    }
	}
	fprintf(psfile,"b\n");
        imv = 0;
}


#ifndef UNDERSCORE
  dvrgbtrg(ixn,iyn,ir,ig,ib)
#else
  dvrgbtrg_(ixn,iyn,ir,ig,ib)
#endif
int4		ixn[],iyn[];
int4		ir[],ig[],ib[];
{
	if(igouraud == 1)
	  {
	    int		i;
	    int4	ix,iy;
	    int		smooth;
	    int4	ired,igreen,iblue;
	    int4	ixtmp[3],iytmp[3];

	    smooth=0;
	    ired=ir[0]; igreen=ig[0]; iblue=ib[0];

	    for (i = 1; i < 3; i++)
	      {
		if (ired != ir[i] || igreen != ig[i] || iblue != ib[i]) {
		  smooth = 1;
		  break;
		}
	      }
	    for (i = 0; i < 3; i++)
	      {
		if (! irotate)
		  {
		    ixtmp[i] = 570 - iyn[i] * delta;
		    iytmp[i] = ixn[i] * delta + 56;
		  }
		else
		  {
		    ixtmp[i] = ixn[i] * delta + 56;
		    iytmp[i] = iyn[i] * delta + 56;
		  }
	      }
	    xpos=ixtmp[2]; ypos=iytmp[2];
	
	
	    if(imv)
	      fprintf(psfile,"S\n");
	    fprintf(psfile,"%% gouraudtriangle\n");
	    fprintf(psfile,"%1.3f w\n",lwidth);
	    dvcolor(2);
	    fprintf(psfile, "[%d %d %d %d %d %d]",
		    ixtmp[0], ixtmp[1], ixtmp[2],
		    iytmp[0], iytmp[1], iytmp[2]);
	    fprintf(psfile, "[%f %f %f] [%f %f %f] [%f %f %f] gouraudtriangle\n",
		    ir[0]/255.0, ig[0]/255.0, ib[0]/255.0, 
		    ir[1]/255.0, ig[1]/255.0, ib[1]/255.0,
		    ir[2]/255.0, ig[2]/255.0, ib[2]/255.0);
	    fprintf(psfile,"b\n");
	    imv = 0;
	  }
	else
	  {
	    int		i;
	    int4	ix,iy;
	    int4	ired,igreen,iblue;
	
	    if(imv)
	      fprintf(psfile,"S\n");
	    fprintf(psfile,"%% Poly\n");
	    fprintf(psfile,"%1.3f w\n",lwidth);

	    ired=  (ir[0]+ir[1]+ir[2])/3;
	    igreen=(ig[0]+ig[1]+ig[2])/3;
	    iblue= (ib[0]+ib[1]+ib[2])/3;
#ifndef UNDERSCORE
	    dvcrgb(&ired,&igreen,&iblue);
#else
	    dvcrgb_(&ired,&igreen,&iblue);
#endif
	    dvcolor(2);

	    for (i = 0; i < 3; i++)
	      {
		if (! irotate)
		  {
		    xpos = 570 - iyn[i] * delta;
		    ypos = ixn[i] * delta + 56;
		  }
		else
		  {
		    xpos = ixn[i] * delta + 56;
		    ypos = iyn[i] * delta + 56;
		  }
		if (i == 0)
		  {
		    fprintf(psfile,"%1.3f %1.3f m\n",xpos,ypos);
		  }
		else
		  {
		    fprintf(psfile,"%1.3f %1.3f L\n",xpos,ypos);
		  }
	      }
	    fprintf(psfile,"b\n");
	    imv = 0;
	  }
}


#ifndef UNDERSCORE
  dvtext(ix,iy,iasc,nchar)
#else
  dvtext_(ix,iy,iasc,nchar)
#endif
int4		*ix,*iy,*iasc,*nchar;
{
	int		i;
	
	if(imv)
	  fprintf(psfile,"S\n");

        if (! irotate)
	  {
	    xpos = 570 - *iy * delta;
	    ypos = *ix * delta + 56;
	  }
	else
	  {
	    xpos = *ix * delta + 56;
	    ypos = *iy * delta + 56;
	  }

	fprintf(psfile,"u\n");
        dvcolor(3);
	switch(ifont)
	  {
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
	for(i=0; i<*nchar; i++) 
	   switch(iasc[i])
	     {
	     case '(': fprintf(psfile,"\\050"); break;
	     case ')': fprintf(psfile,"\\051"); break;
	     case '<': fprintf(psfile,"\\074"); break;
	     case '>': fprintf(psfile,"\\076"); break;
	     case '[': fprintf(psfile,"\\133"); break;
	     case ']': fprintf(psfile,"\\135"); break;
	     case '{': fprintf(psfile,"\\173"); break;
	     case '}': fprintf(psfile,"\\175"); break;
	     case 92: fprintf(psfile,"\\"); break;
	     case '%': fprintf(psfile,"\\045"); break;
             default:  fprintf(psfile,"%c",iasc[i]);
             }
	fprintf(psfile,") t T\nU\n");
	xpos += *nchar * dxch;
	ypos +- *nchar * dych;
	imv = 0;
}

#ifndef UNDERSCORE
  dvstln(iln,ibl,icl)
#else
  dvstln_(iln,ibl,icl)
#endif
int4		*iln,*ibl,*icl;
{
	if(*iln != -1) ilns = *iln;
	if(*ibl != -1) 
	{
	  if(*ibl) 
	    {
	      lwidth = *ibl*0.5;
	    }
	  else
	    {	
	      lwidth = 0.25;
	    }
	}
	if(*icl != -1)
	{
	  switch(*icl)
	    { 
	      case 7: cred = 0.0; cgreen = 0.0; cblue = 0.0; break;
	      case 5: cred = 0.0; cgreen = 0.0; cblue = 1.0; break;
	      case 6: cred = 1.0; cgreen = 0.0; cblue = 0.0; break;
	      case 3: cred = 1.0; cgreen = 0.0; cblue = 1.0; break;
	      case 4: cred = 0.0; cgreen = 1.0; cblue = 0.0; break;
	      case 1: cred = 0.0; cgreen = 1.0; cblue = 1.0; break;
	      case 2: cred = 1.0; cgreen = 1.0; cblue = 0.0; break;
	      case 0: cred = 1.0; cgreen = 1.0; cblue = 1.0; break;
	    }
	  cblack = 1.0 - (0.15 * cblue + 0.30 * cred + 0.55 * cgreen);
	}
}

#ifndef UNDERSCORE
  dvlwdt(iw)
#else
  dvlwdt_(iw)
#endif
int4		*iw;
{
  lwidth = *iw * delta;
}

#ifndef UNDERSCORE
  dvcrgb(ir,ig,ib)
#else
  dvcrgb_(ir,ig,ib)
#endif
int4		*ir,*ig,*ib;
{
  cred   = *ir/255.0;
  cgreen = *ig/255.0;
  cblue  = *ib/255.0;
  cblack = 1.0 - (0.15 * cblue + 0.30 * cred + 0.55 * cgreen);
}

#ifndef UNDERSCORE
  dvstch(ichh,ichw,ichsp,angl,tilt,ind)
#else
  dvstch_(ichh,ichw,ichsp,angl,tilt,ind)
#endif
int4		*ichh,*ichw,*ichsp,*ind;
float		*angl,*tilt;
{
  	int		jchh,jchw;
	float		wfact,tfact,cosch,sinch;

        if (*ichh == 0)
	  {
	    jchh = 1;
	  }
	else
	  {
	    jchh = *ichh;
	  }
        if (*ichw == 0)
	  {
	    jchw = 1;
	  }
	else
	  {
	    jchw = *ichw;
	  }
	wfact = (jchw * 3.0) / (jchh * 2.0);
	tfact = 1.0 / cos (*tilt deg);
        if (! irotate)
	  {
	    cosch = cos((*angl+90) deg);
	    sinch = sin((*angl+90) deg);
	    fxxch = cos((*angl+90) deg) * wfact;
	    fyxch = sin((*angl+90) deg) * wfact;
	    fxych =-sin((*angl+*tilt+90) deg) * tfact;
	    fyych = cos((*angl+*tilt+90) deg) * tfact;
	  }
	else
	  {
	    cosch = cos((*angl) deg);
	    sinch = sin((*angl) deg);
	    fxxch = cos((*angl) deg) * wfact;
	    fyxch = sin((*angl) deg) * wfact;
	    fxych =-sin((*angl+*tilt) deg) * tfact;
	    fyych = cos((*angl+*tilt) deg) * tfact;
	  }

	dxch = *ichsp * cosch * delta;
	dych = *ichsp * sinch * delta;
	if((ifont >= 8) && (ifont <= 11)) 
	  {
	    dchs = jchh * delta * 1.666667;
	  }
	else
	  {
	    dchs = jchh * delta * 1.466667;
	  }
	dchss = dchs * 0.8;
	dchsp = (*ichsp - 1.5* *ichw) * delta ;
	*ind = 0;
}

#ifndef UNDERSCORE
  dvfont(ifnt,ind)
#else
  dvfont_(ifnt,ind)
#endif
int4		*ifnt,*ind;
{
  	if(*ifnt == 0) 
	  {
	    ifont = 8;
	    ifused[ifont] = 1;
	    *ind = 0;
	  }else if((*ifnt >= 32) && (*ifnt <= 44)) {
	    ifont = *ifnt - 32;
	    ifused[ifont] = 1;
	    *ind = 0;
	  }else{
	    *ind = 1;
	  }
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
        if (! irotate)
	  {
	    *iy = (570 - xpos) / delta;
	    *ix = (ypos - 56) / delta;
	  }
	else
	  {
	    *ix = (xpos - 56) / delta;
	    *iy = (ypos - 56) / delta;
	  }
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

