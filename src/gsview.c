#include <stdio.h>

#ifndef SONYCISC
#include <unistd.h>
#include <stdlib.h>
#else
#define  R_OK  4
extern char *getenv();
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

char *basename(name)
char *name;
{
  char *base;
  unsigned len;
  
  base = NULL;
  for (len = strlen (name); len > 0; len--) {
    if (name[len - 1] == '/') 
      {
	base = name + len;
	break;
      }
  }

  if (!base)
    base = name;
  
  return base;
}

main (argc,argv)
int argc;
char **argv;
{
  int c,len,errflg;
  int istart,iend,inum,ititle,interactive,irotate,icolor;
  int igouraud;
  char filename[256];
  char progname[256];
  char str[256];
  char *strptr;

  extern char *optarg;
  extern int optind, optopt;
  
  istart = 1;
  iend = 999;
  inum = 1;
  ititle = 0;
  interactive = 1;
  irotate = 0;
  icolor = 1;
  igouraud = 0;
  errflg = 0;

  while ((c = getopt(argc, argv, ":abce:gmp:rs:t")) != -1)
    switch (c) {
    case 'a':
      istart = 0;
      interactive = 0;
      break;
    case 'b':
      ititle = -1;
      break;
    case 'c':
      icolor = 2;
      break;
    case 'e':
      if (istart == 0)
	istart = 1;
      iend = atoi(optarg);
      interactive = 0;
      break;
    case 'g':
      igouraud = 1;
      break;
    case 'm':
      icolor = 0;
      break;
    case 'p':
      inum = atoi(optarg);
      break;
    case 'r':
      irotate = 1;
      break;
    case 's':
      istart = atoi(optarg);
      interactive = 0;
      break;
    case 't':
      ititle = 1;
      break;
    case ':':        /* -ens without arguments */
      fprintf(stderr, "Option -%c requires an argument\n",
	      optopt);
      errflg++;
      break;
    case '?':
      fprintf(stderr, "Unrecognized option: - %c\n",
	      optopt);
      errflg++;
    }
  if (errflg) {
    fprintf(stderr, "usage: %s [-atbrcmg] [-s ps] [-e pe] [-p np] [filename]\n", argv[0]);
    fprintf(stderr, "           -a    : show all page\n");
    fprintf(stderr, "           -s ps : show from page ps [1]\n");
    fprintf(stderr, "           -e pe : show until page pe [999]\n");
    fprintf(stderr, "           -p np : combine np pages on a sheet [1]\n");
    fprintf(stderr, "           -t    : keep original page title\n");
    fprintf(stderr, "           -b    : no page title\n");
    fprintf(stderr, "           -r    : rotate figure, valid for gstops/eps \n");
    fprintf(stderr, "           -c    : color figure, valid for gstops/eps \n");
    fprintf(stderr, "           -m    : monochrome figure, valid for gstops/eps \n");
    fprintf(stderr, "           -g    : gouraud shading (cannot be edited), valid for gstops/eps \n");
    fprintf(stderr, "           filename : if not specified, prompted\n");
    exit (2);
  }

/*
  strcpy(progname,BINPATH);
  strcat(progname,"/");
  strcat(progname,argv[0]);
*/
  strcpy(progname,argv[0]);
  if(!strcmp(basename(progname),"gstops")) interactive = interactive + 2;
  if(igouraud == 1) interactive = interactive + 4;
  strcat(progname,".slave");
/*
  printf("%s\n",progname);
  printf("%s\n",basename(progname));
  printf("%d\n",interactive);
*/

  if (optind >= argc) {
    printf("## INPUT : FILENAME\n");
    strptr=fgets(filename,256,stdin);
    if(strptr != NULL)
      {
	len=strlen(filename);
	if(len != 0)
	  filename[len-1]='\0';
	if (!access(filename,R_OK)) {
	  sprintf(str,"GSAF_PARM_TEMP='%s' %d %d %d %d %d %d %d\n",
		  filename,interactive,istart,iend,inum,ititle,irotate,icolor);
	  putenv(str);
	  system(progname);
	} 
	else {
	  fprintf(stderr,"File %s not found\n",filename);
	}
      }
  }
  else {
    for ( ; optind < argc; optind++) {
      if (!access(argv[optind], R_OK)) {
        sprintf(str,"GSAF_PARM_TEMP='%s' %d %d %d %d %d %d %d\n",
              argv[optind],interactive,istart,iend,inum,ititle,irotate,icolor);
        putenv(str);
        system(progname);
        } 
      else {
	fprintf(stderr,"File %s not found\n",argv[optind]);
      }
    }  
  }
}

