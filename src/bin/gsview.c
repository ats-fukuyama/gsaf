/* $Id$ */
#include <stdio.h>
#include <string.h>

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

#define MAX_STR	256

static char *basename(name)
char *name;
{
	char *base;
	unsigned len;

	base = NULL;
	for (len = strlen(name); len > 0; len--) {
		if (name[len - 1] == '/') {
			base = name + len;
			break;
		}
	}

	if (!base)
		base = name;

	return base;
}

int main (argc,argv)
int argc;
char **argv;
{
	int c,len,errflg;
	int istart,iend,inum,ititle,interactive,irotate,icolor;
	int igouraud,status;
	char filename[MAX_STR];
	char progname[MAX_STR];
	char str[MAX_STR];
	char *strptr;

	extern char *optarg;
	extern int optind, optopt;
  
	istart = 1;
	iend = 999;
	inum = 1;
	ititle = 0;
	interactive = 1;
	irotate = 0;
	icolor = 2;
	igouraud = 0;
	errflg = 0;

	while ((c = getopt(argc, argv, ":abce:gmp:rs:tz")) != -1) {
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
		case 'z':
			icolor = 1;
			break;
		case ':':	/* -ens without arguments */
			fprintf(stderr, "Option -%c requires an argument\n", optopt);
			errflg++;
			break;
		case '?':
			fprintf(stderr, "Unrecognized option: - %c\n", optopt);
			errflg++;
		}
	}

	if (errflg) {
		fprintf(stderr, "usage: %s [-atbrcmgz] [-s ps] [-e pe] [-p np] [filename]\n"
				"           -a    : show all page\n"
				"           -s ps : show from page ps [1]\n"
				"           -e pe : show until page pe [999]\n"
				"           -p np : combine np pages on a sheet [1]\n"
				"           -t    : keep original page title\n"
				"           -b    : no page title\n"
				"           -r    : rotate figure, valid for gstops/eps \n"
				"           -c    : color figure, valid for gstops/eps \n"
				"           -z    : gray figure, valid for gstops/eps \n"
				"           -m    : monochrome figure, valid for gstops/eps \n"
				"           -g    : gouraud shading (cannot be edited), valid for gstops/eps \n"
				"           filename : if not specified, prompted\n",
			argv[0]);
		exit(2);
	}

/*
	strcpy(progname,BINPATH);
	strcat(progname,"/");
	strcat(progname,argv[0]);
 */
	strcpy(progname,argv[0]);
	if (!strcmp(basename(progname),"gstops"))
		interactive = interactive + 2;
	if (igouraud == 1)
		interactive = interactive + 4;
	strcat(progname,".slave");
/*
	printf("%s\n", progname);
	printf("%s\n", basename(progname));
	printf("%d\n", interactive);
 */

	if (optind >= argc) {
		printf("## INPUT : FILENAME\n");
		strptr=fgets(filename,MAX_STR,stdin);
		if (strptr != NULL) {
			len=strlen(filename);
			if (len != 0)
				filename[len-1]='\0';
			if (!access(filename,R_OK)) {
				sprintf(str, "GSAF_PARM_TEMP='%s' %d %d %d %d %d %d %d\n",
					filename,interactive,istart,iend,
					inum,ititle,irotate,icolor);
				putenv(str);
				status=system(progname);
			} else {
				fprintf(stderr, "File %s not found\n",filename);
			}
		}
	} else {
		for ( ; optind < argc; optind++) {
			if (!access(argv[optind], R_OK)) {
				sprintf(str, "GSAF_PARM_TEMP='%s' %d %d %d %d %d %d %d\n",
					argv[optind],interactive,istart,iend,
					inum,ititle,irotate,icolor);
				putenv(str);
				status=system(progname);
			} else {
				fprintf(stderr, "File %s not found\n",argv[optind]);
			}
		}  
	}

	return 0;
}
