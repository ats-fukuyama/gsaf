### $Id$ ####
# Make for gsaf : V3.80 for SONY CISC [2001/10/24]
#################################################################
#
# Default install path

BINPATH=/usr/local/bin
LIBPATH=/usr/local/lib

# Home install path

#HOMEPATH=/mnt0f/usr/takeno/gslib2
#BINPATH=$(HOMEPATH)/bin
#LIBPATH=$(HOMEPATH)/lib

# X library path

XINCPATH=/usr/include/X11
XLIBPATH=/usr/lib

# Library extension

SL=.a

# Compiler command

FC=f77
CC=cc

# Make library command and options

LD=ar
LDX=ranlib
LDFLAGS=ru

# Libraries for X window application

XLIBS=$(XLIBPATH)/libX11$(SL)

# Libraries for C mathematical functions in Fortran compile

MLIBS=

# Compile flags

FLFLAGS=-O 
CLFLAGS=-O -DUNDERSCORE -DSONYCISC
FFLAGS=-O
CFLAGS=-O -DUNDERSCORE -DSONYCISC

# Print command (No translation mode)

LP=lpr

# Library difinition

GSPLIB=$(LIBPATH)/libgsp$(SL)
GDPLIB=$(LIBPATH)/libgdp$(SL) 
GPPLIB=$(LIBPATH)/libgpp$(SL) 
GPSLIB=$(LIBPATH)/libgps$(SL)
GDUMLIB=$(LIBPATH)/libgdum$(SL) 

LIBS=./libgsp$(SL) ./libgdp$(SL) $(XLIBS) $(MLIBS)
GSLIBS=$(GSPLIB) $(GDPLIB) $(XLIBS) $(MLIBS)
DUMLIBS=./libgsp$(SL) ./libgdum$(SL) $(XLIBS) $(MLIBS)
GDUMSLIBS=$(GSPLIB) $(GDUMLIB) $(XLIBS) $(MLIBS)
ADLIBS=$(GSPLIB) $(GDPLIB) $(XLIBS) $(MLIBS)
APLIBS=$(GSPLIB) $(GPPLIB) $(MLIBS)
APSLIBS=$(GSPLIB) $(GPSLIB) $(MLIBS)

# Necessary dependence for static link

LIBSX=./libgsp$(SL) ./libgdp$(SL)
LIBDSX=./libgsp$(SL) ./libgdum$(SL)
ADLIBSX=$(GSPLIB) $(GDPLIB) 
APLIBSX=$(GSPLIB) $(GPPLIB)
APSLIBSX=$(GSPLIB) $(GPSLIB)

# End of site configuration
#################################################################
include Makefile.body
