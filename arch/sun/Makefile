#
# Make for gsaf : V3.80 for SUN OS 4.1 [2001/10/24]
#################################################################
#
# Default install path

#BINPATH=/usr/local/bin
#LIBPATH=/usr/local/lib

# Home install path

HOMEPATH=/home/fukuyama
BINPATH=$(HOMEPATH)/bin
LIBPATH=$(HOMEPATH)/lib

# X library path

XINCPATH=/usr/openwin/include
XLIBPATH=/usr/openwin/lib

# Library extension

SL=.so

# Compiler command

FC=f77
CC=cc

# Make library command and options

LD=ld
LDX=echo Created:
LDFLAGS=-o

# Libraries for X window application

XLIBS=$(XLIBPATH)/libX11$(SL)

# Libraries for C mathematical functions in Fortran compile

MLIBS=

# Compile flags

FLFLAGS=-O
CLFLAGS=-O -DUNDERSCORE -DSUN
FFLAGS=-O
CFLAGS=-O -DUNDERSCORE -DSUN

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
GDUMLIBS=$(GSPLIB) $(GDUMLIB) $(XLIBS) $(MLIBS)
ADLIBS=$(GSPLIB) $(GDPLIB) $(XLIBS) $(MLIBS)
APLIBS=$(GSPLIB) $(GPPLIB) $(MLIBS)
APSLIBS=$(GSPLIB) $(GPSLIB) $(MLIBS)

# Necessary dependence for static link

LIBSX=
LIBDSX=
ADLIBSX=
APLIBSX=
APSLIBSX=

# End of site configuration
#################################################################
include Makefile.body
