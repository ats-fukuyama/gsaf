### $Id$ ####
# Make for gsaf : V3.80 for HP9000/700 without cc +z [2001/10/24]
#################################################################
#
# Default install path

BINPATH=/usr/local/bin
LIBPATH=/usr/local/lib

# Home install path

#HOMEPATH=/users/fukuyama
#BINPATH=$(HOMEPATH)/bin
#LIBPATH=$(HOMEPATH)/lib

# X library path

XINCPATH=/usr/include/X11R5
XLIBPATH=/usr/lib/X11R5

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

MLIBS=-lm

# Compile flags

#FLFLAGS=-K +O2
#CLFLAGS=+O2
#FFLAGS=-K +O2
#CFLAGS=+O2
FLFLAGS=-K
CLFLAGS=
FFLAGS=-K
CFLAGS=
#FLFLAGS=-K -g
#CLFLAGS=-g
#FFLAGS=-K -g
#CFLAGS=-g

# Print command (No translation mode)

LP=lp -oraw

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

LIBSX=./libgsp$(SL) ./libgdp$(SL)
LIBDSX=./libgsp$(SL) ./libgdum$(SL)
ADLIBSX=$(GSPLIB) $(GDPLIB) 
APLIBSX=$(GSPLIB) $(GPPLIB)
APSLIBSX=$(GSPLIB) $(GPSLIB)

# End of site configuration
#################################################################
include Makefile.body
