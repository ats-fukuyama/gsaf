### $Id$ ####
# Make for gsaf : V3.80 for SX-static [2002/06/26]
#################################################################
#
# Default install path

#BINPATH=/usr/local/bin
#LIBPATH=/usr/local/lib

# Home install path

HOMEPATH=/home/j024
BINPATH=$(HOMEPATH)/bin
LIBPATH=$(HOMEPATH)/lib

# X library path

XINCPATH=/SX/usr/include/X11
XLIBPATH=/SX/usr/lib

# Library extension

SL=.a

# Compiler command

FC=sxf90
CC=sxcc

# Make library command and options

LD=sxar
LDX=echo
LDFLAGS=ru

# Libraries for X window application

XLIBS=$(XLIBPATH)/libX11$(SL)

# Libraries for C mathematical functions in Fortran compile

MLIBS=

# Compile flags

FLFLAGS=
CLFLAGS=-DUNDERSCORE
FFLAGS=
CFLAGS=-DUNDERSCORE

# Print command (No translation mode)

LP=lpr

# Library difinition

GSPLIB=$(LIBPATH)/libgsp$(SL)
GDPLIB=$(LIBPATH)/libgdp$(SL) 
GPPLIB=$(LIBPATH)/libgpp$(SL) 
GPSLIB=$(LIBPATH)/libgps$(SL)
GDUMLIB=$(LIBPATH)/libgdum$(SL) 

LIBS=-L. -lgsp -lgdp -L$(XLIBPATH) -lX11 $(MLIBS)
GSLIBS=-L$(LIBPATH) -lgsp -lgdp -L$(XLIBPATH) -lX11 $(MLIBS)
DUMLIBS=-L. -lgsp -lgdum -L$(XLIBPATH) -lX11 $(MLIBS)
GDUMLIBS=-L$(LIBPATH) -lgsp -lgdum -L$(XLIBPATH) -lX11 $(MLIBS)
ADLIBS=-L$(LIBPATH) -lgsp -lgdp -L$(XLIBPATH) -lX11 $(MLIBS)
APLIBS=-L$(LIBPATH) -lgsp -lgpp -L$(XLIBPATH) -lX11 $(MLIBS)
APSLIBS=-L$(LIBPATH) -lgsp -lgps -L$(XLIBPATH) -lX11 $(MLIBS)

# Necessary dependence for static link

LIBSX=./libgsp$(SL) ./libgdp$(SL)
LIBDSX=./libgsp$(SL) ./libgdum$(SL)
ADLIBSX=$(GSPLIB) $(GDPLIB) 
APLIBSX=$(GSPLIB) $(GPPLIB)
APSLIBSX=$(GSPLIB) $(GPSLIB)

# End of site configuration
#################################################################
include Makefile.body
