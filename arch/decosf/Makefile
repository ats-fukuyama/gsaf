### $id$ ####
# Make for gsaf : V3.80 for DEC ALPHA OSF [2001/10/24]
#################################################################
#
# Default install path

BINPATH=/usr/local/bin
LIBPATH=/usr/loca/lib

# Home install path

#HOMEPATH=/home/hamamatu/
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

XLIBS=$(XLIBPATH)/libX11$(SL) /usr/lib/libdnet_stub.a

# Libraries for C mathematical functions in Fortran compile

MLIBS=

# Compile flags

FLFLAGS=-O -fpe1
CLFLAGS=-O -DUNDERSCORE -DLONGINT
FFLAGS=-O -fpe1
CFLAGS=-O -DUNDERSCORE -DLONGINT

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

LIBSX=./libgsp$(SL) ./libgdp$(SL)
LIBDSX=./libgsp$(SL) ./libgdum$(SL)
ADLIBSX=$(GSPLIB) $(GDPLIB) 
APLIBSX=$(GSPLIB) $(GPPLIB)
APSLIBSX=$(GSPLIB) $(GPSLIB)

# End of site configuration
#################################################################
include Makefile.body

