#
# Make for gsaf : V3.80 for NEC EWS RISC [2001/10/24]
#################################################################
#
# Default install path

BINPATH=/usr/local/bin
LIBPATH=/usr/local/lib

# Home install path

#HOMEPATH=/SOFT/uhome/user/jgfv0111
#BINPATH=$(HOMEPATH)/bin
#LIBPATH=$(HOMEPATH)/lib

# X library path

XINCPATH=/usr/include/X11R5
XLIBPATH=/usr/lib

# Library extension

SL=.so

# Compiler command

FC=f77
CC=cc

# Make library command and options

LD=ld
LDX=echo Created:
LDFLAGS=-G -o

# Libraries for X window application

XLIBS=$(XLIBPATH)/libX11$(SL).1 \
      $(XLIBPATH)/libsocket$(SL) \
      $(XLIBPATH)/libnsl$(SL)

# Libraries for C mathematical functions in Fortran compile

MLIBS=

# Compile flags

FLFLAGS=-O -G
CLFLAGS=-O -G -DUNDERSCORE
FFLAGS=-O
CFLAGS=-O -DUNDERSCORE

# Print command (No translation mode)

LP=lp -opure

# Due to compiler's bug, optimization must be skipped for a while.

pers.o: pers.f
	$(FC) -c -G pers.f

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
