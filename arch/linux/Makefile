#
# Make for gsaf : V3.80 for Linux-dynamic [2001/10/24]
#################################################################
#        You have to set LD_LIBRARY_PATH=.:$(LIBPATH).          #
#################################################################
#
# Default install path

BINPATH=/usr/local/bin
LIBPATH=/usr/local/lib

# Home install path

#HOMEPATH=/home/fukuyama
#BINPATH=$(HOMEPATH)/bin
#LIBPATH=$(HOMEPATH)/lib

# X library path

XINCPATH=/usr/X11R6/include
XLIBPATH=/usr/X11R6/lib

# Library extension

SL=.so

# Compiler command

FC=g77
CC=cc

# Make library command and options

LD=ld
LDX=echo Created:
LDFLAGS=-shared -o

# Libraries for X window application

XLIBS=$(XLIBPATH)/libX11$(SL)

# Libraries for C mathematical functions in Fortran compile

MLIBS=

# Compile flags

FLFLAGS=-O -fpic
CLFLAGS=-O -fpic -DUNDERSCORE
FFLAGS=-O
CFLAGS=-O -DUNDERSCORE

# Print command (No translation mode)

LP=lpr

# Library difinition

LIBS=-L. -lgsp -lgdp -L$(XLIBPATH) -lX11 $(MLIBS)
GSLIBS=-L$(LIBPATH) -lgsp -lgdp -L$(XLIBPATH) -lX11 $(MLIBS)
DUMLIBS=-L. -lgsp -lgdum -L$(XLIBPATH) -lX11 $(MLIBS)
GDUMLIBS=-L$(LIBPATH) -lgsp -lgdum -L$(XLIBPATH) -lX11 $(MLIBS)
ADLIBS=-L$(LIBPATH) -lgsp -lgdp -L$(XLIBPATH) -lX11 $(MLIBS)
APLIBS=-L$(LIBPATH) -lgsp -lgpp -L$(XLIBPATH) -lX11 $(MLIBS)
APSLIBS=-L$(LIBPATH) -lgsp -lgps -L$(XLIBPATH) -lX11 $(MLIBS)

# Necessary dependence for static link

LIBSX=
LIBDSX=
ADLIBSX=
APLIBSX=
APSLIBSX=

# End of site configuration
#################################################################
include Makefile.body
