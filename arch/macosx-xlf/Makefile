#
# Make for gsaf : V3.80 for macosx-xlf [2003/08/31]
#################################################################
#        You have to set LD_LIBRARY_PATH=./$(LIBPATH).          #
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

LIB-SUFFIX=-xlf
SL=$(LIB-SUFFIX).a

# Compiler command

FC=xlf
CC=xlc

# Make library command and options

LD=ld
LDX=echo Created:
LDFLAGS=-r -o

# Libraries for X window application

XLIBS=$(XLIBPATH)/libX11$(SL)

# Libraries for C mathematical functions in Fortran compile

MLIBS=

# Compile flags

FLFLAGS=-O
CLFLAGS=-O -DBSD
FFLAGS=-O
CFLAGS=-O -DBSD

# Print command (No translation mode)

LP=lpr

# Library difinition

LIBS=-L. -lgsp$(LIB-SUFFIX) -lgdp$(LIB-SUFFIX) -L$(XLIBPATH) -lX11 $(MLIBS)
GSLIBS=-L$(LIBPATH) -lgsp$(LIB-SUFFIX) -lgdp$(LIB-SUFFIX) -L$(XLIBPATH) -lX11 $(MLIBS)
DUMLIBS=-L. -lgsp$(LIB-SUFFIX) -lgdum$(LIB-SUFFIX) -L$(XLIBPATH) -lX11 $(MLIBS)
GDUMLIBS=-L$(LIBPATH) -lgsp$(LIB-SUFFIX) -lgdum$(LIB-SUFFIX) -L$(XLIBPATH) -lX11 $(MLIBS)
ADLIBS=-L$(LIBPATH) -lgsp$(LIB-SUFFIX) -lgdp$(LIB-SUFFIX) -L$(XLIBPATH) -lX11 $(MLIBS)
APLIBS=-L$(LIBPATH) -lgsp$(LIB-SUFFIX) -lgpp$(LIB-SUFFIX) -L$(XLIBPATH) -lX11 $(MLIBS)
APSLIBS=-L$(LIBPATH) -lgsp$(LIB-SUFFIX) -lgps$(LIB-SUFFIX) -L$(XLIBPATH) -lX11 $(MLIBS)

# Necessary dependence for static link

LIBSX=
LIBDSX=
ADLIBSX=
APLIBSX=
APSLIBSX=

# End of site configuration
#################################################################
include Makefile.body
