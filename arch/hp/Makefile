#
# Make for gsaf : V3.80 for HP9000/700 [2001/10/24]
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

SL=.sl

# Compiler command

FC=f77
CC=cc

# Make library command and options

LD=ld
LDX=:
LDFLAGS=-b -o

# Libraries for X window application

XLIBS=$(XLIBPATH)/libX11$(SL)

# Libraries for C mathematical functions in Fortran compile

MLIBS=-lm

# Compile flags

#FLFLAGS=+z +O2 -K
#CLFLAGS=+z +O2
#FFLAGS=-K +O2
#CFLAGS=+O2
FLFLAGS=+z -K
CLFLAGS=+z
FFLAGS=-K
CFLAGS=
#FLFLAGS=+z -g -K
#CLFLAGS=+z -g
#FFLAGS=-K -g
#CFLAGS=-g

# Print command (No translation mode)

LP=lp -opure

# Library difinition

LIBS=-Wl,-L. -lgsp -lgdp -Wl,-L$(XLIBPATH) -lX11 $(MLIBS)
GSLIBS=-Wl,-L$(LIBPATH) -lgsp -lgdp -Wl,-L$(XLIBPATH) -lX11 $(MLIBS)
DUMLIBS=-Wl,-L. -lgsp -lgdum -Wl,-L$(XLIBPATH) -lX11 $(MLIBS)
GDUMLIBS=-Wl,-L$(LIBPATH) -lgsp -lgdum -Wl,-L$(XLIBPATH) -lX11 $(MLIBS)
ADLIBS=-Wl,-L$(LIBPATH) -lgsp -lgdp  -Wl,-L$(XLIBPATH) -lX11 $(MLIBS)
APLIBS=-Wl,-L$(LIBPATH) -lgsp -lgpp  -Wl,-L$(XLIBPATH) -lX11 $(MLIBS)
APSLIBS=-Wl,-L$(LIBPATH) -lgsp -lgps  -Wl,-L$(XLIBPATH) -lX11 $(MLIBS)

# Necessary dependence for static link

LIBSX=
LIBDSX=
ADLIBSX=
APLIBSX=
APSLIBSX=

# End of site configuration
#################################################################
include Makefile.body
