### $Id$ ###

include ../Makefile.arch

PSHEADER1=$(LIBPATH)/gsaf_header1.ps
PSHEADER2=$(LIBPATH)/gsaf_header2.ps

default : libs

libs : libgsp$(SL) libgdp$(SL) libgpp$(SL) libgps$(SL) libgdum$(SL) libg3d$(SL)

OBJGSP= bsc1.o bsc2.o bsc3.o igl1.o igl2.o igl3.o fio1.o fio2.o \
        unix.o unixc.o apl1.o apl2.o apl3.o cont1.o cont2.o cont3.o \
        pers.o spln.o \
	Acompat.o

OBJG3D= A3dplot.o Agtseries.o Argbfunc.o Apersen3d.o Acplot1.o \
	Acont3d.o Aline3d.o Acompat3d.o

plot.o : plot.f
	$(FC) -c $(FLFLAGS) plot.f

libplot$(SL) : plot.o
	$(LD) $(LDFLAGS) libplot$(SL) plot.o
	$(LDX) libplot$(SL)

A3dplot.o : A3dplot.f
	$(FC) -c $(FLFLAGS) A3dplot.f

Agtseries.o : Agtseries.f
	$(FC) -c $(FLFLAGS) Agtseries.f

Argbfunc.o : Argbfunc.f
	$(FC) -c $(FLFLAGS) Argbfunc.f

Apersen3d.o : Apersen3d.f
	$(FC) -c $(FLFLAGS) Apersen3d.f

Acplot1.o : Acplot1.f
	$(FC) -c $(FLFLAGS) Acplot1.f

Acont3d.o : Acont3d.f
	$(FC) -c $(FLFLAGS) Acont3d.f

Aline3d.o : Aline3d.f
	$(FC) -c $(FLFLAGS) Aline3d.f

libg3d$(SL) : $(OBJG3D)
	$(LD) $(LDFLAGS) libg3d$(SL) $(OBJG3D)
	$(LDX) libg3d$(SL)

bsc1.o : bsc1.f
	$(FC) -c $(FLFLAGS) bsc1.f

bsc2.o : bsc2.f
	$(FC) -c $(FLFLAGS) bsc2.f

bsc3.o : bsc3.f
	$(FC) -c $(FLFLAGS) bsc3.f

igl1.o : igl1.f
	$(FC) -c $(FLFLAGS) igl1.f

igl2.o : igl2.f
	$(FC) -c $(FLFLAGS) igl2.f

igl3.o : igl3.f
	$(FC) -c $(FLFLAGS) igl3.f

fio1.o : fio1.f
	$(FC) -c $(FLFLAGS) fio1.f

fio2.o : fio1.f
	$(FC) -c $(FLFLAGS) fio2.f

unix.o : unix.f
	$(FC) -c $(FLFLAGS) unix.f

unixc.o : unixc.c
	$(CC) -c $(CLFLAGS) unixc.c

apl1.o : apl1.f
	$(FC) -c $(FLFLAGS) apl1.f

apl2.o : apl2.f
	$(FC) -c $(FLFLAGS) apl2.f

apl3.o : apl3.f
	$(FC) -c $(FLFLAGS) apl3.f

cont1.o : cont1.f
	$(FC) -c $(FLFLAGS) cont1.f

cont2.o : cont2.f
	$(FC) -c $(FLFLAGS) cont2.f

pers.o : pers.f
	$(FC) -c $(FLFLAGS) pers.f

spln.o : spln.f
	$(FC) -c $(FLFLAGS) spln.f

libgsp$(SL) : $(OBJGSP)
	$(LD) $(LDFLAGS) libgsp$(SL) $(OBJGSP)
	$(LDX) libgsp$(SL)

gdp.o : gdp.c
	$(CC) -c $(CLFLAGS) -I$(XINCPATH) gdp.c

libgdp$(SL) : gdp.o
	$(LD) $(LDFLAGS) libgdp$(SL) gdp.o
	$(LDX) libgdp$(SL)

gpplips.o : gpplips.c
	$(CC) -c $(CLFLAGS) gpplips.c

libgpp$(SL) : gpplips.o
	$(LD) $(LDFLAGS) libgpp$(SL) gpplips.o
	$(LDX) libgpp$(SL)

gps.o : gps.c
	$(CC) -c $(CLFLAGS) -DPSHEADER1=\"$(PSHEADER1)\" -DPSHEADER2=\"$(PSHEADER2)\" gps.c

libgps$(SL) : gps.o
	$(LD) $(LDFLAGS) libgps$(SL) gps.o
	$(LDX) libgps$(SL)

gdum.o : gdum.f
	$(FC) -c $(FLFLAGS) gdum.f

libgdum$(SL) : gdum.o
	$(LD) $(LDFLAGS) libgdum$(SL) gdum.o
	$(LDX) libgdum$(SL)

install : libgsp$(SL) libgdp$(SL) libgpp$(SL) libgps$(SL) libgdum$(SL) \
	  libg3d$(SL)
	cp libgsp$(SL) libgdp$(SL) libgpp$(SL) libgps$(SL) libgdum$(SL) \
	   libg3d$(SL) $(LIBPATH)
	-cp gsaf_header?.ps $(LIBPATH)
	$(LDX) $(LIBPATH)/libgsp$(SL) $(LIBPATH)/libgdp$(SL) $(LIBPATH)/libgpp$(SL) $(LIBPATH)/libgps$(SL) $(LIBPATH)/libgdum$(SL) $(LIBPATH)/libg3d$(SL)

installplot : libplot$(SL)
	cp libplot$(SL) $(LIBPATH)
	$(LDX) $(LIBPATH)/libplot$(SL)

clean :
	rm -f *.o ./*~ ./#*# core*

veryclean : clean
	rm -f *$(SL) 

#check:	
#	ftnchek bsc*.f fio*.f igl*.f unix.f | less
