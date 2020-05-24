### $Id$ ###

include ../Makefile.arch

default : test

test : bsctest apltest conttest perstest splntest \
       gdptest gdumtest pgtest pstest sptest g3dtest evtest pagetest \
       imagetest gpstest

gtest : gtest.o bsc1.o fio1.o fio2.o igl1.o igl2.o igl3.o gdp.o unix.o unixc.o
	$(FC) $(FFLAGS) -o gtest gtest.o bsc1.o fio1.o fio2.o \
           igl1.o igl2.o igl3.o gdp.o unix.o unixc.o $(XLIBS) $(MLIBS)

ftest : ftest.o fio1.o fio2.o
	$(FC) $(FFLAGS) -o ftest ftest.o unix.o unixc.o

btest : bsctest.o bsc1.o fio1.o fio2.o igl1.o igl2.o igl3.o \
           gdp.o unix.o unixc.o
	$(FC) $(FFLAGS) -o btest bsctest.o bsc1.o fio1.o fio2.o \
           igl1.o igl2.o igl3.o gdp.o unix.o unixc.o $(XLIBS) $(MLIBS)

gdptest : gdptest.o $(LIBSX)
	$(FC) $(FFLAGS) -o gdptest gdptest.o $(LIBS)

bsctest : bsctest.o $(LIBSX)
	$(FC) $(FFLAGS) -o bsctest bsctest.o $(LIBS)

apltest : apltest.o $(LIBSX)
	$(FC) $(FFLAGS) -o apltest apltest.o $(LIBS)

conttest : conttest.o $(LIBSX)
	$(FC) $(FFLAGS) -o conttest conttest.o $(LIBS)

perstest : perstest.o $(LIBSX)
	$(FC) $(FFLAGS) -o perstest perstest.o $(LIBS)

splntest : splntest.o $(LIBSX)
	$(FC) $(FFLAGS) -o splntest splntest.o $(LIBS)

pgtest : pgtest.o $(LIBSX)
	$(FC) $(FFLAGS) -o pgtest pgtest.o $(LIBS)

pstest : pstest.o $(LIBSX)
	$(FC) $(FFLAGS) -o pstest pstest.o $(LIBS)

sptest : sptest.o $(LIBSX)
	$(FC) $(FFLAGS) -o sptest sptest.o $(LIBS)

g3dtest : g3dtest.o $(LIBSX)
	$(FC) $(FFLAGS) -o g3dtest g3dtest.o $(LIBS)

evtest : evtest.o $(LIBSX)
	$(FC) $(FFLAGS) -o evtest evtest.o $(LIBS)

pagetest : pagetest.o $(LIBSX)
	$(FC) $(FFLAGS) -o pagetest pagetest.o $(LIBS)

imagetest : imagetest.o $(LIBSX)
	$(FC) $(FFLAGS) -o imagetest imagetest.o $(LIBS)

plottest : plottest.o $(LIBSX)
	$(FC) $(FFLAGS) -o plottest plottest.o $(LIBS)

gdumtest : gdumtest.o $(GDUMLIBSX)
	$(FC) $(FFLAGS) -o gdumtest gdumtest.o $(LIBSDUM)

gpstest : gpstest.o $(LIBSX)
	$(FC) $(FFLAGS) -o gpstest gpstest.o $(LIBS)

frtest : frtest.o $(LIBSX)
	$(FC) $(FFLAGS) -o frtest frtest.o $(LIBS)

clean :
	rm -f *.o ./*~ ./#*# core*

veryclean : clean
	rm -f *test
