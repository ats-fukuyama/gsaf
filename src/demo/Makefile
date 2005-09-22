### $Id$ ###

include ../Makefile.arch

default : all

all : plasma plasmax demo

plasma : plasma.o particle.o random.o
	$(FC) $(FFLAGS) -o plasma plasma.o particle.o random.o $(GDPLIBS) 

plasmax : plasmax.o
	$(FC) $(FFLAGS) -o plasmax plasmax.o $(GDPLIBS) 

demo : demo.o particle.o button.o random.o
	$(FC) $(FFLAGS) -o demo demo.o particle.o button.o random.o $(GDPLIBS) 

clean :
	rm -f *.o ./*~ ./#*# core*

veryclean : clean
	rm -f *test

plasma.o: plasma.f particle.f random.f plasma.inc
plasmax.o: plasmax.f plasma.inc
demo.o: demo.f particle.f button.f random.f plasma.inc button.inc
