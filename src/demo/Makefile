### $Id$ ###

include ../Makefile.arch

default : demo

demo : plasma plasmax

plasma : plasma.o
	$(FC) $(FFLAGS) -o plasma plasma.o $(GDPLIBS) 

plasmax : plasmax.o
	$(FC) $(FFLAGS) -o plasmax plasmax.o $(GDPLIBS) 

clean :
	rm -f *.o ./*~ ./#*# core*

veryclean : clean
	rm -f *test

plasma.o: plasma.f plasma.inc
plasmax.o: plasmax.f plasma.inc
