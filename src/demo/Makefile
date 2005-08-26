### $Id$ ###

include ../Makefile.arch

default : demo

demo : plasma

plasma : plasma.o
	$(FC) $(FFLAGS) -o plasma plasma.o $(GDPLIBS) 

clean :
	rm -f *.o ./*~ ./#*# core*

veryclean : clean
	rm -f *test

plasma.o: plasma.f plasma.inc
