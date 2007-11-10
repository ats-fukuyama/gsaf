### $Id$ ###

include ../Makefile.arch

default : all

#all : plasma plasmax demo demo06
all : demo06 demo06i

plasma : plasma.o particle.o random.o
	$(FC) $(FFLAGS) -o plasma plasma.o particle.o random.o $(GDPLIBS) 

plasmax : plasmax.o
	$(FC) $(FFLAGS) -o plasmax plasmax.o $(GDPLIBS) 

demo : demo.o particle.o button.o random.o
	$(FC) $(FFLAGS) -o demo demo.o particle.o button.o random.o $(GDPLIBS) 

demo06 : demo06.o particle06.o button06.o image.o random.o
	$(FC) $(FFLAGS) -o demo06 demo06.o particle06.o button06.o image.o random.o $(GDPLIBS) 

demo07 : demo07.o particle06.o button06.o image.o random.o
	$(FC) $(FFLAGS) -o demo07 demo07.o particle06.o button06.o image.o random.o $(GDPLIBS) 

demo06i : demo06.o particle06.o button06.o imagei.o random.o
	$(FC) $(FFLAGS) -o demo06i demo06.o particle06.o button06.o imagei.o random.o $(GDPLIBS) 

clean :
	rm -f *.o ./*~ ./#*# core*

veryclean : clean
	rm -f plasma plasmax demo

plasma.o: plasma.f particle.f random.f plasma.inc
plasmax.o: plasmax.f plasma.inc
demo.o: demo.f plasma.inc button.inc
particle.o: particle.f plasma.inc 
button.o: button.f button.inc
demo06.o: demo06.f plasma.inc button.inc
particle06.o: particle06.f plasma.inc 
button06.o: button06.f button.inc
image.o: image.f plasma.inc 
imagei.o: imagei.f plasma.inc 
random.o: random.f
