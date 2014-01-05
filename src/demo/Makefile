### $Id$ ###

include ../Makefile.arch

default : all

#all : plasma plasmax demo demo06
#all : demo06 demo06i
all : demo13

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

demo08 : demo08.o particle06.o button08.o image.o random.o
	$(FC) $(FFLAGS) -o demo08 demo08.o particle06.o button08.o image.o random.o $(GDPLIBS) 

demo13 : demo13.o particle06.o button08.o image.o random.o
	$(FC) $(FFLAGS) -o demo13 demo13.o particle06.o button08.o image.o random.o $(GDPLIBS) 

pp08 : pp08.o particle-pp08.o button-pp08.o image.o random.o
	$(FC) $(FFLAGS) -o pp08 pp08.o particle-pp08.o button-pp08.o image.o random.o $(GDPLIBS) 

clean :
	rm -f *.o ./*~ ./#*# core*

veryclean : clean
	rm -f plasma plasmax demo demo06 demo06i demo07 demo08 pp08

plasma.o: plasma.f particle.f random.f plasma.inc
plasmax.o: plasmax.f plasma.inc
demo.o: demo.f plasma.inc button.inc
particle.o: particle.f plasma.inc 
button.o: button.f button.inc
demo06.o: demo06.f plasma.inc button.inc
demo07.o: demo07.f plasma.inc button.inc
demo08.o: demo08.f plasma.inc button.inc
demo13.o: demo13.f plasma.inc button.inc
particle06.o: particle06.f plasma.inc 
button06.o: button06.f button.inc
button08.o: button08.f button.inc
image.o: image.f plasma.inc 
imagei.o: imagei.f plasma.inc 
random.o: random.f
