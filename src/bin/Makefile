### $Id$ ###

include ../Makefile.arch

default : bin

bin : gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gstotgif.slave gstosvg.slave

gsview : gsview.c
	$(CC) $(CFLAGS) -o gsview gsview.c

gslave.o : gslave.f
	$(FC) $(FFLAGS) -c gslave.f

gsview.slave : gslave.o $(GDPLIBSX)
	$(FC) $(FFLAGS) -o gsview.slave gslave.o $(GDPLIBS)

gsfile.slave : gslave.o $(GPPLIBSX)
	$(FC) $(FFLAGS) -o gsfile.slave gslave.o $(GPPLIBS)

gstoeps.slave : gslave.o $(GPSLIBSX)
	$(FC) $(FFLAGS) -o gstoeps.slave gslave.o $(GPSLIBS)

gstops.slave : gslave.o $(GPSLIBSX)
	$(FC) $(FFLAGS) -o gstops.slave gslave.o $(GPSLIBS)

gstotgif.slave : gslave.o $(GTGIFLIBSX)
	$(FC) $(FFLAGS) -o gstotgif.slave gslave.o $(GTGIFLIBS)

gstosvg.slave : gslave.o $(GSVGLIBSX)
	$(FC) $(FFLAGS) -o gstosvg.slave gslave.o $(GSVGLIBS)

gsprint : gsprint.tp
	sed "s@BINPATH@$(BINPATH)@" gsprint.tp | \
	sed "s@LP@$(LP)@" > gsprint
	chmod 755 gsprint

$(FC)g : f77g.tp
	sed "s@FC@$(FC)@" f77g.tp | \
	sed "s@GSLIBS@$(GDPLIBS)@" > $(FC)g
	chmod 755 $(FC)g

$(FC)g90 : f90g.tp
	sed "s@FC@$(FC)@" f90g.tp | \
	sed "s@GSLIBS@$(GDPLIBS)@" > $(FC)g90
	chmod 755 $(FC)g90

$(FC)gdum : f77g.tp
	sed "s@FC@$(FC)@" f77g.tp | \
	sed "s@GSLIBS@$(GDUMLIBS)@" > $(FC)gdum
	chmod 755 $(FC)gdum

$(FC)gdum90 : f90g.tp
	sed "s@FC@$(FC)@" f90g.tp | \
	sed "s@GSLIBS@$(GDUMLIBS)@" > $(FC)gdum90
	chmod 755 $(FC)gdum90

install : gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gstotgif.slave gstosvg.slave gsprint gsdump $(FC)g $(FC)gdum $(FC)g90 $(FC)gdum90
	-rm $(BINPATH)/gsfile $(BINPATH)/gstoeps $(BINPATH)/gstops $(BINPATH)/gstotgif $(BINPATH)/gstosvg
	-rm $(BINPATH)/gsview $(BINPATH)/gsview.slave $(BINPATH)/gsfile.slave
	-rm $(BINPATH)/gstoeps.slave $(BINPATH)/gstops.slave
	-rm $(BINPATH)/gstotgif.slave $(BINPATH)/gstosvg.slave
	-rm $(BINPATH)/gsprint $(BINPATH)/gsdump
	-rm $(BINPATH)/$(FC)g $(BINPATH)/$(FC)gdum
	cp -f gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gstotgif.slave gstosvg.slave gsprint gsdump \
	   $(FC)g $(FC)gdum $(BINPATH)
	ln $(BINPATH)/gsview $(BINPATH)/gsfile
	ln $(BINPATH)/gsview $(BINPATH)/gstoeps
	ln $(BINPATH)/gsview $(BINPATH)/gstops
	ln $(BINPATH)/gsview $(BINPATH)/gstotgif
	ln $(BINPATH)/gsview $(BINPATH)/gstosvg

clean :
	rm -f *.o ./*~ ./#*# core*
	-rm -f gsview $(FC)g $(FC)gdum *.slave

veryclean : clean
	rm -f *$(SL) *test gsprint *g *gdum gsdata*
