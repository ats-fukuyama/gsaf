### $Id$ ###

include ../Makefile.arch

default : bin

bin : gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gstotgif.slave

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

gsprint : gsprint.tp
	sed "s@BINPATH@$(BINPATH)@" gsprint.tp | \
	sed "s@LP@$(LP)@" > gsprint
	chmod 755 gsprint

$(FC)g : f77g.tp
	sed "s@FC@$(FC)@" f77g.tp | \
	sed "s@GSLIBS@$(GDPLIBS)@" > $(FC)g
	chmod 755 $(FC)g

$(FC)gdum : f77g.tp
	sed "s@FC@$(FC)@" f77g.tp | \
	sed "s@GSLIBS@$(GDUMLIBS)@" > $(FC)gdum
	chmod 755 $(FC)gdum

install : gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gstotgif.slave gsprint gsdump \
	$(FC)g $(FC)gdum
	chmod 755 gsdump
	-rm $(BINPATH)/gsfile $(BINPATH)/gstoeps $(BINPATH)/gstops $(BINPATH)/gstotgif
	cp gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gstotgif.slave gsprint gsdump \
	   $(FC)g $(FC)gdum $(BINPATH)
	ln $(BINPATH)/gsview $(BINPATH)/gsfile
	ln $(BINPATH)/gsview $(BINPATH)/gstoeps
	ln $(BINPATH)/gsview $(BINPATH)/gstops
	ln $(BINPATH)/gsview $(BINPATH)/gstotgif

clean :
	rm -f *.o ./*~ ./#*# core*
	-rm -f $(FC)g $(FC)gdum

veryclean : clean
	rm -f *$(SL) *test gsview *.slave gsprint *g *gdum gsdata*
