### $Id$ ###

include ../Makefile.arch

default : bin

bin : gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave

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

install : gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gsprint gsdump \
	$(FC)g $(FC)gdum
	chmod 755 gsdump
	-rm $(BINPATH)/gsfile $(BINPATH)/gstoeps $(BINPATH)/gstops
	cp gsview gsview.slave gsfile.slave gstoeps.slave gstops.slave gsprint gsdump \
	   $(FC)g $(FC)gdum $(BINPATH)
	ln $(BINPATH)/gsview $(BINPATH)/gsfile
	ln $(BINPATH)/gsview $(BINPATH)/gstoeps
	ln $(BINPATH)/gsview $(BINPATH)/gstops

clean :
	rm -f *.o ./*~ ./#*# core*
	-rm -f $(FC)g $(FC)gdum

veryclean : clean
	rm -f *$(SL) *test gsview *.slave gsprint $(FC)g $(FC)gdum gsdata*
