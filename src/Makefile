### $Id$ ###
#
# Make for gsaf : V3.80
#

default : libs install

libs :
	(cd lib; make)

install : libs
	(cd lib; make install)
	(cd bin; make install)

tests : libs
	(cd test; make)

clean :
	rm -f *.o ./*~ ./#*# core*
	(cd lib; make clean)
	(cd bin; make clean)
	(cd test; make clean)

veryclean : clean
	(cd lib; make veryclean)
	(cd bin; make veryclean)
	(cd test; make veryclean)

distclean : veryclean
	rm -f Makefile.arch
