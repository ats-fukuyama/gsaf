
### $Id$ ###
#
# Make for gsaf : V3.80
#

default : libs tests

libs :
	(cd lib; make)

tests : libs
	(cd test; make)

install : libs
	(cd lib; make install)
	(cd bin; make install)

clean :
	rm -f ./*~
	(cd lib; make clean)
	(cd bin; make clean)
	(cd test; make clean)

veryclean : clean
	(cd lib; make veryclean)
	(cd bin; make veryclean)
	(cd test; make veryclean)

distclean : veryclean
	rm -f Makefile.arch
