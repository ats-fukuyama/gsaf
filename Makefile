default :
	@echo "*** make in the directory src, not here ***"
	@echo "   cd src"
	@echo "   cp ../arch/????/Makefile ."
	@echo "   make"

clean:
	(cd arch; rm -f */*~)
	(cd src; make clean)
	(cd doc; make clean)
	-rm -f ./*~

veryclean:
	(cd arch; rm -f */*~)
	(cd src; make veryclean)
	(cd doc; make veryclean)
	-rm -f ./*~

new: veryclean
	-mkdir ../gsafnew
	cp -Rf src ../gsafnew
	-rm ../gsafnew/src/igl3.o
	-rm ../gsafnew/src/Makefile
	cp -Rf doc ../gsafnew
	cp -Rf arch ../gsafnew
	cp -Rf contrib ../gsafnew
	cp Makefile ../gsafnew
	cp README* ../gsafnew
	cp HISTORY ../gsafnew
	cp COPYING ../gsafnew
