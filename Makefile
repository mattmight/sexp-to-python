INSTALLDIR=/usr/local/bin

sxpy: sxpy.rkt
	raco exe sxpy

install:
	sudo cp sxpy $(INSTALLDIR)

clean:
	rm -fv sxpy
