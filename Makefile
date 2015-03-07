INSTALLDIR=/usr/local/bin/

sxpy: sxpy.rkt
	raco exe sxpy.rkt

install: sxpy
	sudo cp sxpy $(INSTALLDIR)

clean:
	rm -fv sxpy
