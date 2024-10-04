all: app/ParCPP.hs
	cabal build		

app/ParCPP.hs: gramatica/CPP.cf
	cd gramatica; \
	bnfc CPP.cf; \
	alex -g LexCPP.x; \
	happy -gac ParCPP.y; \
	mv *.hs ../app

clean:
	rm -rf *.log *.aux *.hi *.o *.dvi *~


