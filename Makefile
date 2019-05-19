all: Main.hs Algebra/Point.hs Algebra/Group.hs
	ghc -O2 --make Main.hs

clean:
	rm *.o *.hi Algebra/*.o Algebra/*.hi