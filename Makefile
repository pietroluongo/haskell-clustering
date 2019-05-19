# Standard build procedure
all: Main.hs Algebra/Point.hs Algebra/Group.hs
	ghc -O3 --make Main.hs

# Disables optimizations, program runs slower
all-no-optimizations: Main.hs Algebra/Point.hs Algebra/Group.hs
	ghc --make Main.hs

# Removes compiled binaries and related files
clean:
	@ rm *.o *.hi Algebra/*.o Algebra/*.hi Main