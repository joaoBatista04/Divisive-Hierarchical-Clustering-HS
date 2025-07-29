clean:
	rm -rf *.hi *.o *.csv Main
all:
	ghc --make Main.hs