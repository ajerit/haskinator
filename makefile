HC = ghc

Haskinator : Oraculo
	$(HC) Haskinator.hs

Oraculo : 
	$(HC) Oraculo.hs

all : Haskinator Oraculo

clean :
	rm *.hi *.o Haskinator
