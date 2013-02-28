PROG=Kamisado

all: build run

profile:
	ghc -O2 -prof -fforce-recomp -fprof-auto -rtsopts Kamisado.hs
	./Kamisado +RTS -p
	awk "$7 > 3" Kamisado.prof

heap:
	./$(PROG) +RTS -hT -i0.05 || true
	hp2pretty $(PROG).hp >$(PROG).svg
	rsvg $(PROG).svg $(PROG).png
	feh $(PROG).png

build:
	ghc -O2 Kamisado.hs

rebuild:
	ghc -fforce-recomp -O2 Kamisado.hs

run:
	./Kamisado

clean:
	rm -v -f *.hi *.o Kamisado Dia
