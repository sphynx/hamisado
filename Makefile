PROG=Kamisado

all: build run

profile:
	ghc -O2 -prof -fforce-recomp -fprof-auto -rtsopts -Wall Kamisado.hs
	./Kamisado +RTS -p
	awk '$$7 > 3' Kamisado.prof

heap:
	./$(PROG) +RTS -hy -i0.05 || true
	hp2pretty $(PROG).hp >$(PROG).svg
	rsvg-view-3 $(PROG).svg

build:
	ghc -O2 -Wall Kamisado.hs

rebuild:
	ghc -fforce-recomp -O2 -Wall Kamisado.hs

run:	build
	sh -c "time ./Kamisado"

test:
	ghc -O2 -Wall Tests.hs
	./Tests

clean:
	rm -v -f *.hi *.o Kamisado Tests
