PROG=Kamisado
PROF_PROG=Prof-Kamisado
PROF_PROG_OPTS=-a alpha -i GameTree -d 9 -p losing

all: build

build:
	ghc -O2 -Wall $(PROG).hs

profile_build:
	ghc -O2 -prof -fforce-recomp -fprof-auto -rtsopts -Wall \
        -o $(PROF_PROG) -osuf prof.o -hisuf prof.hi $(PROG).hs

rebuild:
	ghc -fforce-recomp -O2 -Wall $(PROG).hs

run:	build
	sh -c "time ./Kamisado -d 7"

test:
	ghc -O2 -Wall Tests.hs
	./Tests -t \!Long

test_all:
	ghc -O2 -Wall Tests.hs
	./Tests

profile:	profile_build
	./$(PROF_PROG) $(PROF_PROG_OPTS) +RTS -p
	awk '$$7 > 3' $(PROF_PROG).prof >$(PROF_PROG)-top.prof

backup_profile:
	cp -v $(PROF_PROG)-top.prof previous.prof


heap:	profile_build
	./$(PROF_PROG) $(PROF_PROG_OPTS) +RTS -h${ARGS} -i0.05
	hp2pretty $(PROF_PROG).hp >$(PROF_PROG).svg
	rsvg-view-3 $(PROF_PROG).svg

clean:
	rm -v -f *.hi **/*.hi *.o **/*.o $(PROG) $(PROF_PROG) Tests

clean_data:
	rm -v -f $(PROF_PROG).hp $(PROF_PROG).prof $(PROF_PROG).png $(PROF_PROG).svg
