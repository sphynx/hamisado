PROG=Hamisado
PROF_PROG=Prof-Hamisado
PROF_PROG_OPTS=-a negascout -i my -d 10 -p losing
BENCH_OPTS=-a negascout -i my -p losing

all: build

build:
	ghc -O2 -Wall $(PROG).hs

profile_build:
	ghc -O2 -prof -fforce-recomp -fprof-auto -rtsopts -Wall \
        -o $(PROF_PROG) -osuf prof.o -hisuf prof.hi $(PROG).hs

rebuild:
	ghc -fforce-recomp -O2 -Wall $(PROG).hs

run:	build
	sh -c "time ./$PROG -d 7"

test:
	ghc -O2 -Wall Tests.hs
	./Tests -t \!Long

test_all:
	ghc -O2 -Wall Tests.hs
	./Tests

hpc:	clean_data
	ghc -O2 -Wall -fhpc Tests.hs
	./Tests -t \!Long
	hpc report Tests
	hpc markup Tests --destdir=hpc

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
	rm -v -f $(PROG) $(PROF_PROG) Tests
	find . \( -name '*.hi' -o -name '*.o' -o -name '*.tix' \) -delete

clean_data:	clean
	rm -v -f $(PROF_PROG).hp $(PROF_PROG).prof $(PROF_PROG).png $(PROF_PROG).svg *.html *.zip
	rm -r -f hpc
	rm -r -f .hpc

zip:
	git archive -o hamisado.zip HEAD

etags:
	rm -f TAGS
	fast-tags -e -v -R .

benchmark:
	sh -c "time $(PROG) $(BENCH_OPTS) -d 7"
	echo "Baseline time: 0.61s"
	sh -c "time $(PROG) $(BENCH_OPTS) -d 9"
	echo "Baseline time: 2.73s"
