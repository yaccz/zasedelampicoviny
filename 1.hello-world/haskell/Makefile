.PHONY: all
all: clean main check

main:

	ghc main.hs

clean:

	$(RM) main

.PHONY: check
check:

	EXE=$(shell pwd)/main cram ../test/test.t
