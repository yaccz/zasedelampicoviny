.PHONY: all
all: clean main check

main:

	rustc main.rs

clean:

	$(RM) main

.PHONY: check
check:

	EXE=$(shell pwd)/main cram ../test/test.t
