.PHONY: all
all: clean main check

main:

	nasm -g -f elf64 -o main.o main.asm
	ld -o main main.o

clean:

	$(RM) main main.o

.PHONY: check
check:

	EXE=$(shell pwd)/main cram ../test/test.t
