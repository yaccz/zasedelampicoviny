all:

	nasm -g -f elf64 -o main.o main.asm
	ld -o main main.o

clean:

	rm main main.o

check:

	./main
