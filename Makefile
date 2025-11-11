CC = gcc
CFLAGS = -g -Wall -Wextra -Werror -fsanitize=address -fsanitize=undefined
# -fsanitize=address -fsanitize=undefined

helper: helper.c helper.h
	$(CC) $(CFLAGS) -c tahelperbles.c

cpu: cpu.c cpu.h
	$(CC) $(CFLAGS) -c cpu.c

assembler: assembler.c assembler.h
	$(CC) $(CFLAGS) -c assembler.c

tables: tables.c tables.h
	$(CC) $(CFLAGS) -c tables.c

opcodes: opcodes.c opcodes.h
	$(CC) $(CFLAGS) -c opcodes.c

compile: compile.c cpu.o assembler.o tables.o helper.o opcodes.o
	$(CC) $(CFLAGS) -o compile compile.c cpu.o assembler.o tables.o helper.o opcodes.o

load: load.c cpu.o assembler.o tables.o helper.o opcodes.o
	$(CC) $(CFLAGS) -o load load.c cpu.o assembler.o tables.o helper.o opcodes.o

run_compile: compile
	./compile

run_load: load
	./load

leaks_compile: compile
	leaks --atExit -- ./compile

leaks_load: load
	leaks --atExit -- ./load

clean:
	rm -f *.o
	rm -f compile load
	rm -rf *.dSYM
