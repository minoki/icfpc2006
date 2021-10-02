.PHONY: all clean
all: run_jit

run_jit: run_jit.c
	$(CC) -O2 -Wall -g -o $@ $<

clean:
	-rm run_jit
