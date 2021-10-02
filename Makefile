.PHONY: all
all: run

.PHONY: download
download: um-spec.txt codex.umz sandmark.umz um.um

um-spec.txt:
	curl -O http://boundvariable.org/um-spec.txt

codex.umz:
	curl -O http://boundvariable.org/codex.umz

sandmark.umz:
	curl -O http://boundvariable.org/sandmark.umz

um.um:
	curl -O http://boundvariable.org/um.um

hello.um: hello.um.lua
	lua $<

run: run.c
	$(CC) -o $@ -Wall -O2 $<

run_threaded: run_threaded.c
	$(CC) -o $@ -Wall -O2 $<

run_threaded_switch: run_threaded_switch.c
	$(CC) -o $@ -Wall -O2 $<

run_directthreaded: run_directthreaded.c
	$(CC) -o $@ -Wall -O2 $<

run_directthreaded_tailcall: run_directthreaded_tailcall.c
	$(CC) -o $@ -Wall -O2 $<

run_verbose: run_verbose.c
	$(CC) -o $@ -Wall -O2 $<

umix.um: run codex.umz extract.lua
	lua -e 'io.write("(\\b.bb)(\\v.vv)06FHPVboundvarHRAk\np\n")' | ./run codex.umz | lua extract.lua
