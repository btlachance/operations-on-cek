PYTHON=pypy
RPYTHON=$(PYTHON) ~/projects/pypy/rpython/bin/rpython
RPYTHONOPTS=
_=$(shell mkdir -p build)
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=cek-metalang/runtime.py
.PHONY: all unittest inttest test clean quicktest
.PRECIOUS: build/interpreter-%/machine.py

all: build/cek-metalang.html test

build/%.html: scribblings/%.scrbl
	scribble --dest build/ --dest-name $(@F) $<

# e.g. build/interpreter-lc/machine.py is produced running examples/lc/spec.rkt
build/interpreter-%/machine.py: examples/%/spec.rkt $(METALANGDEPS)
	mkdir -p $(@D) && cp -R interpreter/* $(@D)
	racket $< --print-interp > $@

build/cek-lc-%-c: examples/lc/spec.rkt build/interpreter-lc/machine.py examples/lc/lc-%.rkt
	raco expand examples/lc/lc-$*.rkt | racket $< --compile-term > build/interpreter-lc/program.py
	(cd build/interpreter-lc/; $(RPYTHON) -Ojit targetcek.py && cp cek-c ../$(@F))

runlc-%: examples/lc/spec.rkt build/interpreter-lc/machine.py examples/lc/lc-%.rkt
	raco expand examples/lc/lc-$*.rkt | racket $< --compile-term > build/interpreter-lc/program.py
	@$(PYTHON) build/interpreter-lc/main.py

unittest:
	raco test cek-metalang/
inttest: $(foreach f, $(shell find . -name "lc-*.rkt"), $(addprefix run,$(notdir $(basename $f))))
test: unittest inttest

clean:
	rm -rf build compiled
