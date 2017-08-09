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

build/interpreter-%: examples/%/spec.rkt $(wildcard interpreter/*.py) $(METALANGDEPS)
	mkdir -p $@
	cp -R interpreter/* $@
	racket $< --print-interp > $@/machine.py
	racket $< --print-parser > $@/parser.py

build/cek-lc-c: build/interpreter-lc/
	(cd build/interpreter-lc/; $(RPYTHON) -Ojit targetcek.py && cp cek-c ../$(@F))

build/cek-lc-nojit-c: build/interpreter-lc/
	(cd build/interpreter-lc/; $(RPYTHON) targetcek.py && cp cek-c ../$(@F))

runlc-%: build/cek-lc-c
	raco expand examples/lc/lc-$*.rkt |\
		racket examples/lc/lc.rkt --compile-term |\
		$<

unittest:
	raco test cek-metalang/
inttest: $(foreach f, $(shell find . -name "lc-*.rkt"), $(addprefix run,$(notdir $(basename $f))))
test: unittest inttest

clean:
	rm -rf build compiled
