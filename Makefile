PYTHON=pypy
RPYTHON=$(PYTHON) ~/projects/pypy/rpython/bin/rpython
RPYTHONOPTS=
_=$(shell mkdir -p build)
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=$(wildcard interpreter/*.py)
.PHONY: all unittest inttest test clean quicktest
.PRECIOUS: build/interpreter-%/machine.py

all: build/cek-metalang.html test

build/%.html: scribblings/%.scrbl
	scribble --dest build/ --dest-name $(@F) $<

build/interpreter-%/cek-c : examples/%/spec.rkt $(RTDEPS)  $(METALANGDEPS)
	mkdir -p $(@D)
	cp -R interpreter/* $(@D)
	racket $< --print-interp > $(@D)/machine.py
	racket $< --print-parser > $(@D)/parser.py
	$(RPYTHON) -Ojit $(@D)/targetcek.py && mv cek-c $@

runlc-%: build/interpreter-lc/cek-c
	raco expand examples/lc/lc-$*.rkt |\
		racket examples/lc/lc.rkt --compile-term |\
		$<

unittest:
	raco test cek-metalang/
inttest: $(foreach f, $(shell find . -name "lc-*.rkt"), $(addprefix run,$(notdir $(basename $f))))
test: unittest inttest

clean:
	rm -rf build compiled
