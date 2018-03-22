PYTHON=pypy
RPYTHON=$(PYTHON) ~/projects/pypy/rpython/bin/rpython
RPYTHONOPTS=
_=$(shell mkdir -p build)
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=$(shell find interpreter -type f -name '*.py')
.PHONY: all unittest inttest test clean quicktest
.PRECIOUS: build/interpreter-%/main.py

all: build/cek-metalang.html test

build/%.html: scribblings/%.scrbl
	scribble --dest build/ --dest-name $(@F) $<


build/interpreter-%/main.py : examples/%/spec.rkt $(RTDEPS) examples/%/interpreter/*.py $(METALANGDEPS)
	mkdir -p $(@D)
	cp -R interpreter/* $(@D)
	cp -R examples/$*/interpreter/* $(@D)
	racket $< --print-interp > $(@D)/machine.py
	racket $< --print-parser > $(@D)/parser.py

build/interpreter-%/cek-c : build/interpreter-%/main.py
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
