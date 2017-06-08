PYTHON=pypy
RPYTHON=python ~/projects/pypy/rpython/bin/rpython
_=$(shell mkdir -p build)
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=cek-metalang/runtime.py
.PHONY: all unittest inttest test clean fact7

all: build/cek-metalang.html test

build/%.html: scribblings/%.scrbl
	scribble --dest build/ --dest-name $(@F) $<

examples/lc/lc.rkt: $(METALANGDEPS)

build/lc-interp.py: examples/lc/lc.rkt $(RTDEPS)
	{ racket $< --print-interp;\
	  cat $(RTDEPS); } > $@

build/lc-basictests-linked.py: build/lc-interp.py examples/lc/lc-basictests.py
	{ cat $^;\
	  echo 'if __name__ == "__main__":';\
	  echo '  tests()'; } > $@

build/lc-fact7-linked.py: build/lc-interp.py examples/lc/lc-fact7.txt
	{ cat $<;\
	  racket examples/lc/lc.rkt --compile-term < examples/lc/lc-fact7.txt;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > $@

build/targetlc-c: examples/lc/targetlc.py build/lc-fact7-linked.py
	cp examples/lc/targetlc.py build/targetlc.py
	mv build/lc-fact7-linked.py build/lc.py
	(cd build/; $(RPYTHON) targetlc.py)

unittest:
	raco test cek-metalang/
inttest: build/lc-basictests-linked.py
	$(PYTHON) $<
test: unittest inttest
fact7: build/lc-fact7-linked.py
	$(PYTHON) $<
clean:
	rm -rf build compiled
