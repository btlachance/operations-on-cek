PYTHON=pypy
RPYTHON=$(PYTHON) ~/projects/pypy/rpython/bin/rpython
RPYTHONOPTS=
_=$(shell mkdir -p build)
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=cek-metalang/runtime.py
.PHONY: all unittest inttest test clean fact7 fib quicktest

all: build/cek-metalang.html test

build/%.html: scribblings/%.scrbl
	scribble --dest build/ --dest-name $(@F) $<

build/lc-interp.py: examples/lc/lc.rkt $(METALANGDEPS) $(RTDEPS)
	{ set -e;\
	  racket $< --print-interp;\
	  cat $(RTDEPS); } > $@

build/lc-basictests-linked.py: build/lc-interp.py examples/lc/lc-basictests.py
	{ set -e;\
	  cat $^;\
	  echo 'if __name__ == "__main__":';\
	  echo '  tests()'; } > $@

build/lc-fact7-linked.py: build/lc-interp.py examples/lc/lc-fact7.txt
	{ set -e;\
	  cat $<;\
	  raco expand examples/lc/lc-fact7.txt | racket examples/lc/lc.rkt --compile-term;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > $@
build/lc-fib-linked.py: build/lc-interp.py examples/lc/lc-fib.txt
	{ set -e;\
	  cat $<;\
	  raco expand examples/lc/lc-fib.txt | racket examples/lc/lc.rkt --compile-term;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > $@

build/fact-c: examples/lc/targetlc.py build/lc-fact7-linked.py
	cp examples/lc/targetlc.py build/fact.py
	mv build/lc-fact7-linked.py build/lc.py
	(cd build/; $(RPYTHON) $(RPYTHONOPTS) fact.py)
build/fib-jit-c: examples/lc/targetlc.py build/lc-fib-linked.py
	cp examples/lc/targetlc.py build/fib-jit.py
	mv build/lc-fib-linked.py build/lc.py
	(cd build/; $(RPYTHON) -Ojit $(RPYTHONOPTS) fib-jit.py)
build/fib-nojit-c: examples/lc/targetlc.py build/lc-fib-linked.py
	cp examples/lc/targetlc.py build/fib-nojit.py
	mv build/lc-fib-linked.py build/lc.py
	(cd build/; $(RPYTHON) $(RPYTHONOPTS) fib-nojit.py)

unittest:
	raco test cek-metalang/
inttest: build/lc-basictests-linked.py
	$(PYTHON) $<
test: unittest inttest
fact7: build/lc-fact7-linked.py
	$(PYTHON) $<
fib: build/lc-fib-linked.py
	$(PYTHON) $<
quicktest: build/lc-interp.py
	TMP=$$(mktemp build/runtest-XXXX).py;\
	{ set -e;\
	  cat $<;\
	  racket examples/lc/lc.rkt --compile-term;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > "$$TMP";\
	$(PYTHON) $$TMP
clean:
	rm -rf build compiled
