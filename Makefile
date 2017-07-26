PYTHON=pypy
RPYTHON=$(PYTHON) ~/projects/pypy/rpython/bin/rpython
RPYTHONOPTS=
_=$(shell mkdir -p build)
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=cek-metalang/runtime.py
.PHONY: all unittest inttest test clean quicktest

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

build/lc-%-c: examples/lc/targetlc.py build/lc-%-linked.py
	cp examples/lc/targetlc.py build/lc-$*.py
	mv build/lc-$*-linked.py build/lc.py
	(cd build/; $(RPYTHON) $(RPYTHONOPTS) lc-$*.py)
build/lc-%-jit-c: examples/lc/targetlc.py build/lc-%-linked.py
	cp examples/lc/targetlc.py build/lc-$*-jit.py
	mv build/lc-$*-linked.py build/lc.py
	(cd build/; $(RPYTHON) -Ojit $(RPYTHONOPTS) lc-$*-jit.py)

build/lc-%-linked.py: build/lc-interp.py examples/lc/lc-%.txt
	{ set -e;\
	  cat $<;\
	  raco expand examples/lc/lc-$*.txt | racket examples/lc/lc.rkt --compile-term;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > $@
runlc-%: build/lc-%-linked.py
	$(PYTHON) $<

unittest:
	raco test cek-metalang/
inttest: build/lc-basictests-linked.py
	$(PYTHON) $<
test: unittest inttest

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
