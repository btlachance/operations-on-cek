PYTHON=pypy
RPYTHON=$(PYTHON) ~/projects/pypy/rpython/bin/rpython
RPYTHONOPTS=
_=$(shell mkdir -p build)
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=cek-metalang/runtime.py
.PHONY: all unittest inttest test clean quicktest
.PRECIOUS: build/lc-%-linked.py

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

build/lc-%-linked.py: build/lc-interp.py examples/lc/lc-%.rkt
	{ set -e;\
	  cat $<;\
	  raco expand examples/lc/lc-$*.rkt | racket examples/lc/lc.rkt --compile-term;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > $@
runlc-%: build/lc-%-linked.py
	@$(PYTHON) $<

unittest:
	raco test cek-metalang/
inttest: $(foreach f, $(shell find . -name "lc-*.rkt"), $(addprefix run,$(notdir $(basename $f))))
test: unittest inttest

quicktest: build/lc-interp.py
	TMP=$$(mktemp build/runtest-XXXX).py;\
	{ set -e;\
	  cat $<;\
	  racket examples/lc/lc.rkt --compile-term;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > "$$TMP";\
	$(PYTHON) $$TMP
quicktest-jit: build/lc-interp.py
	cp examples/lc/targetlc.py build/lc-quicktest-jit.py
	TMP=$$(mktemp build/runtest-XXXX).py;\
	{ set -e;\
	  cat $<;\
	  racket examples/lc/lc.rkt --compile-term;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > "$$TMP";\
	mv $$TMP build/lc.py
	(cd build/; $(RPYTHON) -Ojit $(RPYTHONOPTS) lc-quicktest-jit.py)
clean:
	rm -rf build compiled
