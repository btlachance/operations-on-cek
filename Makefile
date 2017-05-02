DEST=build

all: $(DEST)/cek-metalang.html test

$(DEST)/%.html: scribblings/%.scrbl
	scribble --dest $(@D) --dest-name $(@F) $<

$(DEST)/lam-example-tests-linked.py: cek-metalang/runtime.py lam-example-tests.py\
	                                     examples/cek-metalang-ex.rkt
	{ raco test -q --submodule test-ex.lc examples/cek-metalang-ex.rkt;\
	    cat cek-metalang/runtime.py\
	        lam-example-tests.py;\
	    echo 'tests()'; } > $@

.PHONY: test unittest inttest
unittest:
	raco test cek-metalang/
inttest: $(DEST)/lam-example-tests-linked.py
	python $<
test: unittest inttest
