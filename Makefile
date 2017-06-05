DEST=build

all: $(DEST)/cek-metalang.html test

$(DEST)/%.html: scribblings/%.scrbl
	scribble --dest $(@D) --dest-name $(@F) $<

$(DEST)/lam-example-tests-linked.py: cek-metalang/runtime.py $(wildcard cek-metalang/*.rkt)\
	                                     examples/cek-metalang-ex.rkt lam-example-tests.py
	{ raco test -q --submodule test-ex.lc examples/cek-metalang-ex.rkt;\
	    cat cek-metalang/runtime.py\
	        lam-example-tests.py;\
	    echo 'tests()'; } > $@

$(DEST)/lam-example-main-linked.py: cek-metalang/runtime.py $(wildcard cek-metalang/*.rkt)\
	                                     examples/cek-metalang-ex.rkt lam-example-tests.py
	{ raco test -q --submodule test-ex.lc examples/cek-metalang-ex.rkt;\
	    cat cek-metalang/runtime.py\
	        lam-example-tests.py;\
	    echo 'if __name__ == "__main__":';\
	    echo '  main()'; } > $@

.PHONY: test unittest inttest
unittest:
	raco test cek-metalang/
inttest: $(DEST)/lam-example-tests-linked.py
	python $<
test: unittest inttest
main: $(DEST)/lam-example-main-linked.py
	python $<
