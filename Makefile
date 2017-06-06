DEST=build
_=$(shell mkdir -p $(DEST))
METALANGDEPS=$(wildcard cek-metalang/*.rkt)
RTDEPS=cek-metalang/runtime.py
.PHONY: all unittest inttest test clean fact7

all: $(DEST)/cek-metalang.html test

$(DEST):
	mkdir -p $(DEST)

$(DEST)/%.html: scribblings/%.scrbl
	scribble --dest $(@D) --dest-name $(@F) $<

examples/lc.rkt: $(METALANGDEPS)

$(DEST)/lc-interp.py: examples/lc.rkt $(RTDEPS)
	{ racket $< --print-interp;\
	  cat $(RTDEPS); } > $@

$(DEST)/lc-basictests-linked.py: $(DEST)/lc-interp.py lc-basictests.py
	{ cat $^;\
	  echo 'if __name__ == "__main__":';\
	  echo '  tests()'; } > $@

$(DEST)/lc-fact7-linked.py: $(DEST)/lc-interp.py examples/lc-fact7.txt
	{ cat $<;\
	  racket examples/lc.rkt --compile-term < examples/lc-fact7.txt;\
	  echo 'if __name__ == "__main__":';\
	  echo '  main()'; } > $@

unittest:
	raco test cek-metalang/
inttest: $(DEST)/lc-basictests-linked.py
	python $<
test: unittest inttest
fact7: $(DEST)/lc-fact7-linked.py
	python $<
clean:
	rm -r $(DEST)
