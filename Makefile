DEST=build

all: $(DEST)/cek-metalang.html test

$(DEST)/%.html: scribblings/%.scrbl
	scribble --dest $(@D) --dest-name $(@F) $<

.PHONY: test
test:
	raco test cek-metalang/
	raco test --submodule test-ex.lc examples/cek-metalang-ex.rkt > /dev/null
