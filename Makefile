DEST=build

all: $(DEST)/cek-metalang.html

$(DEST)/%.html: scribblings/%.scrbl
	scribble --dest $(@D) --dest-name $(@F) $<
