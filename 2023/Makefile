DAYS=1 2 3 4 5 6 7 8

all: build/2023.beam $(patsubst %,build/day-%.beam,$(DAYS))
.PHONY: all

build/%.beam: src/%.lfe
	lfe -eval "(cd \"build\") " -eval "(c \"../$(<)\")"
