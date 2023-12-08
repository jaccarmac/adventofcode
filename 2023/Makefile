all: build/2023.beam build/day-1.beam build/day-2.beam build/day-3.beam build/day-4.beam build/day-5.beam build/day-6.beam build/day-7.beam
.PHONY: all

build/%.beam: src/%.lfe
	lfe -eval "(cd \"build\") " -eval "(c \"../$(<)\")"
