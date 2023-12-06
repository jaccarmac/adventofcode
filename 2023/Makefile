DAYS=build/day-1.beam build/day-2.beam build/day-3.beam build/day-4.beam build/day-5.beam

build/2023.beam: $(DAYS)

build/%.beam: src/%.lfe
	lfe -eval "(cd \"build\") " -eval "(c \"../$(<)\")"
