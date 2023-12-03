DAYS=build/day-1.beam

build/2023.beam: $(DAYS)

build/%.beam: src/%.lfe
	lfe -eval "(cd \"build\") " -eval "(c \"../$(<)\")"
