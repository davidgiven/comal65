COMAL_TESTS = $(wildcard tests/*.cml)
ASM_TESTS = $(wildcard tests/*.s)

.PHONY: tests
tests: \
	$(patsubst %.cml,$(OBJDIR)/%.stamp,$(COMAL_TESTS)) \
	$(patsubst %.s,$(OBJDIR)/%.stamp,$(ASM_TESTS))

$(OBJDIR)/tests/%.stamp: tests/%.good $(OBJDIR)/tests/%.log
	rm -f $@
	diff -a -u $^ > $(patsubst %.stamp,%.diff,$@) || (cat $(patsubst %.stamp,%.diff,$@); exit 1)
	touch $@

$(OBJDIR)/tests/%.bin: $(OBJDIR)/tests/%.o src/bbctube.ld
	@mkdir -p $(dir $@)
	$(LLVM)ld.lld \
		-Map $(OBJDIR)/$*.map \
		-T src/bbctube.ld \
		-o $@ \
		$(filter %.o, $^)

$(OBJDIR)/tests/%.log: $(OBJDIR)/tests/%.bin bin/tubeemu
	@mkdir -p $(dir $@)
	timeout 4s bin/tubeemu $< bin/tubeemu > $@

$(OBJDIR)/tests/%.log: tests/%.cml $(OBJDIR)/bbctube.bin bin/tubeemu
	@mkdir -p $(dir $@)
	timeout 4s bin/tubeemu $(OBJDIR)/bbctube.bin $< > $@

