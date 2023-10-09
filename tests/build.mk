COMAL_TESTS = $(wildcard tests/*.cml)

.PHONY: tests
tests: $(patsubst %.cml,$(OBJDIR)/%.stamp,$(COMAL_TESTS))

$(OBJDIR)/tests/%.stamp: tests/%.good $(OBJDIR)/tests/%.log
	rm -f $@
	diff -a -u $^ > $(patsubst %.stamp,%.diff,$@) || (cat $(patsubst %.stamp,%.diff,$@); exit 1)
	touch $@

$(OBJDIR)/tests/%.log: tests/%.cml $(OBJDIR)/bbctube.bin bin/tubeemu
	@mkdir -p $(dir $@)
	timeout 4s bin/tubeemu -f $(OBJDIR)/bbctube.bin $(notdir $<) > $@

