COMAL_TESTS = $(wildcard tests/*.cml)
ASM_TESTS = $(wildcard tests/*.good)

.PHONY: tests
tests: \
	$(patsubst %.cml,$(OBJDIR)/%.stamp,$(COMAL_TESTS)) \
	$(patsubst %.good,$(OBJDIR)/%.stamp,$(ASM_TESTS))

FOBJS = \
	$(OBJDIR)/third_party/vm02/ops.o \
	$(OBJDIR)/src/_vars.o \
	$(OBJDIR)/src/_utils.o \
	$(OBJDIR)/tests/_futils.o

$(OBJDIR)/tests/f2i.bin: $(FOBJS)
$(OBJDIR)/tests/ffromint.bin: $(FOBJS)
$(OBJDIR)/tests/fpackunpack.bin: $(FOBJS)
$(OBJDIR)/tests/fnormalise.bin: $(FOBJS)

$(patsubst %.good,$(OBJDIR)/%.o,$(ASM_TESTS)) $(OBJDIR)/tests/_futils.o: \
	tests/_futils.inc

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
	timeout 4s bin/tubeemu $< > $@

$(OBJDIR)/tests/%.log: tests/%.cml $(OBJDIR)/bbctube.bin bin/tubeemu
	@mkdir -p $(dir $@)
	timeout 4s bin/tubeemu $(OBJDIR)/bbctube.bin $< > $@

