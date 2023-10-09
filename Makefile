LLVM ?= /opt/bin/
CC = cc

OBJDIR = .obj

all: bin/bbctube.ssd

bin/bbctube.ssd: $(OBJDIR)/bbctube.bin $(OBJDIR)/mkdfs
	@mkdir -p $(dir $@)
	$(OBJDIR)/mkdfs -O $@ \
		-N Comal-65 \
		-f $(OBJDIR)/bbctube.bin -n \!boot -l 0x400 -e 0x400 -B 2

$(OBJDIR)/%: tools/%.c
	@mkdir -p $(dir $@)
	$(CC) -o $@ $<

$(OBJDIR)/%.bin: $(OBJDIR)/src/%.o src/%.ld
	@mkdir -p $(dir $@)
	$(LD65)ld.lld \
		-Map $(OBJDIR)/$*.map \
		-T src/$*.ld \
		-o $@ \
		$<

$(OBJDIR)/%.o: %.S
	@mkdir -p $(dir $@)
	$(LLVM)mos-cpm65-clang -g -c -o $@ $<

clean:
	rm -rf $(OBJDIR) bin
