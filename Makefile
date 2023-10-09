LLVM ?= /opt/bin/
CC = cc

OBJDIR = .obj

COMMON = \
	$(OBJDIR)/src/_interpreter.o

all: bin/bbctube.ssd

bin/bbctube.ssd: $(OBJDIR)/bbctube.bin $(OBJDIR)/mkdfs
	@mkdir -p $(dir $@)
	$(OBJDIR)/mkdfs -O $@ \
		-N Comal-65 \
		-f $(OBJDIR)/bbctube.bin -n \!boot -l 0x400 -e 0x400 -B 2

$(OBJDIR)/%: tools/%.c
	@mkdir -p $(dir $@)
	$(CC) -o $@ $<

$(OBJDIR)/%.bin: $(OBJDIR)/src/%.o $(COMMON) src/%.ld 
	@mkdir -p $(dir $@)
	$(LD65)ld.lld \
		-Map $(OBJDIR)/$*.map \
		-T src/$*.ld \
		-o $@ \
		$(filter %.o, $^)

$(OBJDIR)/%.o: %.s
	@mkdir -p $(dir $@)
	$(LLVM)mos-cpm65-clang -g -c -o $@ $<

clean:
	rm -rf $(OBJDIR) bin
