LLVM ?= /opt/bin/
CC = cc
CLANGFLAGS =

OBJDIR = .obj

COMMON = \
	$(OBJDIR)/src/_interpreter.o

FILES = \
	examples/hilo.cml

all: bin/bbctube.ssd

bin/bbctube.ssd: $(OBJDIR)/bbctube.bin $(OBJDIR)/mkdfs
	@mkdir -p $(dir $@)
	$(OBJDIR)/mkdfs -O $@ \
		-N Comal-65 \
		-f $(OBJDIR)/bbctube.bin -n \!boot -l 0x400 -e 0x400 -B 2 \
		$(patsubst %, -f %, $(FILES))

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
	$(LLVM)mos-cpm65-clang -g $(CLANGFLAGS) -c -o $@ $<

$(OBJDIR)/src/bbctube.o: CLANGFLAGS += -mcpu=mos65c02

clean:
	rm -rf $(OBJDIR) bin
