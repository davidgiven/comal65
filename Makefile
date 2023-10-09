LLVM ?= /opt/bin/
CC = cc
CLANGFLAGS =

OBJDIR = .obj

COMMON = \
	$(OBJDIR)/src/_interpreter.o

FILES = \
	examples/hilo.cml

all: bin/tubeemu bin/bbctube.ssd

bin/tubeemu: $(OBJDIR)/tools/tubeemu/bbctube
	@mkdir -p $(dir $@)
	cp $< $@

bin/bbctube.ssd: $(OBJDIR)/bbctube.bin $(OBJDIR)/tools/mkdfs
	@mkdir -p $(dir $@)
	$(OBJDIR)/tools/mkdfs -O $@ \
		-N Comal-65 \
		-f $(OBJDIR)/bbctube.bin -n \!boot -l 0x400 -e 0x400 -B 2 \
		$(patsubst %, -f %, $(FILES))

$(OBJDIR)/%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) -c -o $@ $< -I.

$(OBJDIR)/%: $(OBJDIR)/%.o
	@mkdir -p $(dir $@)
	$(CC) -o $@ $^

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

$(OBJDIR)/tools/tubeemu/bbctube: $(OBJDIR)/third_party/lib6502/lib6502.o

include tests/build.mk

clean:
	rm -rf $(OBJDIR) bin
