LLVM ?= /opt/bin/
CC = cc
CXX = c++
CFLAGS = -Og -g
CXXFLAGS = -Og -g
LDFLAGS =
CLANGFLAGS =

OBJDIR = .obj

COMMON = \
	$(OBJDIR)/src/_interpreter.o \
	$(OBJDIR)/src/_buffer.o \
	$(OBJDIR)/src/_maths.o \
	$(OBJDIR)/src/_parser.o \

FILES = \
	examples/hilo.cml

all: tests bin/tubeemu bin/bbctube.ssd

bin/tubeemu: $(OBJDIR)/tools/tubeemu/tubeemu
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
	$(CC) $(CFLAGS) -c -o $@ $< -I.

$(OBJDIR)/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c -o $@ $< -I.

$(OBJDIR)/%: $(OBJDIR)/%.o
	@mkdir -p $(dir $@)
	$(CXX) -g -o $@ $^ $(LDFLAGS)

$(OBJDIR)/%.bin: $(OBJDIR)/src/%.o $(COMMON) src/%.ld 
	@mkdir -p $(dir $@)
	$(LLVM)ld.lld \
		-Map $(OBJDIR)/$*.map \
		-T src/$*.ld \
		-o $@ \
		$(filter %.o, $^)

$(OBJDIR)/%.o: %.s src/zif.inc src/_globals.inc
	@mkdir -p $(dir $@)
	$(LLVM)mos-cpm65-clang -g $(CLANGFLAGS) -c -o $@ $<

$(OBJDIR)/src/bbctube.o: CLANGFLAGS += -mcpu=mos65c02

$(OBJDIR)/tools/tubeemu/tubeemu: LDFLAGS += -lreadline -lelf
$(OBJDIR)/tools/tubeemu/tubeemu: \
	$(OBJDIR)/third_party/lib6502/lib6502.o \
	$(OBJDIR)/tools/tubeemu/mos.o \
	$(OBJDIR)/tools/tubeemu/emulator.o \

$(OBJDIR)/tools/tubeemu/%.o: tools/tubeemu/globals.h

include tests/build.mk

.SECONDARY:

clean:
	rm -rf $(OBJDIR) bin
