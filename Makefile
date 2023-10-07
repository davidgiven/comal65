CA65 = ca65
LD65 = ld65
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

$(OBJDIR)/%.bin: $(OBJDIR)/src/%.o src/%.cfg
	@mkdir -p $(dir $@)
	$(LD65) --config src/$*.cfg -o $@ $(OBJDIR)/src/$*.o

$(OBJDIR)/%.o: %.S
	@mkdir -p $(dir $@)
	$(CA65) -o $@ $<

clean:
	rm -rf $(OBJDIR) bin
