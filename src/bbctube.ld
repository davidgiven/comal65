MEMORY {
    zp : ORIGIN = 0, LENGTH = 0xee
    ram (rw) : ORIGIN = 0x400, LENGTH = 0xf400
}

SECTIONS {
    OSBYTE = 0xfff4;
    OSFIND = 0xffce;
    OSWORD = 0xfff1;
    OSWRCH = 0xffee;
    OSBGET = 0xffd7;
    OSBPUT = 0xffd4;
    OSNEWL = 0xffe7;

    BRKV = 0x202;

    platform_putchar = OSWRCH;
    platform_bget = OSBGET;
    platform_bput = OSBPUT;
 
	.zp : {
		*(.zp .zp.*)
	} >zp

	.text : { *(.text .text.*) } >ram
	.data : { *(.data .data.* .rodata .rodata.*) } > ram
	.noinit (NOLOAD) : { *(.noinit .noinit.*) } >ram
}

OUTPUT_FORMAT {
	TRIM(ram)
}

