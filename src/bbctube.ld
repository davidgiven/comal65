MEMORY {
    zp : ORIGIN = 0, LENGTH = 0xee
    ram (rw) : ORIGIN = 0x400, LENGTH = 0xf400
}

SECTIONS {
    OSARGS = 0xffda;
    OSASCI = 0xffe3;
    OSBGET = 0xffd7;
    OSBPUT = 0xffd4;
    OSBYTE = 0xfff4;
    OSFIND = 0xffce;
    OSNEWL = 0xffe7;
    OSWORD = 0xfff1;
    OSWRCH = 0xffee;

    BRKV = 0x202;

    platform_putchar = OSWRCH;
    platform_bget = OSBGET;
    platform_bput = OSBPUT;
 
	.zp : {
		*(.zp .zp.*)
	} >zp

	.text : { *(.text .text.*) } >ram
	.data : { *(.data .data.* .rodata .rodata.*) } > ram
	.noinit (NOLOAD) : { *(.noinit .noinit.* .bss .bss.* COMMON) } >ram
        
    /* Must be last --- this is where the program data starts. */

    heapbase = .;
}

OUTPUT_FORMAT {
	TRIM(ram)
}

