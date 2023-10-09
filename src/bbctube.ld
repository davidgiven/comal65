MEMORY {
    zp : ORIGIN = 0, LENGTH = 0xee
    ram (rw) : ORIGIN = 0x400, LENGTH = 0xf400
}

SECTIONS {
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

