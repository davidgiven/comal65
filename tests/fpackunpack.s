.include "src/zif.inc"
.include "src/_globals.inc"
.include "tests/_futils.inc"

zproc _start
    zloop
        ldx item_counter
        cpx #data_end - data_start
        zbreakif_eq
        
        ; Print source.

        jsr load_value_into_i0
        jsr print_i0
        jsr space

        ; Run code.

        ldx #v0
        jsr funpack

        ; Print unpacked version.

        jsr print_v0
        
        ; Run code.

        ldx #v0
        jsr fpack

        ; Print packed version again.

        jsr print_i0

        ; Advance.
        
        jsr OSNEWL
        
        ldx item_counter
        inx
        inx
        inx
        inx
        stx item_counter
    zendloop 

    rts
zendproc

item_counter:
    .byte 0
    
data_start:
    .float 0
    .float 1
    .float 2
    .float 3
    .float -0
    .float -1
    .float -2
    .float -3
    .float 1e10
    .float -1e10
    .float 1e-10
    .float -1e-10
    .float 8388607
    .float 8388608
    .float -8388607
    .float -8388608
data_end: