.include "src/zif.inc"
.include "src/_globals.inc"
.include "tests/_futils.inc"

zproc _start
    zloop
        ldx item_counter
        cpx #data_end - data_start
        zbreakif_eq
        
        ; Load unpacked version and print.

        jsr load_value_into_i0
        jsr print_i0
        jsr space
        
        ; Run code and print result.

        ldx #v0
        jsr ffromint

        jsr print_i0
        jsr space

        ; Advance.
        
        jsr OSNEWL
        
        lda item_counter
        clc
        adc #4
        sta item_counter
    zendloop 

    rts
zendproc

item_counter:
    .byte 0

data_start:
    .long 0
    .long 1
    .long 2
    .long 3

    .long -1
    .long -2
    .long -3

    .long 0x876543
data_end: