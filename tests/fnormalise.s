.include "src/zif.inc"
.include "src/_globals.inc"
.include "tests/_futils.inc"

zproc _start
    zloop
        ldx item_counter
        cpx #data_end - data_start
        zbreakif_eq
        
        ; Load unpacked version and print.

        jsr load_value_into_v0
        jsr print_v0
        jsr space
        
        ; Run code and print result.

        ldx #v0
        jsr fnormalise

        jsr print_v0
        jsr space

        ; Print packed version.

        ldx #v0
        jsr fpack
        jsr print_i0

        ; Advance.
        
        jsr OSNEWL
        
        lda item_counter
        clc
        adc #6
        sta item_counter
    zendloop 

    rts
zendproc

item_counter:
    .byte 0

.macro number sgn, man, exp
    .byte \sgn
    .long \man
    .byte \exp
.endm    

data_start:
    ; sgn, man0, man1, man2, man3, exp

    ; Control

    number 0x00, 0x00800000, 0x00 ; 0
    number 0x00, 0x00800000, 0x7f ; 1
    number 0x00, 0x00800000, 0x80 ; 2
    number 0x00, 0x00c00000, 0x80 ; 3

    number 0x00, 0x01000000, 0x00 ; 0 << 1
    number 0x00, 0x01000000, 0x7e ; 1 << 1
    number 0x00, 0x01010101, 0x7e ; 1 and a bit << 1
    number 0x00, 0x01000000, 0x7f ; 2 << 1

    number 0x00, 0x00400000, 0x00 ; 0 >> 1
    number 0x00, 0x00400000, 0x80 ; 1 >> 1
    number 0x00, 0x00404040, 0x80 ; 1 and a bit >> 1
    number 0x00, 0x00400000, 0x81 ; 2 >> 1
data_end: