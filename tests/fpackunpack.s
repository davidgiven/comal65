.include "src/zif.inc"
.include "src/_globals.inc"

zproc _start
    zloop
        ldx item_counter
        cpx #data_end - data_start
        zbreakif_eq
        
        ; Print source.

        lda data_start+0, x
        sta i0+0
        jsr print_hex8_number

        lda data_start+1, x
        sta i0+1
        jsr print_hex8_number

        lda data_start+2, x
        sta i0+2
        jsr print_hex8_number
        
        lda data_start+3, x
        sta i0+3
        jsr print_hex8_number

        jsr space

        ; Run code.

        ldx #v0
        jsr funpack

        ; Print unpacked version.

        lda i0+0
        jsr print_hex8_number
        jsr space

        lda i0+1
        jsr print_hex8_number
        jsr space
        
        lda i0+2
        jsr print_hex8_number
        jsr space
        
        lda i0+3
        jsr print_hex8_number
        jsr space
        
        lda f0
        jsr print_hex8_number
        jsr space

        lda v0
        jsr print_hex8_number
        jsr space
        
        ; Run code.

        ldx #v0
        jsr fpack

        ; Print packed version again.

        jsr print_hex8_number

        lda i0+1
        jsr print_hex8_number

        lda i0+2
        jsr print_hex8_number
        
        lda i0+3
        jsr print_hex8_number

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

zproc space
    lda #' '
    jmp platform_putchar
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