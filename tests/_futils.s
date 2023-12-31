.include "src/zif.inc"
.include "src/_globals.inc"
.include "tests/_futils.inc"

zproc load_value_into_i0
    ldx item_counter
    ldy #0
    zrepeat
        lda data_start, x
        sta i0, y
        inx
        iny
        cpy #4
    zuntil_eq
    rts
zendproc

zproc load_value_into_v0
    ldx item_counter
    ldy #0
    zrepeat
        lda data_start, x
        sta v0, y
        inx
        iny
        cpy #6
    zuntil_eq
    rts
zendproc

zproc print_i0
    ldy #0
    zrepeat
        lda i0, y
        jsr print_hex8_number
        iny
        cpy #4
    zuntil_eq
    rts
zendproc

zproc print_v0
    ldy #0
    zrepeat
        lda v0, y
        jsr print_hex8_number
        jsr space
        iny
        cpy #6
    zuntil_eq
    rts
zendproc

zproc space
    lda #' '
    jmp platform_putchar
zendproc
