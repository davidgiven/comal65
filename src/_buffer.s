.include "src/zif.inc"
.include "src/_globals.inc"

.comm text_buffer, 256

.section .zp, "zax", @nobits

bufferpos: .byte 0 ; current beginning of token in input buffer
bufferend: .byte 0 ; current end of token in input buffer

; Prints the decimal number in i0.

zproc buffer_print_i0
    lda #10
    sta i2+0
    lda #0
    sta i2+1
    sta i2+2
    sta i2+3
    sta p0+0

    ; Is this a negative number?

    lda i0+3
    zif_mi
        lda #'-'
        jsr buffer_print_char

        jsr negi0
    zendif

    zrepeat
        jsr divu4
        lda i1+0
        pha
        inc p0+0

        lda i0+0
        ora i0+1
        ora i0+2
        ora i0+3
    zuntil_eq

    zrepeat
        pla
        clc
        adc #'0'
        jsr buffer_print_char

        dec p0+0
    zuntil_eq
    rts
zendproc

; Outputs the high-bit-terminated string in AX.

zproc buffer_print_string
    sta p0+0
    stx p0+1

    ldy #0xff
    sty p1+0

    ldx bufferend
    zrepeat
        inc p1+0
        ldy p1+0
        lda (p0), y
        and #0x7f
        sta text_buffer, x
        inx

        ldy p1+0
        lda (p0), y
    zuntil_mi
    stx bufferend

    rts
zendproc

zproc buffer_print_separated_string
    jsr buffer_print_string
zendproc
    ; fall through
zproc buffer_print_space
    lda #' '
zendproc
    ; fall through
zproc buffer_print_char
    ldx bufferend
    cpx #0xff - STRING_DATA
    zif_eq
        jmp error_string_overflow
    zendif
    sta text_buffer, x
    inc bufferend
    rts
zendproc

; On entry:
;   X - address of v register

zproc buffer_print_stringvalue
    ldy #0
    lda STACKVALUE_STRING_LENGTH, x
zendproc
    ; fall through
    
; On entry:
;   Y - offset into string to start printing
;   A - number of characters to print
;   X - address of v register
; Uses p0.

zproc buffer_print_partial_stringvalue
    pha                         ; push length
    tya
    pha                         ; push offset

    txa
    tay
    ldx #p0
    jsr get_string_pointer

    pla 
    tay                         ; pop offset
    pla
    tax                         ; pop length

    zif_ne
        zrepeat
            txa
            pha

            lda (p0), y
            jsr buffer_print_char

            pla
            tax

            iny
            dex
        zuntil_eq
    zendif
    rts
zendproc

zproc buffer_print_nl
    lda #0x0d
    jsr buffer_print_char
    lda #0x0a
    bne buffer_print_char       ; always taken
zendproc

zproc buffer_print_current_line_number
    ldy #LINE_NUMBER+0
    lda (execlineptr), y
    sta i0+0
    iny
    lda (execlineptr), y
    sta i0+1
    lda #0
    sta i0+2
    sta i0+3
        
    jmp buffer_print_i0
zendproc

; Reads a string into the text buffer and sets the pointers.

zproc buffer_read_string
    jsr platform_read_line

    ; Set up the pointers.

    inx
    inx
    stx bufferend
    lda #2
    sta bufferpos
    rts
zendproc

; Parses an integer from bufferpos and leaves it in i0. Trashes i1 and
; i2.
; On exit:
;   C - set if the parse failed

zproc buffer_parse_int
    jsr parser_skip_whitespace

    ; Clear i2 and i0.

    ldx #3
    lda #0
    zrepeat
        sta i2, x
        sta i0, x
        dex
    zuntil_mi
    lda #OTYPE_INTEGER
    sta v0

    ; Set i2 to 10 for the multiplication.

    lda #10
    sta i2+0

    lda bufferpos
    sta bufferend

    ; Is the number negative?

    ldx bufferend
    lda text_buffer, x
    cmp #'-'
    php                 ; save comparison result for later
    zif_eq
        inc bufferpos
        inc bufferend
    zendif

    zloop
        ldx bufferend
        lda text_buffer, x
        cmp #'0'
        zbreakif_cc
        cmp #'9'+1
        zbreakif_cs

        pha
        jsr mul4
        pla

        sec
        sbc #'0'
        clc
        adc i0+0
        sta i0+0
        zif_cs
            inc i0+1
            zif_eq
                inc i0+2
                zif_eq
                    inc i0+3
                zendif
            zendif
        zendif

        inc bufferend
    zendloop

    lda bufferpos
    cmp bufferend
    zif_eq
        plp
        sec
        rts
    zendif

    plp                 ; is the number negative?
    zif_eq
        jsr negi0
    zendif

    lda bufferend
    sta bufferpos
    clc
    rts
zendproc

; vim: ts=4 sw=4 et:
