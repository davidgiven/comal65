.include "src/zif.inc"
.include "src/_globals.inc"

; Computes i1i0 := i0 * i2.

zproc mul4
    lda #0              ; clear high part of result
    sta i1+0
    sta i1+1
    sta i1+2
    sta i1+3

    clc
    ldy #$21            ; bit counter

    zrepeat
        ror i1+3            ; shift right result and lhs
        ror i1+2
        ror i1+1
        ror i1+0
        ror i0+3
        ror i0+2
        ror i0+1
        ror i0+0
        zif_cs
            ldx #-4 & 0xff      ; add rhs to low half of result
            clc
            zrepeat
                lda i1+4, x
                adc i2+4, x
                sta i1+4, x
                inx
            zuntil_eq
        zendif

        dey
    zuntil_eq
    rts
zendproc

; Computes i0 rem i1 := i0 / i2, unsigned.
; i2 is unmodified.

zproc divu4
    lda #0              ; clear remainder
    ldx #3
    zrepeat
        sta i1, x
        dex
    zuntil_mi

    ldy #32             ; bit counter
    zrepeat
        asl i0+0            ; left shift result and remainder
        rol i0+1
        rol i0+2
        rol i0+3
        rol i1+0
        rol i1+1
        rol i1+2
        rol i1+3

        sec                 ; if remainder < rhs
        ldx #-4 & 0xff
        zrepeat
            lda i1+4, x
            sbc i2+4, x
            inx
        zuntil_eq
        zif_cs
            ldx #-4 & 0xff      ; remainder := remainder - rhs
            zrepeat
                lda i1+4, x
                sbc i2+4, x
                sta i1+4, x
                inx
            zuntil_eq
            inc i0
        zendif

        dey
    zuntil_eq
    rts
zendproc

; Computes i0 rem i1 := i0 / i2, signed.
; i2 is unmodified.

zproc divs4
    lda i0+3            ; high byte of LHS
    eor i2+3            ; high byte of RHS
    php                 ; save sign of result for later
    lda i0+3            ; high byte of LHS
    php                 ; save sign of remainder for later
    zif_mi              ; negate LHS if negative
        jsr negi0
    zendif

    lda i2+3            ; get sign of RHS
    zif_mi
        jsr negi2       ; negate RHS if negative
    zendif

    jsr divu4           ; actually do the division

    plp                 ; get sign of remainder
    zif_mi
        jsr negi1       ; negate remainder if required
    zendif

    plp                 ; get sign of result
    zif_mi
        jsr negi0       ; invert result if required
    zendif
    rts
zendproc

; Negates i0/i1/i2.

zproc negi0
    ldx #i0
    bne negi            ; always taken
zendproc

zproc negi1
    ldx #i1
    bne negi            ; always taken
zendproc

zproc negi2
    ldx #i2
zendproc
    ; falls through
zproc negi
    ldy #(-4) & 0xff
    sec
    zrepeat
        lda #0
        sbc 0, x
        sta 0, x
        inx
        iny
    zuntil_eq
    rts
zendproc


; vim: ts=4 sw=4 et:
