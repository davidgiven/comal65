.include "src/zif.inc"
.include "src/_globals.inc"

; This implements an abbreviated version of the standard 32-bit IEEE754 floating
; point standard, with an 8-bit exponent with a bias of -127 and a 23-bit
; mantissa with hidden bit:
;
;      +3        +2        +1        +0
;  --------- --------- --------- ---------
;  SEEE EEEE EMMM MMMM MMMM MMMM MMMM MMMM
;
; Inf is not supported. Shortcuts have been taken everywhere for size and speed,
; so it's definitely not conformant.

.section .zp, "zax", @nobits

SGN  = 0
MAN0 = 1
MAN1 = 2
MAN2 = 3
MAN3 = 4
EXP  = 5

; On entry: X points at V value
zproc funpack
	lda MAN2, x		; high byte of mantissa
	cmp #0x80		; set carry from MSB (bottom bit of exponent)
	ora #0x80		; set hidden bit
	sta MAN2, x

	lda MAN3, x     ; get exponent
	rol             ; shift in bottom bit of exponent, shift out sign
	sta EXP, x      

    lda #0
    sta MAN3, x
    ror             ; shift in sign
    sta SGN, x
    rts
zendproc

; On entry: X points at unpacked V value
zproc fpack
    lda SGN, x
    rol             ; shift out sign
    lda EXP, x
    ror             ; shift in sign, shigt out bottom bit
    sta MAN3, x
    
    lda MAN2, x
    and #0x7f       ; clear hidden bit
    zif_cs
        ora #0x80   ; set bottom bit of exponent if necessary
    zendif
    sta MAN2, x

    rts
zendproc
    
; On entry: X points at unpacked V value
zproc fnormalise
    lda EXP, x
    zif_eq
        ; This is zero.

    normalise_to_zero:
        lda #0
        sta SGN, x
        sta MAN0, x
        sta MAN1, x
        sta MAN3, x
        lda #0x80
        sta MAN2, x
        rts
    zendif
    
    lda MAN3, x
    zif_eq
        ; Normalise left, increasing the value and decreasing the exponent until
        ; the mantissa is in range. This can loop a maximum of 24 times.
        ; TODO: optimise shifts of eight or more.

        lda MAN2, x
        zif_pl
            zrepeat
                lda MAN2, x
                zbreakif_mi

                dec EXP, x
                beq precision_lost
                asl MAN0, x
                rol MAN1, x
                rol
            zuntil_mi
            sta MAN2, x
        zendif
    zelse
        ; Normalise right, reducing the value and increasing the exponent until
        ; the mantissa is in range. This can loop a maximum of eight times.

        ; A = MAN3 on entry
        zrepeat
            inc EXP, x
            beq infinity
            lsr a
            ror MAN2, x
            ror MAN1, x
            ror MAN0, x
            
            tay             ; set flags
        zuntil_eq
        sta MAN3, x
    zendif
    rts

precision_lost:
infinity:
    brk
    rts
zendproc
        

#if 0
FNEG:	TSX
	LDA	0x0104,X
	EOR	#0x80
	STA	0x0104,X
	JMP	INCNEXTOP
FSUB:	LDA	#0x80		; TOGGLE SIGN
	BNE	:+
FADD:	LDA	#0x00
:	STA	FP2SGN
	PLA
	STA	FP2MAN0
	PLA
	STA	FP2MAN1
	PLA
	CMP	#0x80		; SET CARRY FROM MSB
	ORA	#0x80		; SET HIDDEN BIT
	STA	FP2MAN2
	PLA
	EOR	FP2SGN		; TOGGLE SIGN FOR FSUB
	ROL
	STA	FP2EXP
	LDA	#0x00
	STA	FPSGN
	BCC	:+
	SBC	FP2MAN0
	STA	FP2MAN0
	LDA	#0x00
	SBC	FP2MAN1
	STA	FP2MAN1
	LDA	#0x00
	SBC	FP2MAN2
	STA	FP2MAN2
	LDA	#0xFF
:	STA	FP2MAN3
	PLA
	STA	FP1MAN0
	PLA
	STA	FP1MAN1
	PLA
	CMP	#0x80		; SET CARRY FROM MSB
	ORA	#0x80		; SET HIDDEN BIT
	STA	FP1MAN2
	PLA
	ROL
	STA	FP1EXP
	LDA	#0x00
	BCC	:+
	SBC	FP1MAN0
	STA	FP1MAN0
	LDA	#0x00
	SBC	FP1MAN1
	STA	FP1MAN1
	LDA	#0x00
	SBC	FP1MAN2
	STA	FP1MAN2
	LDA	#0xFF
:	STA	FP1MAN3
	LDA	FP1EXP		; CALCULATE WHICH MANTISSA TO SHIFT
	STA	fpexp
	SEC
	SBC	FP2EXP
	BEQ	FADDMAN
	BCS	:+
	EOR	#0xFF
	TAY
	INY
	LDA	FP2EXP
	STA	fpexp
	LDA	FP1MAN3
	CPY	#24		; KEEP SHIFT RANGE VALID
	BCC	FP1SHFT
	LDA	#0x00
	STA	FP1MAN3
	STA	FP1MAN2
	STA	FP1MAN1
	STA	FP1MAN0
	BEQ	FADDMAN
FP1SHFT:	CMP	#0x80		; SHIFT FP1 DOWN
	ROR
	ROR	FP1MAN2
	ROR	FP1MAN1
	ROR	FP1MAN0
	DEY
	BNE	FP1SHFT
	STA	FP1MAN3
	JMP	FADDMAN
:	TAY
	LDA	FP2MAN3
	CPY	#24		; KEEP SHIFT RANGE VALID
	BCC	FP2SHFT
	LDA	#0x00
	STA	FP2MAN3
	STA	FP2MAN2
	STA	FP2MAN1
	STA	FP2MAN0
	BEQ	FADDMAN
FP2SHFT:	CMP	#0x80		; SHIFT FP2 DOWN
	ROR
	ROR	FP2MAN2
	ROR	FP2MAN1
	ROR	FP2MAN0
	DEY
	BNE	FP2SHFT
	STA	FP2MAN3
FADDMAN:	LDA	FP1MAN0
	CLC
	ADC	FP2MAN0
	STA	fpman0
	LDA	FP1MAN1
	ADC	FP2MAN1
	STA	fpman1
	LDA	FP1MAN2
	ADC	FP2MAN2
	STA	fpman2
	LDA	FP1MAN3
	ADC	FP2MAN3
	STA	fpman3
	BPL	fpnorm
	LDA	#0x80
	STA	FPSGN
	LDA	#0x00
	SBC	fpman0
	STA	fpman0
	LDA	#0x00
	SBC	fpman1
	STA	fpman1
	LDA	#0x00
	SBC	fpman2
	STA	fpman2
	LDA	#0x00
	SBC	fpman3
	STA	fpman3
fpnorm:	BEQ	FPNORMLEFT		; NORMALIZE FP, A = fpmanT3
fpnormRIGHT:	INC	fpexp
	LSR
	STA	fpman3
	ROR	fpman2
	ROR	fpman1
	LDA	fpman0
	ROR
	ADC	#0x00
	STA	fpman0
	LDA	fpman1
	ADC	#0x00
	STA	fpman1
	LDA	fpman2
	ADC	#0x00
	STA	fpman2
	LDA	fpman3
	ADC	#0x00
	BNE	fpnormRIGHT
	LDA	fpexp
	ASL	fpman2
	LSR
	ORA	FPSGN
	PHA
	LDA	fpman2
	ROR
	PHA
	LDA	fpman1
	PHA
	LDA	fpman0
	PHA
	JMP	INCNEXTOP
fpnormLEFT:	LDA	fpman2
	BNE	fpnormLEFT1
	LDA	fpman1
	BNE	fpnormLEFT8
	LDA	fpman0
	BNE	fpnormLEFT16
	PHA			; RESULT IS ZERO
	PHA
	PHA
	PHA
	JMP	INCNEXTOP
fpnormLEFT16:	TAX
	LDA	fpexp
	SEC
	SBC	#0x10
	STA	fpexp
	LDA	#0x00
	STA	fpman1
	STA	fpman0
	TXA
	BNE	fpnormLEFT1
fpnormLEFT8:	TAX
	LDA	fpman0
	STA	fpman1
	LDA	fpexp
	SEC
	SBC	#0x08
	STA	fpexp
	LDA	#0x00
	STA	fpman0
	TXA
fpnormLEFT1:	BMI	FPNORMDONE
:	DEC	fpexp
	ASL	fpman0
	ROL	fpman1
	ROL
	BPL	:-
fpnormDONE:	ASL
	TAX
	LDA	fpexp
	LSR
	ORA	FPSGN
	PHA
	TXA
	ROR
	PHA
	LDA	fpman1
	PHA
	LDA	fpman0
	PHA
	JMP	INCNEXTOP
FMUL:	PLA
	STA	FP2MAN0
	PLA
	STA	FP2MAN1
	PLA
	CMP	#0x80		; SET CARRY FROM MSB
	ORA	#0x80		; SET HIDDEN BIT
	STA	FP2MAN2
	PLA
	ROL
	STA	FP2EXP
	BNE	:+
	TSX			; MUL BY ZERO, RESULT ZERO
;	LDA	#0x00
	STA	0x0101,X
	STA	0x0102,X
	STA	0x0103,X
	STA	0x0104,X
	JMP	INCNEXTOP
:	LDA	#0x00
	ROR
	STA	FPSGN
	PLA
	STA	FP1MAN0
	PLA
	STA	FP1MAN1
	PLA
	CMP	#0x80		; SET CARRY FROM MSB
	ORA	#0x80		; SET HIDDEN BIT
	STA	FP1MAN2
	PLA
	ROL
	STA	FP1EXP
	BNE	:+
;	LDA	#0x00
	PHA			; MUL BY ZERO, RESULT ZERO
	PHA
	PHA
	PHA
	JMP	INCNEXTOP
:	LDA	#0x00
	ROR
	EOR	FPSGN
	STA	FPSGN
	LDA	FP1EXP
	CLC			; ADD EXPONENTS
	ADC	FP2EXP
	SEC			; SUBTRACT BIAS
	SBC	#0x7F
	STA	fpexp
	LDX	#0x00
	STX	fpman0
	STX	fpman1
	STX	fpman2
	STX	fpman3
	STX	TMP
FMULNEXTBYTE:	LDA	FP1MAN0,X
	BNE	:+
	LDX	fpman1		; SHORT CIRCUIT BYTE OF ZERO BITS
	STX	fpman0
	LDX	fpman2
	STX	fpman1
	LDX	fpman3
	STX	fpman2
	STA	fpman3
	INC	TMP
	LDX	TMP
	CPX	#0x03
	BNE	FMULNEXTBYTE
	LDA	fpman3
	JMP	fpnorm
:	EOR	#0xFF
	LDX	#0x08
FMULTSTBITS:	LSR	fpman3
	ROR	fpman2
	ROR	fpman1
	ROR	fpman0
	LSR
	BCS	FMULNEXTTST
	TAY
	LDA	FP2MAN0
	ADC	fpman0
	STA	fpman0
	LDA	FP2MAN1
	ADC	fpman1
	STA	fpman1
	LDA	FP2MAN2
	ADC	fpman2
	STA	fpman2
	LDA	#0x00
	ADC	fpman3
	STA	fpman3
	TYA
FMULNEXTTST:	DEX
	BNE	FMULTSTBITS
	INC	TMP
	LDX	TMP
	CPX	#0x03
	BNE	FMULNEXTBYTE
	LDA	fpman3
	JMP	fpnorm
FDIV:	PLA
	STA	FP2MAN0
	PLA
	STA	FP2MAN1
	PLA
	CMP	#0x80		; SET CARRY FROM MSB
	ORA	#0x80		; SET HIDDEN BIT
	STA	FP2MAN2
	PLA
	ROL
	STA	FP2EXP
	BNE	:+
	LDA	#23		; DIVIDE BY ZERO, ERROR
	JMP	SYSTHROW
:	LDA	#0x00
	ROR
	STA	FPSGN
	PLA
	STA	FP1MAN0
	PLA
	STA	FP1MAN1
	PLA
	CMP	#0x80		; SET CARRY FROM MSB
	ORA	#0x80		; SET HIDDEN BIT
	STA	FP1MAN2
	PLA
	ROL
	STA	FP1EXP
	BNE	:+
;	LDA	#0x00
	PHA			; DIVIDE ZERO, RESULT ZERO
	PHA
	PHA
	PHA
	JMP	INCNEXTOP
:	LDA	#0x00
	STA	FP1MAN3
	ROR
	EOR	FPSGN
	STA	FPSGN
	LDA	FP1EXP
	SEC			; SUBTRACT EXPONENTS
	SBC	FP2EXP
	CLC
	ADC	#0x7F		; ADD BACK BIAS
	STA	fpexp
	LDX	#24		; #BITS
FDIVLOOP:	LDA	FP1MAN0
	SEC
	SBC	FP2MAN0
	STA	TMP
	LDA	FP1MAN1
	SBC	FP2MAN1
	STA	TMP+1
	LDA	FP1MAN2
	SBC	FP2MAN2
	TAY
	LDA	FP1MAN3
	SBC	#0x00
	BCC	FDIVNEXTBIT
	STA	FP1MAN3
	STY	FP1MAN2
	LDA	TMP+1
	STA	FP1MAN1
	LDA	TMP
	STA	FP1MAN0
FDIVNEXTBIT:	ROL	fpman0
	ROL	fpman1
	ROL	fpman2
	ASL	FP1MAN0
	ROL	FP1MAN1
	ROL	FP1MAN2
	ROL	FP1MAN3
	DEX
	BNE	FDIVLOOP
	LDA	#0x00
	JMP	fpnorm
FCMPL:
FCMPG:	TSX
	TXA
	TAY
	CLC
	ADC	#0x08
	TAX
	TXS
	LDA	0x0104,Y		; COMPARE SIGNS
	AND	#0x80
	STA	FP2SGN
	LDA	0x0108,Y
	AND	#0x80
	CMP	FP2SGN
	BCC	FCMPGTSGN
	BEQ	:+
	BCS	FCMPLTSGN
:	LDA	0x0108,Y		; COMPARE AS MAGNITUDE
	CMP	0x0104,Y
	BCC	FCMPLT
	BEQ	:+
	BCS	FCMPGT
:	LDA	0x0107,Y
	CMP	0x0103,Y
	BCC	FCMPLT
	BEQ	:+
	BCS	FCMPGT
:	LDA	0x0106,Y
	CMP	0x0102,Y
	BCC	FCMPLT
	BEQ	:+
	BCS	FCMPGT
:	LDA	0x0105,Y
	CMP	0x0101,Y
	BCC	FCMPLT
	BEQ	FCMPEQ
	BCS	FCMPGT
FCMPEQ:	PHA			; EQUAL
	PHA
	PHA
	PHA
	JMP	INCNEXTOP
FCMPGT:	LDA	FP2SGN		; FLIP RESULT IF NEGATIVE #S
	BMI	FCMPLTSGN
FCMPGTSGN:	LDA	#0x00		; GREATER THAN
	PHA
	PHA
	PHA
	LDA	#0x01
	PHA
	JMP	INCNEXTOP
FCMPLT:	LDA	FP2SGN		; FLIP RESULT IF NEGATIVE #S
	BMI	FCMPGTSGN
FCMPLTSGN:	LDA	#0xFF		; LESS THAN
	PHA
	PHA
	PHA
	PHA
	JMP	INCNEXTOP
#endif

#if 0
; Entry: x = pointer to a four-byte float

zproc funpack
	lda 0, x
	sta fpman0
	lda 1, x
	sta fpman1
	lda 2, x
	cmp #0x80		; set carry from MSB
	ora #0x80		; set hidden bit
	sta fpman2
	lda 3, x
	rol
	sta fpexp
	lda	#0x00
	ror
	sta	fpsgn
	rts
zendproc

; Entry: i0 = four-byte float

zproc f2i
	ldx #i0
	jsr funpack

	lda	fpexp			; check for less than one
	sec
	sbc	#0x7F
	zif_cc
		lda #0			; return zero
		sta i0+0
		sta i0+1
		sta i0+2
		sta i0+3
		rts
	zendif

	cmp #23
	zif_cc
		sta	fpexp
		lda	#23
		sec
		sbc	fpexp
		tax				; shift mantissa right
		lda	fpman2
		zrepeat
			lsr a
			ror	fpman1
			ror	fpman0
			dex
		zuntil_eq
		sta	fpman2
		stx	fpman3

	f2ichkneg:
		lda	fpsgn
		zif_mi
			asl
			sbc	fpman0
			sta	fpman0
			lda	#0x00
			sbc	fpman1
			sta	fpman1
			lda	#0x00
			sbc	fpman2
			sta	fpman2
			lda	#0x00
			sbc	fpman3
			sta	fpman3
		zendif

		lda fpman0
		sta i0+0
		lda fpman1
		sta i0+1
		lda fpman2
		sta i0+2
		lda fpman3
		sta i0+33
		rts
	zelse
		cmp #32
		zif_cs
			lda #0xff		; overflow, store maxint
			sta	fpman0
			sta	fpman1
			sta	fpman2
			lsr
			sta	fpman3
			bne	f2ichkneg
		zendif
		sec
		sbc	#23
		zif_eq
			sta	fpman3		; stores zero
			beq	f2ichkneg	; always taken
		zendif
		tax					; shift mantissa left
		lda	#0x00
		zrepeat
			asl	fpman0
			rol	fpman1
			rol	fpman2
			rol
			dex
		zuntil_eq
		sta	fpman3			; stores zero
		beq	f2ichkneg		; always taken
	zendif
zendproc

zproc i2f
	lda i0+0
	sta fpman0
	lda i0+1
	sta fpman1
	lda i0+2
	sta fpman2
	
	lda i0+3			; high byte ignored except for sign
	and #0x80
	
#if 0
,	PLA
	STA	fpman0
	PLA
	STA	fpman1
	PLA
	STA	fpman2
	PLA
	STA	fpman3
	AND	#0x80
	STA	FPSGN
	BPL	:+
	LDX	#fpman0
	JSR	NEGINT
:	LDA	#0x7F+23
	STA	fpexp
	LDA	fpman3
	JMP	fpnorm
#endif
zendproc
#endif

; vim: sw=4 ts=4 et
