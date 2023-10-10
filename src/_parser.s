.include "src/zif.inc"
.include "src/_globals.inc"

.section .zp, "zax", @nobits

; Parser temporaries

parsecmd:           .word 0 ; points into parser bytecode
parsefailstackptr:  .byte 0 ; stack pointer to rewind to
parsestartstackptr: .byte 0 ; stack pointer when parse starts
bytecodeptr:        .byte 0 ; offset into bytecode buffer (TODO: remove)

.data 11
pop_exec_hi:
.data 12
pop_exec_lo:

.macro defpop exec
    .data 11
    .byte (\exec-1)@mos16hi
    .data 12
    .byte (\exec-1)@mos16lo
.endm

P_TRY_OP         = 0
P_JSR_OP         = 1
P_JMP_OP         = 2
P_FAIL_OP        = 3
P_PANIC_OP       = 4
P_RETURN_OP      = 5
P_COMMIT_OP      = 6
P_TOKEN_OP       = 7
P_EAGER_TOKEN_OP = 8
P_EMIT_OP        = 9
P_EMIT4_OP       = 10
P_SHUNT_OP       = 11
P_FLUSH_OP       = 12
P_WORD_OP        = 13
P_NUMBER_OP      = 14
P_EOL_OP         = 15
P_PAREN_OP       = 16
P_SUCCEED_OP     = 17
P_STRING_OP      = 18
P_PARAM_OP       = 19
P_FLUSH_AND_EMIT_VARARG_OP = 20

defpop pop_try
defpop pop_jsr
defpop pop_jmp
defpop pop_fail
defpop pop_panic
defpop pop_return
defpop pop_commit
defpop pop_token
defpop pop_eager_token
defpop pop_emit
defpop pop_emit4
defpop pop_shunt
defpop pop_flush
defpop pop_word
defpop pop_number
defpop pop_eol
defpop pop_paren
defpop pop_succeed
defpop pop_string
defpop pop_param
defpop pop_flush_and_emit_vararg

zproc parse_line
    lda #0
    sta bytecode_buffer+LINE_NUMBER+0
    sta bytecode_buffer+LINE_NUMBER+1
    sta bytecode_buffer+LINE_INDENT

    ldx #LINE_DATA
    stx bytecodeptr
    ldx #0
    stx opstackptr
    tsx
    stx parsestartstackptr

    lda #<immediate_statement_rule
    sta parsecmd+0
    lda #>immediate_statement_rule
    sta parsecmd+1

    lda #<pop_succeed_ptr
    pha
    lda #>pop_succeed_ptr
    pha
zendproc
    ; fall through
zproc parse_state_machine
    ldy #0
    lda (parsecmd), y
    tax
    lda pop_exec_hi, x
    pha
    lda pop_exec_lo, x
    pha

.if 0
    tsx
    txa
    jsr print_hex8_number
    jsr print_space
    lda parsecmd+0
    ldx parsecmd+1
    jsr print_hex16_number
    jsr print_nl
.endif

    rts
zendproc

; Adds A to parsecmd.
; Preserves X and Y.
parse_advance_3:
    lda #3
    bne parse_advance           ; always taken
parse_advance_2:
    lda #2
    bne parse_advance           ; always taken
parse_advance_1:
    lda #1
zproc parse_advance
    clc
    adc parsecmd+0
    sta parsecmd+0
    zif_cs
        inc parsecmd+1
    zendif
    rts
zendproc

parse_next_2:
    lda #2
    bne parse_next              ; always taken
parse_next_1:
    lda #1
zproc parse_next
    jsr parse_advance
    jmp parse_state_machine
zendproc

; Pushes A onto the bytecode queue.
; Preserves Y.
zproc queue_bytecode
    ldx bytecodeptr
    sta bytecode_buffer, x
    inc bytecodeptr
    rts
zendproc

; Shunts the operator A through the shunting yard algorithm.
zproc shunt_operator
    tay             ; Y contains operator throughout

    jmp 1f
    zrepeat
        lda operator_stack, x
        zbreakif_eq ; if TOS is 0, it's a parenthesis
        tax
        lda token_precedence_table, x ; priority of operator
        cmp token_precedence_table, y ; priority of TOS
        zbreakif_cc ; stack if TOS < operator

        dec opstackptr
        txa
        jsr queue_bytecode

    1:
        ldx opstackptr
    zuntil_eq

    ; Stack the operator onto the operator stack.

stack_operator:
    ldx opstackptr
    inx
    tya
    sta operator_stack, x
    stx opstackptr
    rts
zendproc

; Unstacks operators down to and including a parenthesis marker (or the stack
; is empty).
;
; Returns:
;   A = the parameter count, if this is a parameter list

zproc unstack_operators
    zloop
        ldx opstackptr
        zif_eq
            rts
        zendif

        lda operator_stack, x
        zif_eq                  ; parenthesis marker
            lda operator_stack-1, x
            dec opstackptr
            dec opstackptr
            rts
        zendif

        dec opstackptr
        jsr queue_bytecode
    zendloop
zendproc
    
pop_succeed_ptr:
    .byte P_SUCCEED_OP

zproc pop_succeed
    jsr unstack_operators

    lda #TOKEN_EOL
    ldx bytecodeptr
    sta bytecode_buffer, x
    inx
    stx bytecode_buffer+LINE_SIZE

    ldx parsestartstackptr
    txs
    rts
zendproc

zproc pop_panic
    ldx parsestartstackptr
    txs

    lda #<1f
    ldx #>1f
    jsr print_string

    ldx text_buffer+1
    zif_ne
        lda #0
        sta p0+0
        zrepeat
            ldx p0+0
            lda text_buffer+2, x
            jsr platform_putchar

            inc p0+0
            ldx p0+0
            cpx text_buffer+1
        zuntil_eq
        jsr print_nl

        lda #2
        sta p0+0
        zloop
            ldx p0+0
            cpx bufferpos
            zbreakif_eq

            jsr print_space
            inc p0+0
        zendloop
        lda #'^'
        jsr platform_putchar
        jsr print_nl
    zendif

    jmp error_return
1:
    .ascii "\r\nSyntax error\r"
    .byte '\n'|0x80

zendproc

zproc pop_try
    ; Read parameter into p0.

    ldy #1
    lda (parsecmd), y
    sta p0+0
    iny
    lda (parsecmd), y
    sta p0+1

    ; Advance.

    jsr parse_advance_3

.if 0
    lda #'>'
    jsr platform_putchar
    lda parsecmd+0
    ldx parsecmd+1
    jsr print_hex16_number
    jsr print_nl
.endif

    ; Push backtracking state.

    lda parsefailstackptr
    pha

    lda parsecmd+0
    pha
    lda parsecmd+1
    pha
    lda bufferpos
    pha
    lda bytecodeptr
    pha
    lda opstackptr
    pha

    tsx
    stx parsefailstackptr

    ; Push the marker indicating that this is a backtracking state.

    lda #0
    pha
    pha

    ; Switch to rule being tried.

    lda p0+0
    sta parsecmd+0
    lda p0+1
    sta parsecmd+1

    jmp parse_state_machine
zendproc

; We can't backtrack any more --- if we get a parse failure, panic.

zproc pop_commit
    lda parsefailstackptr
    pha

    lda #<pop_panic_ptr
    pha
    lda #>pop_panic_ptr
    pha
    lda bufferpos
    pha
    lda bytecodeptr
    pha
    lda opstackptr
    pha

    tsx
    stx parsefailstackptr

    ; Push the marker indicating that this is a backtracking state.

    lda #0
    pha
    pha

    jmp parse_next_1
zendproc

pop_panic_ptr:
    .byte P_PANIC_OP

; Revert to a backtracking state.

zproc pop_fail
    ; Revert the state.

.if 0
    lda #'<'
    jsr platform_putchar
    jsr print_nl
.endif

    ldx parsefailstackptr
    txs

    pla
    sta opstackptr
    pla
    sta bytecodeptr
    pla
    sta bufferpos
    pla
    sta parsecmd+1
    pla
    sta parsecmd+0
    pla
    sta parsefailstackptr

    jmp parse_state_machine
zendproc

zproc pop_jsr
    ; Read parameter into p0.

    ldy #1
    lda (parsecmd), y
    sta p0+0
    iny
    lda (parsecmd), y
    sta p0+1

    ; Advance.

    jsr parse_advance_3

    ; Push return point.

    lda parsecmd+0
    pha
    lda parsecmd+1
    pha

    ; Jump to the new rule.

    lda p0+0
    sta parsecmd+0
    lda p0+1
    sta parsecmd+1

    jmp parse_state_machine
zendproc

zproc pop_jmp
    ; Read parameter into parsecmd.

    ldy #1
    lda (parsecmd), y
    tax
    iny
    lda (parsecmd), y
    stx parsecmd+0
    sta parsecmd+1

    jmp parse_state_machine
zendproc
    
zproc pop_return
    ; Pop the return point.

    pla
    sta parsecmd+1
    pla
    sta parsecmd+0

    ; Was this a backtracking marker?

    lda parsecmd+0
    ora parsecmd+1
    zif_eq
        ; It was. Discard the backtracking state and return again.

        pla
        pla
        pla
        pla
        pla
        pla
        sta parsefailstackptr

        jmp pop_return
    zendif

    jmp parse_state_machine
zendproc

zproc pop_shunt
    ldy #1
    lda (parsecmd), y

    jsr shunt_operator

    jmp parse_next_2
zendproc

zproc pop_paren
    ldy #0
    jsr stack_operator
    jsr stack_operator
    jmp parse_next_1
zendproc

zproc pop_param
    ldx opstackptr
    lda operator_stack, x
    zif_ne
        brk
    zendif

    inc operator_stack-1, x
    zif_eq
        jmp error_invalid_parameter
    zendif
    jmp parse_next_1
zendproc

zproc pop_flush
    jsr unstack_operators
    jmp parse_next_1
zendproc

zproc pop_flush_and_emit_vararg
    jsr unstack_operators
    pha

    ldy #1
    lda (parsecmd), y
    jsr queue_bytecode
    
    pla
    jsr queue_bytecode

    jmp parse_next_2
zendproc

zproc parser_skip_whitespace
    zloop
        ldx bufferpos
        lda text_buffer, x
        cmp #' '
        zbreakif_ne

        inc bufferpos
    zendloop
    rts
zendproc

; Fetch an identifier and set p0/p1 to the atom pointer and ID.
zproc parser_fetch_word
    jsr parser_skip_whitespace

    lda bufferpos
    sta bufferend

    ldx bufferend
    lda text_buffer, x
    jsr is_first_letter_of_word
    zif_cs
        zrepeat
            inc bufferend

            ldx bufferend
            lda text_buffer, x
            jsr is_subsequent_letter_of_word
        zuntil_cc

        lda text_buffer, x
        cmp #'#'
        beq 1f
        cmp #'$'
        zif_eq
        1:
            inc bufferend
        zendif
    zendif

    lda bufferpos
    cmp bufferend
    beq 1f

    jsr get_atom

    lda bufferend
    sta bufferpos
    rts
1:
    jmp pop_fail
zendproc

; Used by pop*token. Leaves the buffer position in X.

zproc skip_token
    jsr parser_skip_whitespace

    ldx bufferpos
    dex
    ldy #0 ; 1 - 1
    zrepeat
        inx
        iny

        lda text_buffer, x
        jsr toupper
        eor (parsecmd), y
        and #0x7f
        zif_ne
            jmp pop_fail
        zendif

        lda (parsecmd), y
    zuntil_mi

    inx
    stx bufferpos
    rts
zendproc

zproc pop_token
    jsr skip_token

    ; Check that this is the end of a word.

    lda text_buffer, x
    jsr is_subsequent_letter_of_word
    zif_cs
        jmp pop_fail
    zendif

1:
    iny
    tya
    jsr parse_advance

    jmp parse_state_machine
zendproc

zproc pop_eager_token
    jsr skip_token
    jmp 1b
zendproc

zproc pop_emit
    ldy #1
    lda (parsecmd), y
    jsr queue_bytecode
    jmp parse_next_2
zendproc

zproc pop_emit4
    ldy #1
    lda (parsecmd), y
    jsr queue_bytecode
    
    ldy #3
    zrepeat
        lda #0
        jsr queue_bytecode
        dey
    zuntil_eq
    
    jmp parse_next_2
zendproc
        
zproc pop_word
    jsr parser_fetch_word

    ldy #1
    lda (parsecmd), y
    jsr queue_bytecode

    sta bytecode_buffer, x
    lda p1+0
    jsr queue_bytecode
    lda p1+1
    jsr queue_bytecode

    jmp parse_next_2
zendproc

zproc pop_eol
    jsr parser_skip_whitespace

    ldx bufferpos
    lda text_buffer, x
    zif_ne
        jmp pop_fail
    zendif

    jmp parse_next_1
zendproc

zproc emit_i0
    lda i0+1
    ora i0+2
    ora i0+3
    zif_eq
        ; Top three bytes are zero.

        lda i0+0
        zif_eq
            lda #TOKEN_ZERO
            jmp queue_bytecode
        zendif
        cmp #1
        zif_eq
            lda #TOKEN_ONE
            jmp queue_bytecode
        zendif
    zendif

    lda i0+0
    and i0+1
    and i0+2
    and i0+3
    cmp #0xff
    zif_eq          ; is the number minus one?
        lda #TOKEN_MONE
        jmp queue_bytecode
    zendif

    lda #TOKEN_INTEGER
    jsr queue_bytecode
    lda i0+0
    jsr queue_bytecode
    lda i0+1
    jsr queue_bytecode
    lda i0+2
    jsr queue_bytecode
    lda i0+3
    jsr queue_bytecode
    rts
zendproc

zproc pop_number
    jsr buffer_parse_int
    zif_cs
    1:
        jmp pop_fail
    zendif

    jsr emit_i0
    jmp parse_next_1
zendproc

zproc pop_string
    jsr parser_skip_whitespace
    
    ; Check for the leading double quote.

    ldx bufferpos
    lda text_buffer, x
    cmp #'"'
    bne 1b              ; fail

    ; Find the bounds of the string in the text buffer.

    zrepeat
        inx
        lda text_buffer, x
        zif_eq
            jmp pop_panic
        zendif
        cmp #'"'
    zuntil_eq
    stx bufferend
    
    ; Emit the bytecode.

    lda #TOKEN_STRING
    jsr queue_bytecode

    inc bufferpos
    lda bufferend
    sec
    sbc bufferpos
    jsr queue_bytecode
    
    zloop
        ldx bufferpos
        cpx bufferend
        zbreakif_eq

        lda text_buffer, x
        jsr queue_bytecode

        inc bufferpos
    zendloop

    inc bufferpos               ; skip over trailing double quota
    jmp parse_next_1
zendproc

.macro P_TRY rule
    .byte P_TRY_OP
    .word \rule
.endm

.macro P_JSR rule
    .byte P_JSR_OP
    .word \rule
.endm

.macro P_JMP rule
    .byte P_JMP_OP
    .word \rule
.endm

.macro P_FAIL
    .byte P_FAIL_OP
.endm

.macro P_PANIC
    .byte P_PANIC_OP
.endm

.macro P_RETURN
    .byte P_RETURN_OP
.endm

.macro P_COMMIT
    .byte P_COMMIT_OP
.endm

.macro P_TOKEN bytes:vararg
    .byte P_TOKEN_OP
    .byte \bytes
.endm

.macro P_EAGER_TOKEN bytes:vararg
    .byte P_EAGER_TOKEN_OP
    .byte \bytes
.endm

.macro P_EMIT token
    .byte P_EMIT_OP
    .byte \token
.endm

.macro P_EMIT4 token
    .byte P_EMIT4_OP
    .byte \token
.endm

.macro P_SHUNT token
    .byte P_SHUNT_OP
    .byte \token
.endm

.macro P_FLUSH
    .byte P_FLUSH_OP
.endm

.macro P_WORD token
    .byte P_WORD_OP
    .byte \token
.endm

.macro P_NUMBER
    .byte P_NUMBER_OP
.endm

.macro P_EOL
    .byte P_EOL_OP
.endm

.macro P_PAREN
    .byte P_PAREN_OP
.endm

.macro P_STRING
    .byte P_STRING_OP
.endm

.macro P_PARAM
    .byte P_PARAM_OP
.endm

.macro P_FLUSH_AND_EMIT_VARARG token
    .byte P_FLUSH_AND_EMIT_VARARG_OP
    .byte \token
.endm

.macro keyword name
    keyword_\name = . + 1
    .global keyword_\name
.endm

immediate_statement_rule:
    P_TRY lineentry_rule
    P_TRY list_rule
    P_TRY del_rule
    P_TRY renum_rule
    P_TRY run_rule
    P_TRY con_rule
    P_TRY enter_rule
statement_rule:
    P_TRY repeat_rule
    P_TRY until_rule
    P_TRY while_rule
    P_TRY endwhile_rule
    P_TRY for_rule
    P_TRY endfor_rule
    P_TRY if_rule
    P_TRY else_rule
    P_TRY elif_rule
    P_TRY endif_rule
    P_TRY proc_or_func_rule
    P_TRY endproc_rule
    P_TRY endfunc_rule
    P_TRY eol_rule
simple_statement_rule:
    P_TRY print_rule
    P_TRY input_rule
    P_TRY stop_rule
    P_TRY bye_rule
    P_TRY end_rule
    P_TRY return0_rule
    P_TRY return1_rule
    P_TRY randomize_rule
    P_TRY proccall_rule
    P_TRY assignment_rule
    P_PANIC

lineentry_rule:
    P_NUMBER
    P_COMMIT
    P_EMIT TOKEN_LINEENTRY
    P_JMP statement_rule

list_rule:
keyword list
    P_TOKEN 'L', 'I', 'S', 'T'|0x80
    P_COMMIT
    P_TRY list_to_file_rule
    P_TRY empty_list_rule
    P_JSR line_range_rule
    P_EMIT TOKEN_LIST
    P_EOL
    P_RETURN

list_to_file_rule:
    P_STRING
    P_EMIT TOKEN_LISTTOF
    P_EOL
    P_RETURN

empty_list_rule:
    P_EOL
    P_EMIT TOKEN_ZERO
    P_EMIT TOKEN_ZERO
    P_EMIT TOKEN_LIST
    P_RETURN

del_rule:
    P_TOKEN 'D', 'E', 'L'|0x80
    P_COMMIT
    P_JSR line_range_rule
    P_EMIT TOKEN_DEL
    P_EOL
    P_RETURN

renum_rule:
    P_TOKEN 'R', 'E', 'N', 'U', 'M'|0x80
    P_COMMIT
    P_JSR first_optional_number_or_zero_rule
    P_JSR second_optional_number_or_zero_rule
    P_EOL
    P_EMIT TOKEN_RENUM
    P_RETURN

second_optional_number_or_zero_rule:
    P_TRY comma_then_number_rule
    P_EMIT TOKEN_ZERO
    P_RETURN

first_optional_number_or_zero_rule:
    P_TRY number_rule
    P_EMIT TOKEN_ZERO
    P_RETURN

comma_then_number_rule:
    P_EAGER_TOKEN ','|0x80
number_rule:
    P_NUMBER
    P_RETURN

line_range_rule:
    P_TRY open_left_line_range_rule
    P_TRY single_line_range_rule
    P_NUMBER
    P_EAGER_TOKEN '-'|0x80
    P_TRY eol_line_range_rule
    P_NUMBER
    P_RETURN

single_line_range_rule:
    P_NUMBER
    P_EMIT TOKEN_ZERO
    P_EOL
    P_RETURN

open_left_line_range_rule:
    P_EAGER_TOKEN '-'|0x80
    P_EMIT TOKEN_ZERO
    P_NUMBER
    P_RETURN

eol_line_range_rule:
    P_EOL
    P_EMIT TOKEN_MONE
    P_RETURN

run_rule:
    P_TOKEN 'R', 'U', 'N'|0x80
    P_EMIT TOKEN_RUN
    P_EOL
    P_RETURN

stop_rule:
keyword stop
    P_TOKEN 'S', 'T', 'O', 'P'|0x80
    P_EMIT TOKEN_STOP
    P_EOL
    P_RETURN

end_rule:
keyword end
    P_TOKEN 'E', 'N', 'D'|0x80
    P_EMIT TOKEN_END
    P_EOL
    P_RETURN

bye_rule:
keyword bye
    P_TOKEN 'B', 'Y', 'E'|0x80
    P_EMIT TOKEN_BYE
    P_EOL
    P_RETURN

con_rule:
    P_TOKEN 'C', 'O', 'N'|0x80
    P_EMIT TOKEN_CON
    P_EOL
    P_RETURN

return0_rule:
    P_JSR return_keyword_rule
    P_EMIT TOKEN_RETURN0
    P_EOL
    P_RETURN

return1_rule:
    P_JSR return_keyword_rule
    P_JSR expr_rule
    P_EMIT TOKEN_RETURN1
    P_EMIT 0
    P_EOL
    P_RETURN

randomize_rule:
keyword randomize
    P_TOKEN 'R', 'A', 'N', 'D', 'O', 'M', 'I', 'Z', 'E'|0x80
    P_SHUNT TOKEN_RANDZ
    P_JSR expr_rule
    P_EOL
    P_RETURN

return_keyword_rule:
keyword return
    P_TOKEN 'R', 'E', 'T', 'U', 'R', 'N'|0x80
    P_RETURN

enter_rule:
keyword enter
    P_TOKEN 'E', 'N', 'T', 'E', 'R'|0x80
    P_COMMIT
    P_SHUNT TOKEN_ENTER
    P_JSR expr_rule
    P_EOL
    P_RETURN

repeat_rule:
keyword repeat
    P_TOKEN 'R', 'E', 'P', 'E', 'A', 'T'|0x80
    P_EMIT TOKEN_REPEAT
    P_EOL
    P_RETURN

until_rule:
keyword until
    P_TOKEN 'U', 'N', 'T', 'I', 'L'|0x80
    P_COMMIT
    P_JSR expr_rule
    P_FLUSH
    P_EMIT4 TOKEN_UNTIL
    P_EOL
    P_RETURN

while_rule:
keyword while
    P_TOKEN 'W', 'H', 'I', 'L', 'E'|0x80
    P_COMMIT
    P_JSR expr_rule
    P_FLUSH
    P_EMIT4 TOKEN_WHILE
    P_JSR optional_do_rule
    P_TRY eol_rule
    P_COMMIT
    P_JSR simple_statement_rule
    P_FLUSH
    P_JMP 1f

endwhile_rule:
keyword endwhile
    P_TOKEN 'E', 'N', 'D', 'W', 'H', 'I', 'L', 'E'|0x80
1:
    P_EMIT4 TOKEN_ENDWHILE
    P_EOL
    P_RETURN

for_rule:
keyword for
    P_TOKEN 'F', 'O', 'R'|0x80
    P_WORD TOKEN_VARREFRW
    P_JSR assignment_operator_rule
    P_JSR expr_rule
keyword to
    P_TOKEN 'T', 'O'|0x80
    P_JSR expr_rule
    P_JSR optional_step_rule
    P_FLUSH
    P_EMIT4 TOKEN_FOR
    P_JSR optional_do_rule
    P_TRY eol_rule
    P_COMMIT
    P_JSR simple_statement_rule
    P_FLUSH
    P_JMP 1f

endfor_rule:
    P_JSR endfor_or_next_rule
1:
    P_EMIT4 TOKEN_ENDFOR
    P_RETURN

endfor_or_next_rule:
    P_TRY endfor_token_rule
    P_TOKEN 'N', 'E', 'X', 'T'|0x80
    P_RETURN

endfor_token_rule:
keyword endfor
    P_TOKEN 'E', 'N', 'D', 'F', 'O', 'R'|0x80
    P_RETURN

optional_step_rule:
    P_TRY step_rule
    P_FLUSH
    P_EMIT TOKEN_ONE
    P_RETURN

step_rule:
keyword step
    P_TOKEN 'S', 'T', 'E', 'P'|0x80
    P_JMP expr_rule

if_rule:
keyword if
    P_TOKEN 'I', 'F'|0x80
    P_COMMIT
    P_JSR expr_rule
    P_FLUSH
    P_EMIT4 TOKEN_IF
    P_JSR optional_then_rule
    P_TRY eol_rule
    P_COMMIT
    P_JSR simple_statement_rule
    P_FLUSH
    P_JMP 1f

endif_rule:
keyword endif
    P_TOKEN 'E', 'N', 'D', 'I', 'F'|0x80
1:
    P_EMIT TOKEN_ENDIF
    P_EOL
    P_RETURN

else_rule:
keyword else
    P_TOKEN 'E', 'L', 'S', 'E'|0x80
    P_EMIT4 TOKEN_ELSE
    P_JMP eol_rule

elif_rule:
keyword elif
    P_TOKEN 'E', 'L', 'I', 'F'|0x80
    P_COMMIT
    P_EMIT4 TOKEN_ELIFGOTO
    P_JSR expr_rule
    P_FLUSH
    P_EMIT4 TOKEN_ELIF
    P_JSR optional_then_rule
    P_JMP eol_rule

optional_then_rule:
    P_TRY then_rule
    P_RETURN

then_rule:
keyword then
    P_TOKEN 'T', 'H', 'E', 'N'|0x80
    P_RETURN

optional_do_rule:
    P_TRY do_rule
    P_RETURN

do_rule:
keyword do
    P_TOKEN 'D', 'O'|0x80
    P_RETURN

proc_or_func_rule:
    P_JSR proc_or_func_keyword_rule
    P_PAREN
    P_JSR optional_parameter_list_rule
    P_FLUSH_AND_EMIT_VARARG TOKEN_PROC2
    P_JSR optional_closed_rule
    P_EOL
    P_RETURN

optional_parameter_list_rule:
    P_EAGER_TOKEN '('|0x80
    P_COMMIT
    P_TRY end_parameter_list_rule
1:
    P_COMMIT
    P_JSR parameter_rule
    P_PARAM
    P_TRY end_parameter_list_rule
    P_EAGER_TOKEN ','|0x80
    P_JMP 1b

parameter_rule:
    P_TRY ref_parameter_rule
    P_WORD TOKEN_SETPARAM
    P_RETURN

ref_parameter_rule:
keyword ref
    P_TOKEN 'R', 'E', 'F'|0x80
    P_WORD TOKEN_REFPARAM
    P_RETURN

end_parameter_list_rule:
    P_EAGER_TOKEN ')'|0x80
    P_RETURN

optional_closed_rule:
    P_TRY eol_rule
keyword closed
    P_TOKEN 'C', 'L', 'O', 'S', 'E', 'D'|0x80
    P_EMIT TOKEN_CLOSED
    P_RETURN

proc_or_func_keyword_rule:
    P_TRY proc_keyword_rule
keyword func
    P_TOKEN 'F', 'U', 'N', 'C'|0x80
    P_WORD TOKEN_FUNC1
    P_RETURN

proc_keyword_rule:
keyword proc
    P_TOKEN 'P', 'R', 'O', 'C'|0x80
    P_WORD TOKEN_PROC1
    P_RETURN

endproc_rule:
keyword endproc
    P_TOKEN 'E', 'N', 'D', 'P', 'R', 'O', 'C'|0x80
    P_EMIT TOKEN_ENDPROC
    P_EOL
    P_RETURN

endfunc_rule:
keyword endfunc
    P_TOKEN 'E', 'N', 'D', 'F', 'U', 'N', 'C'|0x80
    P_EMIT TOKEN_ENDFUNC
    P_EOL
    P_RETURN

proccall_rule:
    P_WORD TOKEN_VARREFRO
    P_PAREN
    P_PARAM ; counts the procedure name
    P_JSR argument_list_rule
    P_FLUSH_AND_EMIT_VARARG TOKEN_APPLYVOID
    P_EOL
    P_RETURN

argument_list_rule:
    P_EAGER_TOKEN '('|0x80
    P_TRY end_argument_list_rule
1:
    P_PAREN
    P_JSR expr_or_range_rule
    P_FLUSH
    P_PARAM
    P_TRY end_argument_list_rule
    P_EAGER_TOKEN ','|0x80
    P_JMP 1b

single_argument_list_rule:
    P_PAREN
    P_JSR expr_or_range_rule
    P_FLUSH
end_argument_list_rule:
    P_EAGER_TOKEN ')'|0x80
    P_RETURN
    
expr_or_range_rule:
    P_TRY range_rule
    P_JMP expr_rule

assignment_rule:
    P_JSR assignment_kind_rule
    P_COMMIT
    P_JSR expr_rule
    P_TRY eol_rule
    P_FLUSH
    P_EAGER_TOKEN ';'|0x80
    P_JMP assignment_rule

assignment_kind_rule:
    P_TRY simple_assignment_kind_rule
    P_TRY slice_assignment_kind_rule
    P_FAIL

simple_assignment_kind_rule:
    P_WORD TOKEN_VARREFRW
    P_SHUNT TOKEN_ASSIGN
    P_JMP assignment_operator_rule

slice_assignment_kind_rule:
    P_WORD TOKEN_VARREFRW
    P_EAGER_TOKEN '('|0x80
    P_PAREN
    P_JSR range_rule
    P_EAGER_TOKEN ')'|0x80
    P_FLUSH
    P_SHUNT TOKEN_SLASSIGN
    P_JMP assignment_operator_rule

assignment_operator_rule:
    P_TRY equals_rule
keyword assign
    P_EAGER_TOKEN ':', '='|0x80
    P_RETURN

equals_rule:
keyword equals
    P_EAGER_TOKEN '='|0x80
    P_RETURN

eol_rule:
    P_EOL
    P_RETURN

print_rule:
keyword print
    P_TOKEN 'P', 'R', 'I', 'N', 'T'|0x80
    P_COMMIT
    P_TRY print_no_terminator_rule
    P_SHUNT TOKEN_PRINT
    P_JSR expr_rule
print_expr_list_rule:
    P_TRY print_no_terminator_rule
    P_TRY print_terminator_rule
    P_JSR print_separator_rule
    P_COMMIT
    P_SHUNT TOKEN_PRINT
    P_JSR expr_rule
    P_JMP print_expr_list_rule

print_terminator_rule:
    P_JSR print_separator_rule
    P_EOL
    P_RETURN

comma_rule:
keyword comma
    P_EAGER_TOKEN ','|0x80
    P_RETURN

print_separator_rule:
    P_TRY comma_rule
semicolon_rule:
keyword semicolon
    P_EAGER_TOKEN ';'|0x80
    P_RETURN

print_no_terminator_rule:
    P_EOL
    P_SHUNT TOKEN_PRINTNL
    P_RETURN

optional_expr_rule:
    P_TRY expr_rule
    P_RETURN

compulsory_expr_rule:
    P_TRY expr_rule
    P_PANIC

input_rule:
keyword input
    P_TOKEN 'I', 'N', 'P', 'U', 'T'|0x80
    P_JSR optional_input_prompt_rule
    P_COMMIT
    P_WORD TOKEN_VARREFRW
    P_EMIT TOKEN_INPUT
    P_COMMIT
    P_TRY print_no_terminator_rule
    P_JSR print_separator_rule
    P_EOL
    P_RETURN

optional_input_prompt_rule:
    P_TRY input_prompt_rule
    P_EMIT TOKEN_DEFPROMPT
    P_RETURN

input_prompt_rule:
    P_STRING
    P_EAGER_TOKEN ':'|0x80
    P_EMIT TOKEN_STRPROMPT
    P_RETURN

expr_rule:
    P_JSR leaf_rule
    P_COMMIT
    P_TRY application_rule
    P_TRY addition_rule
    P_TRY subtraction_rule
    P_TRY multiplication_rule
    P_TRY division_rule
    P_TRY modulus_rule
    P_TRY compare_eq_rule           ; =
    P_TRY compare_noteq_rule        ; <>
    P_TRY compare_lte_rule          ; <=
    P_TRY compare_lt_rule           ; <
    P_TRY compare_gte_rule          ; >=
    P_TRY compare_gt_rule           ; >
    P_RETURN

application_rule:
    P_PAREN
    P_PARAM ; counts the thing being dereferenced
    P_JSR argument_list_rule
    P_FLUSH_AND_EMIT_VARARG TOKEN_APPLY
    P_RETURN

range_rule:
    P_JSR expr_rule
    P_TRY range_rhs_rule
    P_RETURN

range_rhs_rule:
keyword colon
    P_EAGER_TOKEN ':'|0x80
    P_SHUNT TOKEN_MKRANGE
    P_JSR expr_rule
    P_RETURN

addition_rule:
keyword add
    P_EAGER_TOKEN '+'|0x80
    P_SHUNT TOKEN_ADD
    P_JMP compulsory_expr_rule

subtraction_rule:
keyword sub
    P_EAGER_TOKEN '-'|0x80
    P_SHUNT TOKEN_SUB
    P_JMP compulsory_expr_rule

multiplication_rule:
keyword mul
    P_EAGER_TOKEN '*'|0x80
    P_SHUNT TOKEN_MUL
    P_JMP compulsory_expr_rule

division_rule:
keyword div
    P_EAGER_TOKEN '/'|0x80
    P_SHUNT TOKEN_DIV
    P_JMP compulsory_expr_rule

modulus_rule:
keyword mod
    P_TOKEN 'M', 'O', 'D'|0x80
    P_SHUNT TOKEN_MOD
    P_JMP compulsory_expr_rule

compare_eq_rule:
keyword eq
    P_EAGER_TOKEN '='|0x80
    P_SHUNT TOKEN_EQ
    P_JMP compulsory_expr_rule

compare_noteq_rule:
keyword noteq
    P_EAGER_TOKEN '<', '>'|0x80
    P_SHUNT TOKEN_NOTEQ
    P_JMP compulsory_expr_rule

compare_lt_rule:
keyword lt
    P_EAGER_TOKEN '<'|0x80
    P_SHUNT TOKEN_LT
    P_JMP compulsory_expr_rule

compare_lte_rule:
keyword lte
    P_EAGER_TOKEN '<', '='|0x80
    P_SHUNT TOKEN_LTE
    P_JMP compulsory_expr_rule

compare_gt_rule:
keyword gt
    P_EAGER_TOKEN '>'|0x80
    P_SHUNT TOKEN_GT
    P_JMP compulsory_expr_rule

compare_gte_rule:
keyword gte
    P_EAGER_TOKEN '>', '='|0x80
    P_SHUNT TOKEN_GTE
    P_JMP compulsory_expr_rule

leaf_rule:
    P_TRY parenthesised_rvalue_rule
    P_TRY not_rvalue_rule
    P_TRY sgn_rvalue_rule
    P_TRY len_rvalue_rule
    P_TRY str$_rvalue_rule
    P_TRY rnd_rvalue_rule
    P_TRY free_rvalue_rule
    P_TRY true_rvalue_rule
    P_TRY false_rvalue_rule
    P_TRY varref_rvalue_rule
    P_TRY number_rvalue_rule
    P_TRY negate_rvalue_rule
    P_TRY string_rvalue_rule
    P_FAIL

parenthesised_rvalue_rule:
    P_EAGER_TOKEN '('|0x80
    P_COMMIT
    P_PAREN
    P_JSR expr_rule
    P_EAGER_TOKEN ')'|0x80
    P_FLUSH
    P_RETURN

varref_rvalue_rule:
    P_WORD TOKEN_VARREFRO
    P_RETURN

number_rvalue_rule:
    P_NUMBER
    P_RETURN

string_rvalue_rule:
    P_STRING
    P_RETURN

negate_rvalue_rule:
keyword neg
    P_EAGER_TOKEN '-'|0x80
    P_COMMIT
    P_SHUNT TOKEN_NEG
    P_JMP leaf_rule

not_rvalue_rule:
keyword not
    P_TOKEN 'N', 'O', 'T'|0x80
    P_COMMIT
    P_SHUNT TOKEN_NOT
    P_JMP leaf_rule

sgn_rvalue_rule:
keyword sgn
    P_TOKEN 'S', 'G', 'N'|0x80
    P_COMMIT
    P_SHUNT TOKEN_SGN
    P_JMP leaf_rule

len_rvalue_rule:
keyword len
    P_TOKEN 'L', 'E', 'N'|0x80
    P_COMMIT
    P_SHUNT TOKEN_LEN
    P_JMP leaf_rule

str$_rvalue_rule:
keyword str$
    P_TOKEN 'S', 'T', 'R', '$'|0x80
    P_COMMIT
    P_SHUNT TOKEN_STR$
    P_JMP leaf_rule

rnd_rvalue_rule:
keyword rnd
    P_TOKEN 'R', 'N', 'D'|0x80
    P_COMMIT
    P_PAREN
    P_JSR argument_list_rule
    P_FLUSH_AND_EMIT_VARARG TOKEN_RND
    P_RETURN

free_rvalue_rule:
keyword free
    P_TOKEN 'F', 'R', 'E', 'E'|0x80
    P_COMMIT
    P_EMIT TOKEN_FREE
    P_RETURN

true_rvalue_rule:
keyword true
    P_TOKEN 'T', 'R', 'U', 'E'|0x80
    P_COMMIT
    P_EMIT TOKEN_TRUE
    P_RETURN

false_rvalue_rule:
keyword false
    P_TOKEN 'F', 'A', 'L', 'S', 'E'|0x80
    P_COMMIT
    P_EMIT TOKEN_FALSE
    P_RETURN

.global keyword_zero
keyword_zero:
    .byte '0'|0x80
.global keyword_mone
keyword_mone:
    .byte '-'
.global keyword_one
keyword_one:
    .byte '1'|0x80

; vim: ts=4 sw=4 et:
