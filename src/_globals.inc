; General purpose 2-byte temporaries.

.global p0, p1, p2, p3, p4
.zeropage p0, p1, p2, p3, p4

; The three 'registers' used by the interpreter.

.global v0, v1, v2
.global i0, i1, i2
.global f0, f1, f2
.zeropage v0, v1, v2
.zeropage i0, i1, i2
.zeropage f0, f1, f2

; The 256-byte general purpose text buffer.

.global text_buffer
.global bufferpos, bufferend
.zeropage bufferpos, bufferend

; Used by the parser and lister. 256 bytes long.

.global operator_stack
.global opstackptr
.zeropage opstackptr

; Misc tables.

.global token_precedence_table

; Errors.

.global error_string_overflow
.global error_invalid_parameter

; Current execution address.

.global execlineptr, execbyteptr
.zeropage execlineptr, execbyteptr

; Tokens.

TOKEN_EOL        = 0x00 ; end of line
TOKEN_VARREFRW   = 0x01 ; +2 bytes for the ID
TOKEN_INTEGER    = 0x02 ; +4 bytes for the number
TOKEN_MUL        = 0x03
TOKEN_DIV        = 0x04
TOKEN_ADD        = 0x05
TOKEN_SUB        = 0x06
TOKEN_MOD        = 0x07
TOKEN_PRINT      = 0x08
TOKEN_PRINTNL    = 0x09
TOKEN_ASSIGN     = 0x0a
TOKEN_NEG        = 0x0b
TOKEN_LINEENTRY  = 0x0c
TOKEN_ZERO       = 0x0d
TOKEN_LIST       = 0x0e
TOKEN_DEL        = 0x0f
TOKEN_MONE       = 0x10
TOKEN_RENUM      = 0x11
TOKEN_RUN        = 0x12
TOKEN_STOP       = 0x13
TOKEN_CON        = 0x14
TOKEN_REPEAT     = 0x15
TOKEN_UNTIL      = 0x16
TOKEN_NOT        = 0x17
TOKEN_EQ         = 0x18
TOKEN_WHILE      = 0x19
TOKEN_ENDWHILE   = 0x1a
TOKEN_ONE        = 0x1b
TOKEN_FOR        = 0x1c
TOKEN_ENDFOR     = 0x1d
TOKEN_SGN        = 0x1e
TOKEN_NOTEQ      = 0x1f
TOKEN_LT         = 0x20
TOKEN_LTE        = 0x21
TOKEN_GT         = 0x22
TOKEN_GTE        = 0x23
TOKEN_IF         = 0x24
TOKEN_ELSE       = 0x25
TOKEN_ENDIF      = 0x26
TOKEN_ELIF       = 0x27
TOKEN_ELIFGOTO   = 0x28
TOKEN_STRING     = 0x29
TOKEN_FREE       = 0x2a
TOKEN_TRUE       = 0x2b
TOKEN_FALSE      = 0x2c
TOKEN_LEN        = 0x2d
TOKEN_CLOSED     = 0x2e
TOKEN_MKRANGE    = 0x2f
TOKEN_SLASSIGN   = 0x30
TOKEN_ENTER      = 0x31
TOKEN_STR$       = 0x32
TOKEN_BYE        = 0x33
TOKEN_PROC1      = 0x34
TOKEN_ENDPROC    = 0x35
TOKEN_APPLY      = 0x36
TOKEN_PROC2      = 0x37
TOKEN_END        = 0x38
TOKEN_RETURN0    = 0x39
TOKEN_SETPARAM   = 0x3a ; +2 bytes for the ID
TOKEN_VARREFRO   = 0x3b ; +2 bytes for the ID
TOKEN_REFPARAM   = 0x3c ; +2 bytes for the ID
TOKEN_FUNC1      = 0x3d ; +2 bytes for the ID
TOKEN_ENDFUNC    = 0x3e
TOKEN_RETURN1    = 0x3f
TOKEN_APPLYVOID  = 0x40
TOKEN_INPUT      = 0x41
TOKEN_DEFPROMPT  = 0x42
TOKEN_STRPROMPT  = 0x43
TOKEN_RND        = 0x44
TOKEN_RANDZ      = 0x45
TOKEN_LISTTOF    = 0x46

; Data structure definitions.

OBJECT_SIZE     = 0
OBJECT_TYPE     = 2
OBJECT_DATA     = 3

OTYPE_FREE       = 0x00
OTYPE_ATOM       = 0x01
OTYPE_INTEGER    = 0x02
OTYPE_STRINGDATA = 0x03
OTYPE_RANGE      = 0x04
OTYPE_PROCEDURE  = 0x05
OTYPE_CALL       = 0x06
OTYPE_STRING     = 0x10
OTYPE_STRINGLIT  = 0x11
OTYPE_VAR        = 0x20 ; pointer to a hashnode
OTYPE_REF        = 0x21 ; pointer to a hashnode

HASH_SLOTS      = 16

.macro startstruct init
    .set p, \init
.endm

.macro structmember name, size
    \name = p
    .set p, p + (\size)
.endm

.macro endstruct name
    .set \name, p
.endm

; Atom structure:
;  0..1 - object size
;  2    - object type
;  3    - atom type (OTYPE)
;  4... - text

ATOM_TYPE       = OBJECT_DATA + 0
ATOM_DATA       = OBJECT_DATA + 1

; Hash node structure:
;  0..1 - object size
;  2    - object type
;  3..6 - data
;  7..8 - key
;  9..a - object ID of next node

HASHNODE_KEY    = 7
HASHNODE_NEXT   = 9
HASHNODE_SIZE   = 0xb

; String structure:
;  0..1 - object size
;  2    - object type
;  3... - string data

STRING_DATA     = OBJECT_DATA + 0

; Stack value structure:
;  0    - object type
;  1..4 - data

startstruct 0
structmember STACKVALUE_TYPE, 1
structmember STACKVALUE_DATA, 4
endstruct STACKVALUE_SIZE

; Variable stack value structure:

startstruct STACKVALUE_DATA
structmember STACKVALUE_VARREF_HASHNODE_ID, 2
structmember STACKVALUE_VARREF_TYPE, 1
endstruct STACKVALUE_VARREF__SIZE

; String stack value structure:

startstruct STACKVALUE_DATA
structmember STACKVALUE_STRING_ID, 2
structmember STACKVALUE_STRING_OFFSET, 1
structmember STACKVALUE_STRING_LENGTH, 1
endstruct STACKVALUE_STRING__SIZE

; Range stack value structure:

startstruct STACKVALUE_DATA
structmember STACKVALUE_RANGE_OFFSET, 2
structmember STACKVALUE_RANGE_LENGTH, 2
endstruct STACKVALUE_RANGE__SIZE

; Procedure stack value structure:

startstruct STACKVALUE_DATA
structmember STACKVALUE_PROC_ADDRESS, 3 ; 3-byte bytecode address
structmember STACKVALUE_PROC_PCOUNT, 1
endstruct STACKVALUE_PROC__SIZE

; Special non-standard stack object representing a procedure invocation.

startstruct STACKVALUE_DATA
structmember STACKVALUE_CALL_CPUSTATE, 6 ; cpustate__end - cpustate__start
structmember STACKVALUE_CALL_PCOUNT, 1
structmember STACKVALUE_CALL_NEWHASHTABLE, HASH_SLOTS*2
endstruct STACKVALUE_CALL__SIZE

; Program storage: a flat list of lines, each of which looks like:
;   0    - line size
;   1..2 - line number
;   3    - indentation
;   4... - data
; Lines should end in a TOKEN_EOL command (which may take a parameter).

LINE_SIZE       = 0
LINE_NUMBER     = 1
LINE_INDENT     = 3
LINE_DATA       = 4

; Useful macros.

.macro copystruct n, src, dest
    ldx #(\n)-1
    zrepeat
        lda \src, x
        sta \dest, x
        dex
    zuntil_mi
.endm

.macro copy16 src, dest
    lda \src+0
    sta \dest+0
    lda \src+1
    sta \dest+1
.endm

