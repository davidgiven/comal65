.include "src/_globals.inc"

.section .zp, "zax", @nobits

; contiguous
p0:        .word 0
p1:        .word 0
p2:        .word 0
p3:        .word 0
p4:        .word 0

; contiguous
v0:        .byte 0
i0:        .word 0, 0 ; also, TOS when executing
f0:        .byte 0 ; floating point workspace

; contiguous
v1:        .byte 0
i1:        .word 0, 0
f1:        .byte 0 ; floating point workspace

; contiguous
v2:        .byte 0
i2:        .word 0, 0
f2:        .byte 0 ; floating point workspace

