.include "src/zif.inc"
.include "src/_globals.inc"

zproc main
    jmp start_interpreter
zendproc

zproc platform_get_progtop
    brk
zendproc

zproc platform_startup_hook
    brk
zendproc

zproc platform_quit
    brk
zendproc

zproc platform_read_line
    brk
zendproc

zproc platform_putchar
    brk
zendproc

zproc platform_openin
    brk
zendproc

zproc platform_openout
    brk
zendproc

zproc platform_openup
    brk
zendproc

zproc platform_close
    brk
zendproc

zproc platform_bget
    brk
zendproc

zproc platform_bput
    brk
zendproc
