Comal-65
========

What?
-----

This is a brand new implementation of an interpreter for the Comal dialect of
BASIC to the 6502.

It's about half complete. The core language is implemented, although with
integer maths only, and missing some constructs (CASE...WHEN...ENDCASE is the
major one). The only file support is `ENTER <string>` and `LIST <string>` for
reading and writing programs as text files. (I haven't done `LOAD` or `SAVE` yet
as the bytecode format isn't complete.) Also you can't enter comments!

Programs are compiled into bytecode on the fly, and decompiled on the fly when
you list them.

Currently this works on the BBC Micro Tube system, simply because it was easy
and makes a good testbed. There's a simple emulator supplied which is used for
running unit tests.


Why?
----

Because.


Where?
------

It's [open source on GitHub!](https://github.com/davidgiven/comal65)


How?
----

You will need the [llvm-mos](https://llvm-mos.org) toolchain. CP/M-65 support
is available out of the box. Once installed, you should just be able to run the
Makefile and you'll get bootable disk images for the Commodore 64 (with 1541
drive) and BBC Micro (producing a 200kB SSSD DFS disk):

    make LLVM=<your llvm-mos bin directory here>/

### BBC Micro notes

  - To run, mount the `bbctube.ssd` disk image into a system with a second
  processor, and do SHIFT+BREAK.
  
  - You can also do `*!boot <filename>` to start the interpreter and load the
  supplied text file.
  
  - Do CTRL+BREAK to quit.
  

Who?
----

You may contact me at dg@cowlark.com, or visit my website at
http://www.cowlark.com.  There may or may not be anything interesting there.
The Comal-65 project was designed and written by me, David Given. 


License
-------

Everything here so far _except_ the contents of the `third_party` directory is
© 2022-2023 David Given, and is licensed under the two-clause BSD open source
license. Please see [LICENSE](LICENSE) for the full text. The tl;dr is: you can
do what you like with it provided you don't claim you wrote it.

The exceptions are the contents of the `third_party` directory, which were
written by other people and are not covered by this license. This directory as
a whole contains GPL software, which means that if you redistribute the entire
directory, you must conform to the terms of the GPL.

`third_party/lib6502` contains a hacked copy of the lib6502 library, which is ©
2005 Ian Plumarta and is available under the terms of the MIT license. See
`third_party/lib6502/COPYING.lib6502` for the full text.

