Slim Compiler
=============

Slim Compiler serves as an infrastructure for studying compiler
construction and programming language theory. The first reference
structure of this project is taken from Tiger Book. It'll
diverge eventually.

TopLevel
--------

You can specify an action sequence to manipulate the loaded
program. For example, you can do the following to print the AST and
then print the IR.

./main.byte -load ./tests/samples/test1.tig -ast -p -ir -p

Dependencies
------------
Dependencies are managed via opam. Do

```
opam pin add .
```

To install dependencies.

Trouble Shooting
----------------

1. On my GNU/Linux laptop, compiling runtime.c with `-m32` complains
   `fatal error: sys/cdefs.h: No such file or directory`

```
apt-get install gcc-multilib
```
