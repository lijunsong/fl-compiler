#!/bin/bash

# tc will call tiger to generate assembly, use gas generate object
# file and link with runtime

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
tig=$1

$dir/tigerc.byte -load $tig -codegen -p > /tmp/tigtmp.s
$(cd /tmp && cc -w -m32 -g tigtmp.s $dir/runtime/runtime.c && mv a.out $dir)
