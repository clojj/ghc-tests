Loopification
=============

Work on loopification during my MSR internship

## Factorial

Tail-recursive factorial. Used for testing Cmm generation.

## Sum of squares using Stream Fusion

Used for testing Cmm generation. Taken from "Low-level code optimizations in
Glasgow Haskell Compiler".

## T783

Fails validation with my patch. No difference in generated assembly:

```
# diff -y --suppress-common-lines T783-master.s T783-loopify.s
.file   "/tmp/ghc35391_0/ghc35391_0.bc" |.file   "/tmp/ghc35422_0/ghc35422_0.bc"
# BB#0:                         # %c2Xh | # BB#0:                       # %c2Xj
# BB#0:                         # %c3Eg | # BB#0:                       # %c3Mq
# BB#0:                         # %c6wa | # BB#0:                       # %c6wl
```

Compiled with `ghc -O2 -fllvm -optlo-O3 -fforce-recomp -keep-s-file`

## T1969

Fails validation with my patch. No difference in generated assembly compared to
master branch (except for 606 label comments - see T1969.diff).

## T3064

Fails validation with my patch. No difference in generated assembly
(except for 51 label comments - see T3064.diff).
