Loopification & Copy Propagation
================================

Work on loopification during my MSR internship

## Factorial (fact.hs)

Tail-recursive factorial. Used for testing Cmm generation.

## Sum of squares using Stream Fusion (sumSqrFusion.hs)

Used for testing Cmm generation. Taken from "Low-level code optimizations
in Glasgow Haskell Compiler".

## Register spilling (reg-spill.hs)

Demonstrates parameter spilling to the stack. Requires 32bit architecture
to achieve this results, because on i386 floating point params are passed
on the stack, not in the register.

## Infinite loop in hand-written Cmm (infinite-loop.cmm)

My copy-propagation pass goes into an infinite loop when compiling this
hand-written Cmm file with `-O2`. These are the two key lines:

```
(h) = ccall cas(mv + SIZEOF_StgHeader + OFFSET_StgMutVar_var, x, y);
if (x != x) { goto retry; }
```
which are urned into following low-level Cmm code:
```
ck: _cq::I32 = I32[_c1::P32 + 4 + 0];
    I32[_co::I32 + 4 + 4 + 1 * 4] = _cq::I32;
    _cr::I32 = cas;
    _cs::P32 = _c1::P32 + 4 + 0;
    _ct::I32 = _cq::I32;
    _cu::I32 = _cp::I32;
    (_cv::I32) = call "ccall" arg hints:  [,
                                           ,]  result hints:  [] (_cr::I32)(_cs::P32, _ct::I32, _cu::I32);
    if (_cq::I32 != _cq::I32) goto ck; else goto cn;
cn: R1 = _cj::I32;
    call (P32[(old + 4)])(R1) args: 4, res: 0, upd: 4;
```

If one of these lines is commented out then compilation succeeds.

This code needs to be compiled with

```
"inplace/bin/ghc-stage1" -static -optc-DTHREADED_RTS  -H64m -O0 -fasm -Iincludes -Iincludes/dist -Iincludes/dist-derivedconstants/header -Iincludes/dist-ghcconstants/header -Irts -Irts/dist/build -DCOMPILING_RTS -package-name rts -dcmm-lint      -i -irts -irts/dist/build -irts/dist/build/autogen -Irts/dist/build -Irts/dist/build/autogen           -O1    -c /home/t-jastol/tests/loopification/infite-loop.cmm -ddump-cmm > /home/t-jastol/tests/loopification/infite-loop_dump/infinite-loop-dump-noif.cmm

```
