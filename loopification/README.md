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
If one of these lines is commented out then compilation succeeds.
