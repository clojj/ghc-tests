Bool primops (#6135)
====================

Various programs used for testing my work on [#6135](http://ghc.haskell.org/trac/ghc/ticket/6135).

bool-prim-ops.hs
================

With my patch function

```haskel
f :: Int# -> Int# -> Int# -> Int# -> Int
f x y width height =
    case (x <$# 0#) `orI#` (x >=$# width) `orI#` (y <$# 0#) `orI#` (y >=$# height) of
      1# -> 1
      0# -> 0
```

should produce a nice branchless assembly:

```gas
# BB#0:                                 # %c1oe
  movq   %rsi, %rax
  orq    %r14, %rax
  shrq   $63, %rax
  cmpq   %rdi, %r14
  setge  %cl
  movzbl %cl, %ecx
  orq    %rax, %rcx
  cmpq   %r8, %rsi
  setge  %al
  movzbl %al, %eax
  orq    %rcx, %rax
  jne    .LBB2_1
```

constant-folds.hs
=================

Constant folding for Integer

repa-fft.hs
===========

Modified FFT using new primops. No performance difference compared to unmodified version.
