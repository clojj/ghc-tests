Bool primops (#6135)
====================

Program used for testing my work on [#6135](http://ghc.haskell.org/trac/ghc/ticket/6135). With my patch function `f` should produce a nice branchless assembly:

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
