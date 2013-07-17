Various backend tests
=====================

## Heap checks (heap-checks.hs)

Program for testing heap checks in tail-recursive functions.
See [BackEndNotes on GHC Wiki](http://ghc.haskell.org/trac/ghc/wiki/BackEndNotes#Heapstackchecks)
and [#1498](http://ghc.haskell.org/trac/ghc/ticket/1498). Worker function is
compiled to this Cmm code:

```
{offset
  co0:
      _snf::I32 = I32[Sp];
      if (_snf::I32 != 0) goto co4; else goto co5;
  co4:
      I32[Sp] = _snf::I32 - 1;
      call HeapChecks.$wf_info() args: 12, res: 0, upd: 4;
  co5:
      R1 = P32[Sp + 4];
      Sp = Sp + 8;
      call (P32[Sp])(R1) args: 4, res: 0, upd: 4;
}
```
Wrapper function is compiled to:
```
{offset
  cou:
      if ((Sp + -4) < SpLim) goto coF; else goto coH;
  coF:
      R1 = HeapChecks.f_closure;
      call (stg_gc_fun)(R1) args: 12, res: 0, upd: 4;
  coH:
      _snk::P32 = P32[Sp];
      I32[Sp] = cov;
      R1 = _snk::P32;
      if (R1 & 3 != 0) goto cov; else goto coI;
  coI:
      call (I32[R1])(R1) returns to cov, args: 4, res: 4, upd: 4;
  cov:
      _sno::P32 = P32[Sp + 4];
      I32[Sp + 4] = coz;
      I32[Sp - 4] = I32[R1 + 3];
      P32[Sp] = _sno::P32;
      Sp = Sp - 4;
      call HeapChecks.$wf_info() returns to coz, args: 12, res: 4, upd: 4;
  coz:
      Hp = Hp + 8;
      _snr::P32 = R1;
      if (Hp > HpLim) goto coO; else goto coQ;
  coO:
      HpAlloc = 8;
      R1 = _snr::P32;
      call stg_gc_unpt_r1(R1) returns to coz, args: 4, res: 4, upd: 4;
  coQ:
      I32[Hp - 4] = Data.Maybe.Just_con_info;
      P32[Hp] = _snr::P32;
      R1 = Hp - 2;
      Sp = Sp + 4;
      call (P32[Sp])(R1) args: 4, res: 0, upd: 4;
}
```
