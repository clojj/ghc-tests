{-# LANGUAGE BangPatterns, MagicHash, CPP #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
module HashStr where

import Foreign.C
import GHC.Exts
import Data.Word

-- Extracted from utils/FastString, according to comment in CmmSink
-- this shoul demonstrate that problem:
--
--  s2ay:
--      if ((_s2an::I64 == _s2ao::I64) >= 1) goto c2gn; else goto c2gp;
--  c2gn:
--      R1 = _s2au::I64;
--      call (I64[Sp])(R1) args: 8, res: 0, upd: 8;
--  c2gp:
--      _s2cO::I64 = %MO_S_Rem_W64(%MO_UU_Conv_W8_W64(I8[_s2aq::I64 + (_s2an::I64 << 0)]) + _s2au::I64 * 128,
--                                 4091);
--      _s2an::I64 = _s2an::I64 + 1;
--      _s2au::I64 = _s2cO::I64;
--      goto s2ay;
--
-- a nice loop, but we didn't eliminate the silly assignment at the end.
-- See Note [dependent assignments], which would probably fix this.

#define hASH_TBL_SIZE          4091


hashStr  :: Ptr Word8 -> Int -> Int
 -- use the Addr to produce a hash value between 0 & m (inclusive)
hashStr (Ptr a#) (I# len#) = loop 0# 0#
   where
    loop h n | n GHC.Exts.==# len# = I# h
             | otherwise  = loop h2 (n GHC.Exts.+# 1#)
          where !c = ord# (indexCharOffAddr# a# n)
                !h2 = (c GHC.Exts.+# (h GHC.Exts.*# 128#)) `remInt#`
                      hASH_TBL_SIZE#

{-


{offset
  cut:
      goto cux;
  cux:
      _stC::I64 = R3;
      _stB::I64 = R2;
      _stF::I64 = 0;
      _stE::I64 = 0;
      goto stD;
  stD:
      if (_stF::I64 == _stC::I64) goto cuH; else goto cuI;
  cuH:
      R1 = _stE::I64;
      call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
  cuI:
      _stM::I64 = %MO_S_Rem_W64(%MO_UU_Conv_W8_W64(I8[_stB::I64 + _stF::I64]) + (_stE::I64 << 7),
                                4091);
      _stF::I64 = _stF::I64 + 1;
      _stE::I64 = _stM::I64;
      goto stD;
}

-}
