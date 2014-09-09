{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module T9157 where

import GHC.Exts
import GHC.Types


data Chunk
   = Chunk00
   | Chunk01 Word#
   | Chunk02 Word#
   | Chunk03 Word#
   | Chunk04 Word#
   | Chunk05 Word#
   | Chunk06 Word#
   | Chunk07 Word#
   | Chunk08 Word#


toTuple# :: Chunk -> (# Int#, (# Word# #) #)
toTuple# chunk = (# dataToTag# chunk, words# chunk #)
   where words# (Chunk01 w#) = (# w# #)
         words# (Chunk02 w#) = (# w# #)
         words# (Chunk03 w#) = (# w# #)
         words# (Chunk04 w#) = (# w# #)
         words# (Chunk05 w#) = (# w# #)
         words# (Chunk06 w#) = (# w# #)
         words# (Chunk07 w#) = (# w# #)
         words# (Chunk08 w#) = (# w# #)

