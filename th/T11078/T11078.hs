{-# LANGUAGE TemplateHaskell #-}
module T11078 where

import T11078a as T (checkAxioms)
--import qualified T11078a as S
--import T11078a


$( checkAxioms )
