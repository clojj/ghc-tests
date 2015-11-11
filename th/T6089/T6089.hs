{-# LANGUAGE TemplateHaskell #-}

module T6089 where

import Language.Haskell.TH

decs :: Q [Dec]
decs = [d| data MyType = MyCon | MyOtherCon

           val1 :: MyType
           val1 = MyCon

           val2 :: MyType
           val2 = MyOtherCon

           $( do let mkFun v i = [| if $v == i then val1 else val2 |]
                 [d| fun3 x = $(mkFun [| x |] (3 :: Int))
                     fun4 x = $(mkFun [| x |] (4 :: Int)) |] ) |]
