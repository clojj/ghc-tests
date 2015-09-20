{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module T10267 where

import Language.Haskell.TH

{-
Note [How top-level splices are handled] in TcSplice

maybe this does not work correctly because it's a decl?

-}

{-
[d| i :: a -> a
    i = _foo |]

$(return [
   SigD (mkName "i")
        (ForallT [PlainTV (mkName "a")]
                 []
                 (AppT (AppT ArrowT (VarT (mkName "a"))) (VarT (mkName "a"))))
 , FunD (mkName "i")
        [Clause [VarP (mkName "x")] (NormalB (UnboundVarE (mkName "_"))) []]
 ])
-}
