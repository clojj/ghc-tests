module T11078a where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Control.Monad

data M a = M a | N deriving Show

checkAxioms :: DecsQ
checkAxioms = do
    ModuleInfo ms <- reifyModule =<< thisModule
    forM_ ms $ \mi@(ModuleImport _ m _ _) -> do
        runIO (print m)
    return [
      NewtypeD [] (mkName "X") [PlainTV (mkName "a")]
                (InfixC (NotStrict, AppT (ConT $ mkName "M")
                                   (VarT $ mkName "a"))
                        (mkName ":+")
                        (NotStrict, AppT (ConT $ mkName "M")
                                   (VarT $ mkName "a")) ) []]
