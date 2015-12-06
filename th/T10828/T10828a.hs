module T10828a where

import Language.Haskell.TH

gadtQuoteTest :: Q [Dec] -> Q [Dec]
gadtQuoteTest decs = do
    decs' <- decs
    mapM go decs'
    where
      go :: Dec -> Q Dec
      go d@(DataD _ _ _ cons _) = do
        runIO $ putStrLn "Data declaration"
        mapM con cons
        return d
      go d = return d

con :: Con -> Q ()
con (ForallC vars ctx c) = do
  runIO $ putStrLn ("ForallC. Vars:" ++ pprint vars ++ ", Context: " ++ pprint ctx)
  con c
  return ()
con (NormalC _ _) = do
  runIO $ putStrLn "NormalC"
  return ()
con (GadtC n a r i) = do
  runIO $ putStrLn ("GadtC. Name: " ++ pprint n ++ ", Args: " ++ pprint (map snd a) ++ ", Result type: " ++ pprint r ++ ", indices: " ++ pprint i)
  return ()
con (RecGadtC n a r i) = do
  runIO $ putStrLn ("RecGadtC. Name: " ++ pprint n ++ ", Args: " ++ pprint (map (\(_,_,b)->b) a) ++ ", Field names: " ++ pprint (map (\(b,_,_) -> b) a) ++ ", Result type: " ++ pprint r ++ ", indices: " ++ pprint i)
  return ()
con _ = do
  runIO $ putStrLn "other data constructor"
  return ()
