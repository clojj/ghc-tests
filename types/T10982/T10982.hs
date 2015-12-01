{-# LANGUAGE TypeFamilies #-}
{- # LANGUAGE NamedWildCards #-}

module T10982 where

type family F a where
    F _t = Int

