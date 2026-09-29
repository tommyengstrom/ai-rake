{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module TypeSafe.Jev.Effect
    ( Jev (..)
    , assess
    ) where

import Effectful
import Effectful.TH
import TypeSafe.Jev.Batch (Batch)
import TypeSafe.Jev.Types (Content, Response)

data Jev :: Effect where
    Assess :: Content -> Batch a -> Jev m (Response a)

makeEffect ''Jev
