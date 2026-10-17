{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module TypeSafe.Jev.Effect
    ( Jev (..)
    , assess
    , assessWithAssets
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.TH
import TypeSafe.Jev.Batch (Batch)
import TypeSafe.Jev.Types (Content, ImageAsset, Response)

data Jev :: Effect where
    AssessWithAssets :: Map Text ImageAsset -> Content -> Batch a -> Jev m (Response a)

makeEffect ''Jev

assess :: Jev :> es => Content -> Batch a -> Eff es (Response a)
assess = assessWithAssets Map.empty
