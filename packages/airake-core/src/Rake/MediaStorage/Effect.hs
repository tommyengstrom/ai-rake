module Rake.MediaStorage.Effect
    ( RakeMediaStorage (..)
    , saveMediaReferences
    , lookupMediaReference
    , saveMedia
    , loadMedia
    , saveMediaReference
    ) where

import Data.Aeson (Value)
import Effectful
import Effectful.TH
import Rake.Types
import Relude

data RakeMediaStorage :: Effect where
    SaveMedia :: StoredMedia -> RakeMediaStorage m ()
    LoadMedia :: MediaBlobId -> RakeMediaStorage m (Maybe StoredMedia)
    SaveMediaReference :: MediaProviderReference -> RakeMediaStorage m ()
    LookupMediaReference :: ProviderApiFamily -> MediaBlobId -> RakeMediaStorage m (Maybe Value)

type instance DispatchOf RakeMediaStorage = 'Dynamic

makeEffect ''RakeMediaStorage

saveMediaReferences :: RakeMediaStorage :> es => [MediaProviderReference] -> Eff es ()
saveMediaReferences =
    traverse_ saveMediaReference
