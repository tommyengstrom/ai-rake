module Rake.MediaStorage.InMemory
    ( runRakeMediaStorageInMemory
    ) where

import Data.Aeson (Value)
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Dispatch.Dynamic (interpretWith)
import Rake.MediaStorage.Effect
import Rake.Types
import Relude

data InMemoryMediaEntry = InMemoryMediaEntry
    { mediaContent :: Maybe StoredMedia
    , providerReferences :: Map.Map ProviderApiFamily Value
    }

type InMemoryMediaStore = Map.Map MediaBlobId InMemoryMediaEntry

runRakeMediaStorageInMemory
    :: IOE :> es
    => Eff (RakeMediaStorage ': es) a
    -> Eff es a
runRakeMediaStorageInMemory eff = do
    mediaStoreRef <- liftIO (IORef.newIORef (mempty :: InMemoryMediaStore))
    interpretWith eff \_ -> \case
        SaveMedia storedMedia@StoredMedia{mediaBlobId} ->
            liftIO $
                IORef.atomicModifyIORef' mediaStoreRef \mediaStore ->
                    ( Map.alter
                        (Just . setMediaContent storedMedia . fromMaybe emptyMediaEntry)
                        mediaBlobId
                        mediaStore
                    , ()
                    )
        LoadMedia mediaBlobId ->
            liftIO $
                Map.lookup mediaBlobId <$> IORef.readIORef mediaStoreRef
                    <&> (>>= inMemoryMediaContent)
        SaveMediaReference MediaProviderReference{mediaBlobId, providerFamily, providerRequestPart} ->
            liftIO $
                IORef.atomicModifyIORef' mediaStoreRef \mediaStore ->
                    ( Map.alter
                        ( Just
                            . setProviderReference providerFamily providerRequestPart
                            . fromMaybe emptyMediaEntry
                        )
                        mediaBlobId
                        mediaStore
                    , ()
                    )
        LookupMediaReference providerFamily mediaBlobId ->
            liftIO $
                Map.lookup mediaBlobId <$> IORef.readIORef mediaStoreRef
                    <&> (>>= lookupInMemoryProviderReference providerFamily)

emptyMediaEntry :: InMemoryMediaEntry
emptyMediaEntry =
    InMemoryMediaEntry
        { mediaContent = Nothing
        , providerReferences = mempty
        }

setMediaContent :: StoredMedia -> InMemoryMediaEntry -> InMemoryMediaEntry
setMediaContent storedMedia mediaEntry =
    mediaEntry
        { mediaContent = Just storedMedia
        }

setProviderReference :: ProviderApiFamily -> Value -> InMemoryMediaEntry -> InMemoryMediaEntry
setProviderReference providerFamily providerRequestPart mediaEntry =
    case mediaEntry of
        InMemoryMediaEntry{mediaContent, providerReferences} ->
            InMemoryMediaEntry
                { mediaContent
                , providerReferences =
                    Map.insert providerFamily providerRequestPart providerReferences
                }

inMemoryMediaContent :: InMemoryMediaEntry -> Maybe StoredMedia
inMemoryMediaContent InMemoryMediaEntry{mediaContent} = mediaContent

lookupInMemoryProviderReference :: ProviderApiFamily -> InMemoryMediaEntry -> Maybe Value
lookupInMemoryProviderReference targetProviderFamily InMemoryMediaEntry{providerReferences} =
    Map.lookup targetProviderFamily providerReferences
