module Rake.MediaStorage.DirectorySpec where

import Control.Exception (finally)
import Data.Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.List qualified as List
import Data.UUID.V4 (nextRandom)
import Effectful
import Rake
import Rake.MediaStorage.Directory (runRakeMediaStorageDirectory)
import Relude
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Rake.MediaStorage.Directory" $ do
    it "persists media bytes and references in per-blob files across fresh interpreters" $
        withTempMediaDirectory \mediaDirectory -> do
            let blobId = "blob-image-1"
                storedBytes = BS8.pack "cat-bytes"
                storedMedia =
                    StoredMedia
                        { mediaBlobId = blobId
                        , mimeType = Just "image/png"
                        , fileName = Just "my-image.png"
                        , mediaBytes = storedBytes
                        }
                requestPart =
                    object
                        [ "type" .= ("input_image" :: Text)
                        , "image_url" .= ("https://example.com/cat.png" :: Text)
                        ]
                mediaReference =
                    MediaProviderReference
                        { mediaBlobId = blobId
                        , providerFamily = ProviderOpenAIResponses
                        , providerRequestPart = requestPart
                        }

            (loadedMedia, loadedReference) <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ do
                        saveMedia storedMedia
                        saveMediaReference mediaReference
                        (,) <$> loadMedia blobId <*> lookupMediaReference ProviderOpenAIResponses blobId

            loadedMedia `shouldBe` Just storedMedia
            loadedReference `shouldBe` Just requestPart

            [blobDirectoryName] <- Directory.listDirectory mediaDirectory
            let blobDirectory = mediaDirectory </> blobDirectoryName
            Directory.doesFileExist (blobDirectory </> "my-image.png")
                `shouldReturn` True
            Directory.doesFileExist (blobDirectory </> "my-image-references.json")
                `shouldReturn` True
            Directory.doesFileExist (mediaDirectory </> "media-references.json")
                `shouldReturn` False

            (persistedMedia, persistedReference) <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ do
                        (,) <$> loadMedia blobId <*> lookupMediaReference ProviderOpenAIResponses blobId

            persistedMedia `shouldBe` Just storedMedia
            persistedReference `shouldBe` Just requestPart

    it "allows refs-only blobs with no saved media bytes" $
        withTempMediaDirectory \mediaDirectory -> do
            let blobId = "blob-image-1"
                requestPart =
                    object
                        [ "type" .= ("input_image" :: Text)
                        , "image_url" .= ("https://example.com/cat.png" :: Text)
                        ]
                mediaReference =
                    MediaProviderReference
                        { mediaBlobId = blobId
                        , providerFamily = ProviderOpenAIResponses
                        , providerRequestPart = requestPart
                        }

            (loadedMedia, loadedReference) <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ do
                        saveMediaReference mediaReference
                        (,) <$> loadMedia blobId <*> lookupMediaReference ProviderOpenAIResponses blobId

            loadedMedia `shouldBe` Nothing
            loadedReference `shouldBe` Just requestPart

    it "keeps the latest reference for the same blob and provider" $
        withTempMediaDirectory \mediaDirectory -> do
            let blobId = "blob-image-1"
                originalRequestPart =
                    object
                        [ "type" .= ("input_image" :: Text)
                        , "image_url" .= ("https://example.com/first.png" :: Text)
                        ]
                updatedRequestPart =
                    object
                        [ "type" .= ("input_image" :: Text)
                        , "image_url" .= ("https://example.com/second.png" :: Text)
                        ]
                originalReference =
                    MediaProviderReference
                        { mediaBlobId = blobId
                        , providerFamily = ProviderOpenAIResponses
                        , providerRequestPart = originalRequestPart
                        }
                updatedReference =
                    MediaProviderReference
                        { mediaBlobId = blobId
                        , providerFamily = ProviderOpenAIResponses
                        , providerRequestPart = updatedRequestPart
                        }

            finalLookup <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ do
                        saveMediaReference originalReference
                        saveMediaReference updatedReference
                        lookupMediaReference ProviderOpenAIResponses blobId

            finalLookup `shouldBe` Just updatedRequestPart

            persistedLookup <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ lookupMediaReference ProviderOpenAIResponses blobId

            persistedLookup `shouldBe` Just updatedRequestPart

    it "keeps distinct blob storage for ids that would previously alias or nest" $
        withTempMediaDirectory \mediaDirectory -> do
            let firstBlobId = "a/b"
                secondBlobId = "a?b"
                firstRequestPart =
                    object
                        [ "type" .= ("input_image" :: Text)
                        , "image_url" .= ("https://example.com/first.png" :: Text)
                        ]
                secondRequestPart =
                    object
                        [ "type" .= ("input_image" :: Text)
                        , "image_url" .= ("https://example.com/second.png" :: Text)
                        ]

            (firstLookup, secondLookup) <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ do
                        saveMediaReference
                            MediaProviderReference
                                { mediaBlobId = firstBlobId
                                , providerFamily = ProviderOpenAIResponses
                                , providerRequestPart = firstRequestPart
                                }
                        saveMediaReference
                            MediaProviderReference
                                { mediaBlobId = secondBlobId
                                , providerFamily = ProviderOpenAIResponses
                                , providerRequestPart = secondRequestPart
                                }
                        (,) <$> lookupMediaReference ProviderOpenAIResponses firstBlobId
                            <*> lookupMediaReference ProviderOpenAIResponses secondBlobId

            firstLookup `shouldBe` Just firstRequestPart
            secondLookup `shouldBe` Just secondRequestPart

            blobDirectories <- Directory.listDirectory mediaDirectory
            length blobDirectories `shouldBe` 2

    it "avoids content filename collisions with sidecar files" $
        withTempMediaDirectory \mediaDirectory -> do
            let blobId = "blob-json"
                storedMedia =
                    StoredMedia
                        { mediaBlobId = blobId
                        , mimeType = Just "application/json"
                        , fileName = Just "foo-references.json"
                        , mediaBytes = BS8.pack "{\"ok\":true}"
                        }
                requestPart =
                    object
                        [ "type" .= ("input_file" :: Text)
                        , "file_id" .= ("file-123" :: Text)
                        ]

            (loadedMedia, loadedReference) <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ do
                        saveMedia storedMedia
                        saveMediaReference
                            MediaProviderReference
                                { mediaBlobId = blobId
                                , providerFamily = ProviderOpenAIResponses
                                , providerRequestPart = requestPart
                                }
                        (,) <$> loadMedia blobId <*> lookupMediaReference ProviderOpenAIResponses blobId

            loadedMedia `shouldBe` Just storedMedia
            loadedReference `shouldBe` Just requestPart

            [blobDirectoryName] <- Directory.listDirectory mediaDirectory
            blobEntries <- Directory.listDirectory (mediaDirectory </> blobDirectoryName)
            length (filter (List.isSuffixOf "-references.json") blobEntries) `shouldBe` 1

    it "avoids content filename collisions with the lock file" $
        withTempMediaDirectory \mediaDirectory -> do
            let blobId = "blob-lock"
                storedMedia =
                    StoredMedia
                        { mediaBlobId = blobId
                        , mimeType = Just "text/plain"
                        , fileName = Just ".media.lock"
                        , mediaBytes = BS8.pack "hello"
                        }

            loadedMedia <-
                runEff
                    . runRakeMediaStorageDirectory mediaDirectory
                    $ do
                        saveMedia storedMedia
                        loadMedia blobId

            loadedMedia `shouldBe` Just storedMedia

withTempMediaDirectory :: (FilePath -> IO a) -> IO a
withTempMediaDirectory action = do
    tempRoot <- Directory.getTemporaryDirectory
    uuid <- nextRandom
    let mediaDirectory = tempRoot </> ("ai-rake-media-" <> show uuid)
    Directory.createDirectoryIfMissing True mediaDirectory
    action mediaDirectory
        `finally` Directory.removePathForcibly mediaDirectory
