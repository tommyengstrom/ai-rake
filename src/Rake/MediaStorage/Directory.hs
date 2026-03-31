module Rake.MediaStorage.Directory
    ( runRakeMediaStorageDirectory
    ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Dispatch.Dynamic (interpretWith)
import Numeric (showHex)
import Rake.MediaStorage.Effect
import Rake.Types
import Relude
import System.Directory qualified as Directory
import System.FileLock qualified as FileLock
import System.FilePath ((</>), dropExtension, takeDirectory, takeExtension, takeFileName)
import System.IO qualified as IO

data MediaSidecar = MediaSidecar
    { sidecarBlobId :: MediaBlobId
    , sidecarMimeType :: Maybe Text
    , sidecarFileName :: Maybe Text
    , sidecarContentFileName :: Maybe FilePath
    , sidecarProviderReferences :: [MediaProviderReference]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

runRakeMediaStorageDirectory
    :: IOE :> es
    => FilePath
    -> Eff (RakeMediaStorage ': es) a
    -> Eff es a
runRakeMediaStorageDirectory storageDirectory eff = do
    liftIO (Directory.createDirectoryIfMissing True storageDirectory)
    interpretWith eff \_ -> \case
        SaveMedia storedMedia ->
            liftIO (saveMediaToDirectory storageDirectory storedMedia)
        LoadMedia mediaBlobId ->
            liftIO (loadMediaFromDirectory storageDirectory mediaBlobId)
        SaveMediaReference mediaReference ->
            liftIO (saveMediaReferenceToDirectory storageDirectory mediaReference)
        LookupMediaReference providerFamily mediaBlobId ->
            liftIO (lookupMediaReferenceFromDirectory storageDirectory providerFamily mediaBlobId)

saveMediaToDirectory :: FilePath -> StoredMedia -> IO ()
saveMediaToDirectory storageDirectory storedMedia@StoredMedia{mediaBlobId, mediaBytes} =
    withBlobExclusiveLock storageDirectory mediaBlobId do
        maybeExistingSidecar <- readSidecarUnlocked storageDirectory mediaBlobId
        let existingSidecar = fromMaybe (emptyMediaSidecar mediaBlobId) maybeExistingSidecar
            updatedSidecar = applyStoredMedia storedMedia existingSidecar
            maybeOldSidecarPath = sidecarPath storageDirectory <$> maybeExistingSidecar
            newSidecarPath = sidecarPath storageDirectory updatedSidecar
            maybeOldContentPath = contentPath storageDirectory =<< maybeExistingSidecar
            maybeNewContentPath = contentPath storageDirectory updatedSidecar
        case maybeNewContentPath of
            Nothing ->
                fail ("Directory media storage could not determine a content path for blob " <> show mediaBlobId)
            Just newContentPath -> do
                writeByteStringAtomic newContentPath mediaBytes
                writeJsonAtomic newSidecarPath updatedSidecar
                maybe (pure ()) (removeStaleFile newContentPath) maybeOldContentPath
                maybe (pure ()) (removeStaleFile newSidecarPath) maybeOldSidecarPath

loadMediaFromDirectory :: FilePath -> MediaBlobId -> IO (Maybe StoredMedia)
loadMediaFromDirectory storageDirectory mediaBlobId = do
    let blobDirectory = mediaBlobDirectory storageDirectory mediaBlobId
    blobExists <- Directory.doesDirectoryExist blobDirectory
    if blobExists
        then
            withBlobSharedLock storageDirectory mediaBlobId do
                maybeSidecar <- readSidecarUnlocked storageDirectory mediaBlobId
                case maybeSidecar of
                    Nothing ->
                        pure Nothing
                    Just MediaSidecar{sidecarContentFileName = Nothing} ->
                        pure Nothing
                    Just sidecar@MediaSidecar
                        { sidecarMimeType
                        , sidecarFileName
                        } ->
                            case contentPath storageDirectory sidecar of
                                Nothing ->
                                    pure Nothing
                                Just mediaPath -> do
                                    contentExists <- Directory.doesFileExist mediaPath
                                    if contentExists
                                        then do
                                            mediaBytes <- BS.readFile mediaPath
                                            pure
                                                ( Just
                                                    StoredMedia
                                                        { mediaBlobId
                                                        , mimeType = sidecarMimeType
                                                        , fileName = sidecarFileName
                                                        , mediaBytes
                                                        }
                                                )
                                        else
                                            fail
                                                ( "Stored media bytes are missing for blob "
                                                    <> show mediaBlobId
                                                    <> " at "
                                                    <> mediaPath
                                                )
        else pure Nothing

saveMediaReferenceToDirectory :: FilePath -> MediaProviderReference -> IO ()
saveMediaReferenceToDirectory storageDirectory mediaReference@MediaProviderReference{mediaBlobId} =
    withBlobExclusiveLock storageDirectory mediaBlobId do
        maybeExistingSidecar <- readSidecarUnlocked storageDirectory mediaBlobId
        let existingSidecar = fromMaybe (emptyMediaSidecar mediaBlobId) maybeExistingSidecar
            updatedSidecar = applyMediaReference mediaReference existingSidecar
            maybeOldSidecarPath = sidecarPath storageDirectory <$> maybeExistingSidecar
            newSidecarPath = sidecarPath storageDirectory updatedSidecar
        writeJsonAtomic newSidecarPath updatedSidecar
        maybe (pure ()) (removeStaleFile newSidecarPath) maybeOldSidecarPath

lookupMediaReferenceFromDirectory :: FilePath -> ProviderApiFamily -> MediaBlobId -> IO (Maybe Value)
lookupMediaReferenceFromDirectory storageDirectory providerFamily mediaBlobId = do
    let blobDirectory = mediaBlobDirectory storageDirectory mediaBlobId
    blobExists <- Directory.doesDirectoryExist blobDirectory
    if blobExists
        then
            withBlobSharedLock storageDirectory mediaBlobId do
                maybeSidecar <- readSidecarUnlocked storageDirectory mediaBlobId
                pure (maybeSidecar >>= lookupProviderReference providerFamily)
        else pure Nothing

emptyMediaSidecar :: MediaBlobId -> MediaSidecar
emptyMediaSidecar sidecarBlobId =
    MediaSidecar
        { sidecarBlobId
        , sidecarMimeType = Nothing
        , sidecarFileName = Nothing
        , sidecarContentFileName = Nothing
        , sidecarProviderReferences = []
        }

applyStoredMedia :: StoredMedia -> MediaSidecar -> MediaSidecar
applyStoredMedia StoredMedia{mediaBlobId, mimeType, fileName} MediaSidecar{sidecarProviderReferences} =
    MediaSidecar
        { sidecarBlobId = mediaBlobId
        , sidecarMimeType = mimeType
        , sidecarFileName = fileName
        , sidecarContentFileName = Just (contentFileName mediaBlobId mimeType fileName)
        , sidecarProviderReferences
        }

applyMediaReference :: MediaProviderReference -> MediaSidecar -> MediaSidecar
applyMediaReference mediaReference@MediaProviderReference{mediaBlobId, providerFamily} mediaSidecar =
    case mediaSidecar of
        MediaSidecar
            { sidecarMimeType
            , sidecarFileName
            , sidecarContentFileName
            , sidecarProviderReferences
            } ->
                MediaSidecar
                    { sidecarBlobId = mediaBlobId
                    , sidecarMimeType
                    , sidecarFileName
                    , sidecarContentFileName
                    , sidecarProviderReferences =
                        mediaReference : filter (not . matchesProvider providerFamily) sidecarProviderReferences
                    }

matchesProvider :: ProviderApiFamily -> MediaProviderReference -> Bool
matchesProvider targetProviderFamily MediaProviderReference{providerFamily} =
    providerFamily == targetProviderFamily

lookupProviderReference :: ProviderApiFamily -> MediaSidecar -> Maybe Value
lookupProviderReference targetProviderFamily MediaSidecar{sidecarProviderReferences} =
    sidecarProviderReferences
        & find (matchesProvider targetProviderFamily)
        <&> \MediaProviderReference{providerRequestPart} -> providerRequestPart

contentPath :: FilePath -> MediaSidecar -> Maybe FilePath
contentPath storageDirectory MediaSidecar{sidecarBlobId, sidecarContentFileName} =
    sidecarContentFileName <&> \storedFileName ->
        mediaBlobDirectory storageDirectory sidecarBlobId </> storedFileName

sidecarPath :: FilePath -> MediaSidecar -> FilePath
sidecarPath storageDirectory mediaSidecar@MediaSidecar{sidecarBlobId} =
    mediaBlobDirectory storageDirectory sidecarBlobId
        </> (sidecarBaseName mediaSidecar <> sidecarSuffix)

sidecarBaseName :: MediaSidecar -> FilePath
sidecarBaseName MediaSidecar
    { sidecarBlobId
    , sidecarFileName
    , sidecarContentFileName
    } =
        case sidecarContentFileName of
            Just storedFileName ->
                dropExtension storedFileName
            Nothing ->
                fromMaybe
                    (safeBlobBaseName sidecarBlobId)
                    (sidecarFileName >>= sanitizeUserFileName >>= stemFromFileName)

contentFileName :: MediaBlobId -> Maybe Text -> Maybe Text -> FilePath
contentFileName mediaBlobId mimeType fileName =
    reserveInternalFileName $
        fromMaybe
            (safeBlobBaseName mediaBlobId <> fromMaybe "" (mimeExtension =<< mimeType))
            (fileName >>= sanitizeUserFileName)

sanitizeUserFileName :: Text -> Maybe FilePath
sanitizeUserFileName rawFileName =
    let normalizedFileName = takeFileName (toString rawFileName)
     in if normalizedFileName == "" || normalizedFileName == "." || normalizedFileName == ".."
            then Nothing
            else Just normalizedFileName

stemFromFileName :: FilePath -> Maybe FilePath
stemFromFileName fileName =
    let fileStem = dropExtension fileName
     in if null fileStem then Nothing else Just fileStem

mimeExtension :: Text -> Maybe FilePath
mimeExtension mimeType =
    case Text.toLower mimeType of
        "image/png" -> Just ".png"
        "image/jpeg" -> Just ".jpg"
        "image/jpg" -> Just ".jpg"
        "image/gif" -> Just ".gif"
        "image/webp" -> Just ".webp"
        "audio/mpeg" -> Just ".mp3"
        "audio/mp3" -> Just ".mp3"
        "audio/wav" -> Just ".wav"
        "audio/ogg" -> Just ".ogg"
        "application/pdf" -> Just ".pdf"
        "application/json" -> Just ".json"
        "text/plain" -> Just ".txt"
        _ -> Nothing

safeBlobBaseName :: MediaBlobId -> FilePath
safeBlobBaseName (MediaBlobId blobIdText) =
    "blob-" <> concatMap hexEncodeByte (BS.unpack (TextEncoding.encodeUtf8 blobIdText))

hexEncodeByte :: Word8 -> String
hexEncodeByte byte =
    let rendered = showHex byte ""
     in if length rendered == 1 then '0' : rendered else rendered

reserveInternalFileName :: FilePath -> FilePath
reserveInternalFileName fileName
    | fileName == blobLockFileName = appendBeforeExtension fileName "-content"
    | List.isSuffixOf sidecarSuffix fileName = appendBeforeExtension fileName "-content"
    | otherwise = fileName

appendBeforeExtension :: FilePath -> String -> FilePath
appendBeforeExtension fileName suffix =
    let extension = takeExtension fileName
        fileStem = dropExtension fileName
     in if null extension
            then fileName <> suffix
            else fileStem <> suffix <> extension

sidecarSuffix :: String
sidecarSuffix = "-references.json"

blobLockFileName :: FilePath
blobLockFileName = ".media.lock"

readSidecarUnlocked :: FilePath -> MediaBlobId -> IO (Maybe MediaSidecar)
readSidecarUnlocked storageDirectory mediaBlobId = do
    let blobDirectory = mediaBlobDirectory storageDirectory mediaBlobId
    blobExists <- Directory.doesDirectoryExist blobDirectory
    if blobExists
        then do
            sidecarCandidates <-
                filter (List.isSuffixOf sidecarSuffix) <$> Directory.listDirectory blobDirectory
            case sidecarCandidates of
                [] ->
                    pure Nothing
                [sidecarFileName] -> do
                    let sidecarFilePath = blobDirectory </> sidecarFileName
                    decodedSidecar <-
                        (Aeson.eitherDecodeFileStrict' sidecarFilePath :: IO (Either String MediaSidecar))
                    case decodedSidecar of
                        Left decodeError ->
                            fail
                                ( "Failed to decode media sidecar from "
                                    <> sidecarFilePath
                                    <> ": "
                                    <> decodeError
                                )
                        Right mediaSidecar ->
                            pure (Just mediaSidecar)
                _ ->
                    fail
                        ( "Found multiple media sidecars for blob "
                            <> show mediaBlobId
                            <> " in "
                            <> blobDirectory
                        )
        else pure Nothing

mediaBlobDirectory :: FilePath -> MediaBlobId -> FilePath
mediaBlobDirectory storageDirectory mediaBlobId =
    storageDirectory </> safeBlobBaseName mediaBlobId

withBlobExclusiveLock :: FilePath -> MediaBlobId -> IO a -> IO a
withBlobExclusiveLock storageDirectory mediaBlobId action = do
    let blobDirectory = mediaBlobDirectory storageDirectory mediaBlobId
    Directory.createDirectoryIfMissing True blobDirectory
    FileLock.withFileLock (blobDirectory </> blobLockFileName) FileLock.Exclusive \_ -> action

withBlobSharedLock :: FilePath -> MediaBlobId -> IO a -> IO a
withBlobSharedLock storageDirectory mediaBlobId action =
    FileLock.withFileLock (mediaBlobDirectory storageDirectory mediaBlobId </> blobLockFileName) FileLock.Shared \_ -> action

writeJsonAtomic :: ToJSON a => FilePath -> a -> IO ()
writeJsonAtomic path value =
    writeLazyByteStringAtomic path (Aeson.encode value)

writeByteStringAtomic :: FilePath -> BS.ByteString -> IO ()
writeByteStringAtomic path bytes =
    writeBinaryFileAtomic path (\handle -> BS.hPut handle bytes)

writeLazyByteStringAtomic :: FilePath -> LBS.ByteString -> IO ()
writeLazyByteStringAtomic path bytes =
    writeBinaryFileAtomic path (\handle -> LBS.hPut handle bytes)

writeBinaryFileAtomic :: FilePath -> (IO.Handle -> IO ()) -> IO ()
writeBinaryFileAtomic path writeContents = do
    let parentDirectory = takeDirectory path
        fileName = takeFileName path
    Directory.createDirectoryIfMissing True parentDirectory
    (tempPath, tempHandle) <- IO.openBinaryTempFile parentDirectory (fileName <> ".tmp")
    writeContents tempHandle
    IO.hClose tempHandle
    removeFileIfExists path
    Directory.renameFile tempPath path

removeStaleFile :: FilePath -> FilePath -> IO ()
removeStaleFile currentPath stalePath =
    when (currentPath /= stalePath) (removeFileIfExists stalePath)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
    fileExists <- Directory.doesFileExist filePath
    when fileExists (Directory.removeFile filePath)
