module GenCliSupport
    ( buildOutputPaths
    , downloadBinary
    , resolveInlineImageSource
    , resolveMediaSource
    , slugifyPromptWithFallback
    , urlExtension
    , writeBinaryFile
    ) where

import Control.Exception (try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, toLower)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Data.Time
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (newTlsManager)
import Rake.Providers.Gemini.Images (GeminiInlineImage (..))
import Relude
import System.Directory
import System.FilePath

slugifyPromptWithFallback :: FilePath -> Text -> FilePath
slugifyPromptWithFallback fallback promptText =
    case take 64 (toString collapsedSlug) of
        [] ->
            fallback
        slug ->
            slug
  where
    rawSlug =
        T.map normalizeChar (T.toLower promptText)
    collapsedSlug =
        T.dropAround (== '-') $
            T.intercalate "-"
                (filter (not . T.null) (T.splitOn "-" rawSlug))

    normalizeChar char
        | isAlphaNum char =
            char
        | otherwise =
            '-'

resolveMediaSource :: Text -> IO (Either Text Text)
resolveMediaSource source
    | isRemoteOrDataSource source =
        pure (Right source)
    | otherwise = do
        let path = toString source
        exists <- doesFileExist path
        if exists
            then do
                sourceBytes <- BS.readFile path
                pure $
                    case detectImageMimeType sourceBytes <|> mimeTypeFromPath path of
                        Nothing ->
                            Left ("Could not determine a MIME type for source file: " <> source)
                        Just mimeType ->
                            Right (renderDataUrl mimeType sourceBytes)
            else pure (Left ("Source is not a URL, data URL, or existing file: " <> source))

resolveInlineImageSource :: Text -> IO (Either Text GeminiInlineImage)
resolveInlineImageSource source
    | "data:" `T.isPrefixOf` source =
        pure (decodeDataUrl source)
    | isRemoteUrl source = do
        downloadResult <- downloadBinary source
        pure $ do
            sourceBytes <- downloadResult
            mimeType <-
                maybe
                    (Left ("Could not determine a MIME type for source URL: " <> source))
                    Right
                    (detectImageMimeType sourceBytes <|> mimeTypeFromPath (toString source))
            pure (inlineImageFromBytes mimeType sourceBytes)
    | otherwise = do
        let path = toString source
        exists <- doesFileExist path
        if exists
            then do
                sourceBytes <- BS.readFile path
                pure $ do
                    mimeType <-
                        maybe
                            (Left ("Could not determine a MIME type for source file: " <> source))
                            Right
                            (detectImageMimeType sourceBytes <|> mimeTypeFromPath path)
                    pure (inlineImageFromBytes mimeType sourceBytes)
            else pure (Left ("Source is not a URL, data URL, or existing file: " <> source))

downloadBinary :: Text -> IO (Either Text BS.ByteString)
downloadBinary mediaUrl = do
    try @SomeException
        ( do
            manager <- newTlsManager
            request <- HttpClient.parseRequest (toString mediaUrl)
            response <- HttpClient.httpLbs request manager
            pure (LBS.toStrict (HttpClient.responseBody response))
        )
        >>= \case
            Left err ->
                pure (Left ("Failed to download media: " <> toText (displayException err)))
            Right bytes ->
                pure (Right bytes)

buildOutputPaths :: FilePath -> Maybe FilePath -> Text -> [String] -> IO [FilePath]
buildOutputPaths defaultSlug maybeOutput promptText extensions =
    case maybeOutput of
        Just requestedPath ->
            traverse
                (uncurry ensureUniquePath)
                (zip (requestedPathTemplates requestedPath (length extensions)) extensions)
        Nothing -> do
            timestamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime
            let basePath = "generated" </> (timestamp <> "-" <> slugifyPromptWithFallback defaultSlug promptText)
            traverse
                (uncurry ensureUniquePath)
                (zip (defaultPathTemplates basePath (length extensions)) extensions)

writeBinaryFile :: FilePath -> BS.ByteString -> IO ()
writeBinaryFile path bytes =
    BS.writeFile path bytes

urlExtension :: Text -> Maybe String
urlExtension sourceUrl =
    case map toLower (takeExtension (takeWhile (/= '?') (toString sourceUrl))) of
        "" ->
            Nothing
        ext ->
            Just ext

isRemoteOrDataSource :: Text -> Bool
isRemoteOrDataSource source =
    "data:" `T.isPrefixOf` source || isRemoteUrl source

isRemoteUrl :: Text -> Bool
isRemoteUrl source =
    "http://" `T.isPrefixOf` source
        || "https://" `T.isPrefixOf` source

renderDataUrl :: Text -> BS.ByteString -> Text
renderDataUrl mimeType sourceBytes =
    "data:"
        <> mimeType
        <> ";base64,"
        <> TextEncoding.decodeUtf8 (Base64.encode sourceBytes)

decodeDataUrl :: Text -> Either Text GeminiInlineImage
decodeDataUrl source = do
    header <- maybe (Left ("Invalid data URL: " <> source)) Right (T.stripPrefix "data:" rawHeader)
    let (mimeType, parameters) = splitDataUrlHeader header
    when (T.null mimeType) $
        Left ("Data URL is missing a MIME type: " <> source)
    unless ("base64" `elem` parameters) $
        Left ("Only base64 data URLs are supported: " <> source)
    when (T.null encodedPayload) $
        Left ("Data URL is missing payload: " <> source)
    decodedBytes <-
        first
            (("Failed to decode base64 data URL: " <>) . toText)
            (Base64.decode (TextEncoding.encodeUtf8 encodedPayload))
    pure (inlineImageFromBytes mimeType decodedBytes)
  where
    (rawHeader, payloadWithComma) = T.breakOn "," source
    encodedPayload = T.drop 1 payloadWithComma

splitDataUrlHeader :: Text -> (Text, [Text])
splitDataUrlHeader header =
    case T.splitOn ";" header of
        [] ->
            ("", [])
        mimeType : parameters ->
            (mimeType, parameters)

inlineImageFromBytes :: Text -> BS.ByteString -> GeminiInlineImage
inlineImageFromBytes mimeType sourceBytes =
    GeminiInlineImage
        { mimeType
        , base64Data = TextEncoding.decodeUtf8 (Base64.encode sourceBytes)
        }

requestedPathTemplates :: FilePath -> Int -> [FilePath]
requestedPathTemplates requestedPath assetTotal
    | assetTotal <= 1 =
        [requestedPath]
    | otherwise =
        [dropExtension requestedPath <> "-" <> show index <> takeExtension requestedPath | index <- [1 .. assetTotal]]

defaultPathTemplates :: FilePath -> Int -> [FilePath]
defaultPathTemplates basePath assetTotal
    | assetTotal <= 1 =
        [basePath]
    | otherwise =
        [basePath <> "-" <> show index | index <- [1 .. assetTotal]]

ensureUniquePath :: FilePath -> String -> IO FilePath
ensureUniquePath pathTemplate suggestedExtension = do
    let normalizedPath = applySuggestedExtension pathTemplate suggestedExtension
    createDirectoryIfMissing True (takeDirectory normalizedPath)
    let pathStem = dropExtension normalizedPath
        pathExtension = takeExtension normalizedPath
    go pathStem pathExtension (1 :: Int)
  where
    go pathStem pathExtension suffix = do
        let candidatePath =
                if suffix == 1
                    then pathStem <> pathExtension
                    else pathStem <> "-" <> show suffix <> pathExtension
        exists <- doesFileExist candidatePath
        if exists
            then go pathStem pathExtension (suffix + 1)
            else pure candidatePath

applySuggestedExtension :: FilePath -> String -> FilePath
applySuggestedExtension pathTemplate suggestedExtension
    | null (takeExtension pathTemplate) =
        pathTemplate <.> dropLeadingDot suggestedExtension
    | otherwise =
        pathTemplate

dropLeadingDot :: String -> String
dropLeadingDot = dropWhile (== '.')

detectImageMimeType :: BS.ByteString -> Maybe Text
detectImageMimeType bytes
    | pngSignature `BS.isPrefixOf` bytes =
        Just "image/png"
    | jpegSignature `BS.isPrefixOf` bytes =
        Just "image/jpeg"
    | gif87aSignature `BS.isPrefixOf` bytes || gif89aSignature `BS.isPrefixOf` bytes =
        Just "image/gif"
    | riffSignature `BS.isPrefixOf` bytes && webpSignature `BS.isInfixOf` BS.take 16 bytes =
        Just "image/webp"
    | otherwise =
        Nothing
  where
    pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]
    jpegSignature = BS.pack [255, 216, 255]
    gif87aSignature = TextEncoding.encodeUtf8 "GIF87a"
    gif89aSignature = TextEncoding.encodeUtf8 "GIF89a"
    riffSignature = TextEncoding.encodeUtf8 "RIFF"
    webpSignature = TextEncoding.encodeUtf8 "WEBP"

mimeTypeFromPath :: FilePath -> Maybe Text
mimeTypeFromPath path =
    case map toLower (takeExtension path) of
        ".png" ->
            Just "image/png"
        ".jpg" ->
            Just "image/jpeg"
        ".jpeg" ->
            Just "image/jpeg"
        ".gif" ->
            Just "image/gif"
        ".webp" ->
            Just "image/webp"
        ".mp4" ->
            Just "video/mp4"
        ".mov" ->
            Just "video/quicktime"
        ".webm" ->
            Just "video/webm"
        _ ->
            Nothing
