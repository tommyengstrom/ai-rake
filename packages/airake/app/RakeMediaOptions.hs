{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE NoOverloadedLists #-}

module RakeMediaOptions
    ( MediaOptions (..)
    , parseMediaOptions
    , chooseModel
    , finishOptions
    , enumSetting
    , numericSetting
    , rejectSettings
    , runMediaCli
    , runProvider
    , saveBytes
    ) where

import Control.Exception (IOException, try)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Rake.Error
import RakeCliSupport
import Relude hiding (die, getArgs, lookupEnv)
import System.Environment (getArgs, lookupEnv)
import System.IO qualified as IO

data MediaOptions = MediaOptions
    { selectedModel :: Text
    , modelSettings :: Object
    , rawSettings :: Bool
    , promptText :: Text
    , outputPath :: Maybe FilePath
    , imageSources :: [Text]
    , lastFrameSource :: Maybe Text
    , extendSource :: Maybe Text
    , pollingSelected :: Bool
    , pollInterval :: Int
    , pollAttempts :: Int
    }
    deriving stock (Show, Eq)

parseMediaOptions
    :: Text -> [(Text, Text, Text -> Either Text Value)] -> [Text] -> Either Text MediaOptions
parseMediaOptions defaultModel flags = go initial [] False
  where
    initial :: MediaOptions
    initial = MediaOptions defaultModel mempty False "" Nothing [] Nothing Nothing False 5000 120
    go :: MediaOptions -> [Text] -> Bool -> [Text] -> Either Text MediaOptions
    go options promptParts modelSelected = \case
        [] -> Right options{promptText = T.unwords (reverse promptParts)}
        "--" : rest -> Right options{promptText = T.unwords (reverse promptParts <> rest)}
        arg : rest
            | "--" `T.isPrefixOf` arg
            , (key, value) <- T.breakOn "=" arg
            , not (T.null value) ->
                go options promptParts modelSelected (key : T.drop 1 value : rest)
            | arg `elem` ["-m", "--model"] -> do
                when modelSelected $ Left "Use --model only once"
                (value, remaining) <- takeValue arg rest
                go options{selectedModel = value} promptParts True remaining
            | arg `elem` ["-o", "--output"] -> do
                (value, remaining) <- takeValue arg rest
                when (isJust (outputPath options)) $ Left "Use --output only once"
                go options{outputPath = Just (toString value)} promptParts modelSelected remaining
            | arg == "--model-options" -> do
                when (rawSettings options) $ Left "Use --model-options only once"
                (value, remaining) <- takeValue arg rest
                fields <- case eitherDecodeStrictText value of
                    Right (Object fields) -> Right fields
                    _ -> Left "--model-options must be a JSON object"
                merged <- mergeExplicit (modelSettings options) fields
                go options{modelSettings = merged, rawSettings = True} promptParts modelSelected remaining
            | arg == "--image" -> do
                (value, remaining) <- takeValue arg rest
                go
                    options{imageSources = imageSources options <> [value]}
                    promptParts
                    modelSelected
                    remaining
            | arg == "--last-frame" -> do
                (value, remaining) <- takeValue arg rest
                when (isJust (lastFrameSource options)) $ Left "Use --last-frame only once"
                go options{lastFrameSource = Just value} promptParts modelSelected remaining
            | arg == "--extend" -> do
                (value, remaining) <- takeValue arg rest
                when (isJust (extendSource options)) $ Left "Use --extend only once"
                go options{extendSource = Just value} promptParts modelSelected remaining
            | arg `elem` ["--poll-interval-ms", "--max-poll-attempts"] -> do
                (value, remaining) <- takeValue arg rest
                number <- maybe (Left (arg <> " requires an integer")) Right (readMaybe (toString value))
                unless (number > 0 && number <= maxBound `div` 1000)
                    $ Left (arg <> " must be positive and bounded")
                let updated :: MediaOptions
                    updated =
                        if arg == "--poll-interval-ms"
                            then options{pollInterval = number, pollingSelected = True}
                            else options{pollAttempts = number, pollingSelected = True}
                go updated promptParts modelSelected remaining
            | Just (_, field, parseValue) <- find (\(flag, _, _) -> flag == arg) flags -> do
                (value, remaining) <- takeValue arg rest
                parsed <- parseValue value
                merged <- mergeExplicit (modelSettings options) (KM.singleton (Key.fromText field) parsed)
                go options{modelSettings = merged} promptParts modelSelected remaining
            | "-" `T.isPrefixOf` arg ->
                Left
                    ( "Unsupported option: "
                        <> arg
                        <> ". See --help; native-only options are not forwarded silently."
                    )
            | otherwise -> go options (arg : promptParts) modelSelected rest

    takeValue :: Text -> [Text] -> Either Text (Text, [Text])
    takeValue key = \case
        [] -> Left (key <> " requires a value")
        value : remaining | not (T.null value) -> Right (value, remaining)
        _ -> Left (key <> " requires a nonempty value")

mergeExplicit :: Object -> Object -> Either Text Object
mergeExplicit left right = do
    forM_ (KM.keys right) $ \key ->
        when (KM.member key left) $ Left ("Setting provided more than once: " <> Key.toText key)
    pure (KM.union left right)

chooseModel :: [Text] -> Text -> Either Text Text
chooseModel catalog requested
    | "/" `T.isInfixOf` canonical = Right canonical
    | otherwise = case filter ((== canonical) . lastPart) catalog of
        [model] -> Right model
        _ -> Left "Use a full OpenRouter author/model ID for an unknown or ambiguous model"
  where
    withoutPrefix :: Text
    withoutPrefix = fromMaybe requested (T.stripPrefix "openrouter/" requested)
    canonical :: Text
    canonical = maybe withoutPrefix ("x-ai/" <>) (T.stripPrefix "xai/" withoutPrefix)
    lastPart :: Text -> Text
    lastPart = snd . T.breakOnEnd "/"

finishOptions :: [(Text, Object)] -> MediaOptions -> Either Text MediaOptions
finishOptions catalog options@MediaOptions{selectedModel, modelSettings, rawSettings, promptText} = do
    model <- chooseModel (map fst catalog) selectedModel
    when (T.null (T.strip promptText)) $ Left "A prompt is required"
    let defaults :: Object
        defaults = if rawSettings then mempty else fromMaybe mempty (lookup model catalog)
    pure options{selectedModel = model, modelSettings = KM.union modelSettings defaults}

enumSetting :: Text -> [Value] -> Object -> Either Text ()
enumSetting key allowed fields =
    forM_ (KM.lookup (Key.fromText key) fields) $ \value ->
        unless (value `elem` allowed)
            $ Left
                ("Unsupported " <> key <> "; allowed values: " <> T.intercalate ", " (map render allowed))
  where
    render :: Value -> Text
    render = \case
        String value -> value
        value -> decodeUtf8 (toStrict (encode value))

numericSetting :: Text -> Int -> Int -> Object -> Either Text ()
numericSetting key lo hi fields =
    forM_ (KM.lookup (Key.fromText key) fields) $ \value -> case fromJSON @Int value of
        Success number | number >= lo && number <= hi -> Right ()
        _ -> Left (key <> " must be an integer between " <> show lo <> " and " <> show hi)

rejectSettings :: [Text] -> Object -> Either Text ()
rejectSettings allowed fields = forM_ (KM.keys fields) $ \key ->
    unless (Key.toText key `elem` allowed)
        $ Left
            ( "Unsupported setting for this model: "
                <> Key.toText key
                <> ". Use --model-options for new API settings."
            )

runProvider :: Eff '[Concurrent, Error RakeError, IOE] a -> IO (Either Text a)
runProvider action =
    first renderRakeError <$> runEff (runErrorNoCallStack @RakeError (runConcurrent action))

runMediaCli
    :: Text
    -> ([Text] -> Either Text MediaOptions)
    -> (Text -> MediaOptions -> IO (Either Text [FilePath]))
    -> IO ()
runMediaCli help parseArgs run = do
    args <- map toText <$> getArgs
    if any (`elem` ["--help", "-h"]) (takeWhile (/= "--") args)
        then putTextLn help
        else case parseArgs args of
            Left err -> die err
            Right options ->
                lookupEnv "OPENROUTER_API_KEY" >>= \case
                    Nothing -> die "OPENROUTER_API_KEY is required"
                    Just apiKey -> do
                        result <- try @IOException (run (toText apiKey) options)
                        case result of
                            Left err -> die (toText (displayException err))
                            Right (Left err) -> die err
                            Right (Right paths) -> traverse_ (putTextLn . ("Saved: " <>) . toText) paths
  where
    die :: Text -> IO a
    die err = IO.hPutStrLn IO.stderr (toString err) >> exitFailure

saveBytes :: Text -> Maybe FilePath -> Text -> [(String, BS.ByteString)] -> IO [FilePath]
saveBytes kind output prompt values = do
    paths <- buildOutputPaths (toString kind) output prompt (map fst values)
    zipWithM_ writeBinaryFile paths (map snd values)
    pure paths
