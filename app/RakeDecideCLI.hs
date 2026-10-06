{-# LANGUAGE NoOverloadedLists #-}

module RakeDecideCLI
    ( DecideOptions (..)
    , Decision (..)
    , Answer (..)
    , Output (..)
    , Usage (..)
    , decideModels
    , parseDecideArgs
    , decodeContext
    , executeDecision
    , renderError
    , decideHelp
    , runDecideCli
    ) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless, when)
import Data.Aeson qualified as JSON
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.IO qualified as Text
import Data.Time (NominalDiffTime)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import GHC.Generics (Generic)
import RakeCliModels (CliModel (..), modelHelpLines, resolveModel)
import System.Console.GetOpt qualified as Opt
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Read (readMaybe)
import TypeSafe.Jev qualified as Jev
import Prelude

data Decision
    = Choose (Jev.ChoiceQuestion Text)
    | Rate Jev.ScoreQuestion
    | Ask Jev.NoulQuestion
    deriving stock (Show, Eq)

data DecideOptions = DecideOptions
    { decision :: Decision
    , contextFiles :: [FilePath]
    , jsonContext :: Bool
    , model :: Text
    , baseUrl :: Text
    , requestTimeout :: NominalDiffTime
    }
    deriving stock (Show, Eq)

data Answer
    = ChoiceAnswer
        {choice :: Text, confidence :: Jev.Confidence, probabilities :: Map Text Jev.Probability}
    | ScoreAnswer
        { score :: Double
        , confidence :: Jev.Confidence
        , legend :: Map Text Jev.Content
        , probabilities :: Map Text Jev.Probability
        }
    | NoulAnswer {noul :: Jev.Probability}
    deriving stock (Show, Eq, Generic)

data Usage = Usage {inputTokens :: Int, outputTokens :: Int}
    deriving stock (Show, Eq, Generic)

data Output = Output {model :: Maybe Text, answers :: Map Text Answer, usage :: Maybe Usage}
    deriving stock (Show, Eq, Generic)

jsonOptions :: JSON.Options
jsonOptions =
    JSON.defaultOptions
        { JSON.sumEncoding = JSON.TaggedObject "type" "contents"
        , JSON.constructorTagModifier = map toLower . takeWhile (/= 'A')
        , JSON.omitNothingFields = True
        }

instance JSON.FromJSON Answer where
    parseJSON = JSON.genericParseJSON jsonOptions
instance JSON.ToJSON Answer where
    toJSON = JSON.genericToJSON jsonOptions
instance JSON.FromJSON Usage where
    parseJSON = JSON.genericParseJSON jsonOptions{JSON.fieldLabelModifier = JSON.camelTo2 '_'}
instance JSON.ToJSON Usage where
    toJSON = JSON.genericToJSON jsonOptions{JSON.fieldLabelModifier = JSON.camelTo2 '_'}
instance JSON.FromJSON Output where
    parseJSON = JSON.genericParseJSON jsonOptions
instance JSON.ToJSON Output where
    toJSON = JSON.genericToJSON jsonOptions

data Flag
    = Context FilePath
    | JsonContext
    | Model String
    | BaseUrl String
    | Timeout String
    | Question String
    | WhenTrue String
    | WhenFalse String
    | Help
    | Argument String
    deriving stock (Eq)

flags :: [Opt.OptDescr Flag]
flags =
    [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "Show help"
    , Opt.Option
        ['c']
        ["context"]
        (Opt.ReqArg Context "FILE...")
        "Read context files; consumes paths up to the next option"
    , Opt.Option
        []
        ["json"]
        (Opt.NoArg JsonContext)
        "Parse each context as JSON (string, object, or array)"
    , Opt.Option
        ['q']
        ["question"]
        (Opt.ReqArg Question "TEXT")
        "Instructions for a choice or score question"
    , Opt.Option
        []
        ["true"]
        (Opt.ReqArg WhenTrue "TEXT")
        "Criteria for a yes answer (noul only)"
    , Opt.Option
        []
        ["false"]
        (Opt.ReqArg WhenFalse "TEXT")
        "Criteria for a no answer (noul only)"
    , Opt.Option
        ['m']
        ["model"]
        (Opt.ReqArg Model "MODEL")
        "Select a supported model and its provider"
    , Opt.Option
        []
        ["base-url"]
        (Opt.ReqArg BaseUrl "URL")
        "API base URL (default: https://api.typesafe.ai)"
    , Opt.Option [] ["timeout"] (Opt.ReqArg Timeout "SECONDS") "Request timeout (default: 30)"
    ]

decideHelp :: String
decideHelp =
    Opt.usageInfo
        ( unlines
            [ "Usage: rake-decide choice CHOICE... [-q QUESTION] [-c FILE...] [OPTIONS]"
            , "       rake-decide score LEVEL... [-q QUESTION] [-c FILE...] [OPTIONS]"
            , "       rake-decide noul QUESTION [-c FILE...] [OPTIONS]"
            , ""
            , "Reads context from stdin when -c is omitted. Use - as a path for stdin."
            , "Choices are NAME or NAME=DESCRIPTION. Put choices before -c."
            , "Multiple files are sent together with their filenames in one request."
            , "Reads TYPESAFE_API_KEY. Writes JSON to stdout and errors to stderr."
            , "The answer is under answers.result, alongside model and token usage."
            , ""
            , "Examples:"
            , "  rake-decide choice Billing Support Sales -c message.txt"
            , "  cat message.txt | rake-decide choice Billing Support Sales"
            , "  rake-decide choice approve reject -q 'Should we merge?' -c diff.txt notes.txt"
            , "  rake-decide score Low Medium High -q 'How urgent?' -c message.txt"
            , "  rake-decide noul 'Is this spam?' -c message.txt"
            ]
            <> unlines (map Text.unpack (modelHelpLines decideModels))
            <> "\n"
        )
        flags

decideModels :: NonEmpty (CliModel ())
decideModels = CliModel "typesafe" "jev-latest" () :| []

parseDecideArgs :: [String] -> Either String (Maybe DecideOptions)
parseDecideArgs = \case
    [] -> Right Nothing
    args -> do
        let (rawOptions, _, errors) = Opt.getOpt (Opt.ReturnInOrder Argument) flags args
            options :: [Flag]
            options = collectContexts rawOptions
            values :: (Flag -> Maybe String) -> [String]
            values pick = [value | Just value <- map pick options]
            positional :: [String]
            positional = values (\case Argument value -> Just value; _ -> Nothing)
            single :: String -> [String] -> Either String (Maybe String)
            single name = \case
                [] -> Right Nothing
                [value] -> Right (Just value)
                _ -> Left (name <> " may only be supplied once")
        unless (null errors) (Left (concat errors))
        selector <- single "--model" (values (\case Model value -> Just value; _ -> Nothing))
        CliModel{modelName = model} <-
            first Text.unpack (resolveModel decideModels (Text.pack <$> selector))
        if Help `elem` options
            then Right Nothing
            else do
                (command, entries) <- case positional of
                    command : entries | command `elem` ["choice", "score", "noul"] -> Right (command, entries)
                    _ -> Left "Choose a decision type: choice, score, or noul"
                baseUrl <-
                    Text.pack . fromMaybe "https://api.typesafe.ai"
                        <$> single "--base-url" (values (\case BaseUrl value -> Just value; _ -> Nothing))
                timeout <- single "--timeout" (values (\case Timeout value -> Just value; _ -> Nothing))
                requestTimeout <- maybe (Right 30) parseTimeout timeout
                instructions <-
                    fmap (Jev.TextContent . Text.pack)
                        <$> single "--question" (values (\case Question value -> Just value; _ -> Nothing))
                whenTrue <-
                    fmap (Jev.TextContent . Text.pack)
                        <$> single "--true" (values (\case WhenTrue value -> Just value; _ -> Nothing))
                whenFalse <-
                    fmap (Jev.TextContent . Text.pack)
                        <$> single "--false" (values (\case WhenFalse value -> Just value; _ -> Nothing))
                let contextFiles :: [FilePath]
                    contextFiles = values (\case Context value -> Just value; _ -> Nothing)
                    hasNoulCriteria :: Bool
                    hasNoulCriteria = isJust whenTrue || isJust whenFalse
                when (length (filter (== "-") contextFiles) > 1) (Left "Stdin (-) may only be used once")
                when
                    (any (\path -> null path || (path /= "-" && "-" `isPrefixOf` path)) contextFiles)
                    (Left "--context requires a file; prefix filenames starting with '-' with './'")
                decision <- case command of
                    "noul" -> do
                        when
                            (isJust instructions)
                            (Left "noul takes its question as a positional argument, not -q")
                        question <- case entries of
                            [value] -> Right (Jev.TextContent (Text.pack value))
                            _ -> Left "noul requires exactly one positional question"
                        pure
                            ( Ask
                                ( Jev.NoulQuestion
                                    (Just question)
                                    (if hasNoulCriteria then Just (Jev.NoulCriteria whenTrue whenFalse) else Nothing)
                                )
                            )
                    _ -> do
                        when hasNoulCriteria (Left "--true and --false are only available for noul")
                        nonEmptyEntries <-
                            maybe
                                (Left "Supply at least one choice or score level")
                                Right
                                (NonEmpty.nonEmpty entries)
                        if command == "score"
                            then
                                pure
                                    (Rate (Jev.ScoreQuestion instructions (fmap (Jev.TextContent . Text.pack) nonEmptyEntries)))
                            else do
                                choices <- traverse parseChoice nonEmptyEntries
                                let names :: [Text]
                                    names = [name | Jev.ChoiceOption{name} <- NonEmpty.toList choices]
                                when
                                    (Map.size (Map.fromList [(name, ()) | name <- names]) /= length names)
                                    (Left "Choice names must be unique")
                                pure (Choose (Jev.ChoiceQuestion instructions choices))
                pure
                    ( Just
                        DecideOptions
                            { decision
                            , contextFiles
                            , jsonContext = JsonContext `elem` options
                            , model
                            , baseUrl
                            , requestTimeout
                            }
                    )

collectContexts :: [Flag] -> [Flag]
collectContexts = \case
    [] -> []
    Context path : rest -> Context path : followingPaths rest
    flag : rest -> flag : collectContexts rest
  where
    followingPaths :: [Flag] -> [Flag]
    followingPaths = \case
        Argument path : rest -> Context path : followingPaths rest
        remaining -> collectContexts remaining

parseTimeout :: String -> Either String NominalDiffTime
parseTimeout raw = case readMaybe @Double raw of
    Just value
        | not (isNaN value || isInfinite value)
        , value > 0
        , toRational value * 1_000_000 <= toRational (maxBound :: Int)
        , realToFrac value > (0 :: NominalDiffTime) ->
            Right (realToFrac value)
    _ ->
        Left "--timeout must be a positive, finite number of seconds that fits in microseconds"

parseChoice :: String -> Either String (Jev.ChoiceOption Text)
parseChoice raw = do
    let (name, suffix) = Text.breakOn "=" (Text.pack raw)
    when (Text.null name) (Left "Choice names must not be empty")
    pure
        ( Jev.ChoiceOption
            name
            name
            (if Text.null suffix then Nothing else Just (Jev.TextContent (Text.drop 1 suffix)))
        )

decodeContext :: Bool -> NonEmpty (Text, BS.ByteString) -> Either Text Jev.Content
decodeContext jsonContext sources = do
    decoded <-
        traverse
            ( \(name, bytes) ->
                (name,)
                    <$> first
                        ((name <> ": ") <>)
                        ( if jsonContext
                            then first Text.pack (JSON.eitherDecodeStrict' bytes)
                            else Jev.TextContent <$> first (Text.pack . displayException) (Text.decodeUtf8' bytes)
                        )
            )
            sources
    pure $ case decoded of
        (_, content) :| [] -> content
        _ :| (_ : _) ->
            Jev.ArrayContent
                [ JSON.object ["file" JSON..= name, "content" JSON..= content]
                | (name, content) <- NonEmpty.toList decoded
                ]

executeDecision
    :: Jev.JevSettings -> Jev.Content -> Decision -> IO (Either Jev.JevError Output)
executeDecision settings state decision = do
    result <-
        runEff . runErrorNoCallStack @Jev.JevError $
            Jev.runJev settings (Jev.assess state (decisionBatch decision))
    pure $
        fmap
            ( \Jev.Response{answers = answer, metadata} ->
                let answers :: Map Text Answer
                    answers = Map.singleton "result" answer
                 in case metadata of
                        Nothing -> Output{answers, model = Nothing, usage = Nothing}
                        Just Jev.CallMetadata{model, inputTokens, outputTokens} -> Output{answers, model = Just model, usage = Just (Usage inputTokens outputTokens)}
            )
            result

decisionBatch :: Decision -> Jev.Batch Answer
decisionBatch = \case
    Choose question ->
        ( \Jev.ChoiceAssessment{chosen = Jev.ChoiceOption{name}, confidence, probabilities} ->
            ChoiceAnswer
                name
                confidence
                ( Map.fromList
                    [ (optionName, probability)
                    | (Jev.ChoiceOption{name = optionName}, probability) <- NonEmpty.toList probabilities
                    ]
                )
        )
            <$> Jev.choice question
    Rate question ->
        ( \Jev.ScoreAssessment{scoreValue, confidence, levels} ->
            let indexed :: [(Text, (Jev.Content, Jev.Probability))]
                indexed = zip (map (Text.pack . show) [0 :: Int ..]) (NonEmpty.toList levels)
             in ScoreAnswer
                    scoreValue
                    confidence
                    (Map.fromList [(index, content) | (index, (content, _)) <- indexed])
                    (Map.fromList [(index, probability) | (index, (_, probability)) <- indexed])
        )
            <$> Jev.score question
    Ask question ->
        (\Jev.NoulAssessment{probability} -> NoulAnswer probability) <$> Jev.noul question

renderError :: Jev.JevError -> Text
renderError = \case
    Jev.InvalidRequest message -> "Invalid request: " <> message
    Jev.InvalidResponse message -> "Invalid response: " <> message
    Jev.TransportFailure message -> "Transport failure: " <> message
    Jev.RequestTimedOut seconds -> "Request timed out after " <> Text.pack (show seconds)
    Jev.HttpFailure{statusCode, responseBody, requestId} ->
        "HTTP "
            <> Text.pack (show statusCode)
            <> maybe "" (\value -> " (request " <> value <> ")") requestId
            <> ": "
            <> Text.decodeUtf8With lenientDecode (LBS.toStrict responseBody)

runDecideCli :: IO ()
runDecideCli = do
    args <- getArgs
    case parseDecideArgs args of
        Left message -> failCli (Text.pack message <> "\nRun rake-decide --help for usage.")
        Right Nothing -> putStr decideHelp
        Right
            (Just DecideOptions{decision, contextFiles, jsonContext, model, baseUrl, requestTimeout}) -> do
                outcome <- try @IOException $ do
                    key <-
                        lookupEnv "TYPESAFE_API_KEY" >>= \case
                            Just value | not (null value) -> pure (Text.pack value)
                            _ -> failCli "Set TYPESAFE_API_KEY to your TypeSafe API key."
                    let readSource :: FilePath -> IO (Text, BS.ByteString)
                        readSource path =
                            (Text.pack path,) <$> case path of
                                "-" -> BS.getContents
                                _ -> BS.readFile path
                    sources <- traverse readSource (fromMaybe ("-" :| []) (NonEmpty.nonEmpty contextFiles))
                    state <- either failCli pure (decodeContext jsonContext sources)
                    let settings :: Jev.JevSettings
                        settings =
                            (Jev.defaultJevSettings key)
                                { Jev.baseUrl = baseUrl
                                , Jev.requestTimeout = requestTimeout
                                , Jev.model = model
                                }
                    result <- executeDecision settings state decision
                    output <- either (failCli . renderError) pure result
                    LBS.putStr (JSON.encode output <> "\n")
                either (failCli . Text.pack . displayException) pure outcome

failCli :: Text -> IO a
failCli message = Text.hPutStrLn stderr message >> exitFailure
