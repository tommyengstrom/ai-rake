{-# LANGUAGE NoOverloadedLists #-}
{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module TypeSafe.JevSpec (spec) where

import Data.Aeson
    ( FromJSON
    , Result (..)
    , ToJSON
    , Value (..)
    , eitherDecode
    , encode
    , fromJSON
    , object
    , toJSON
    , (.=)
    )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (cancel, waitCatch, withAsync)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Network.HTTP.Types (Status, mkStatus, status200, status302, status422, status429)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , choose
    , counterexample
    , forAll
    , listOf
    , oneof
    , resize
    , sized
    , (===)
    )
import TypeSafe.Jev
import TypeSafe.JevExample
import Prelude

spec :: Spec
spec = describe "TypeSafe.Jev" $ do
    describe "content and bounded numeric codecs" $ do
        it "round-trips arbitrary nested content" $
            forAll genContent $
                \content -> eitherDecode (encode content) === Right content
        it "round-trips probabilities" $
            forAll (choose (0, 1)) $
                \number -> numericRoundTrip @Probability number
        it "round-trips confidence" $
            forAll (choose (0, 1)) $
                \number -> numericRoundTrip @Confidence number
        it "rejects unsupported top-level content" $
            mapM_
                (\value -> (fromJSON value :: Result Content) `shouldSatisfy` isParseError)
                [Null, Bool True, Number 1]
        it "rejects invalid probabilities and confidence" $
            mapM_
                ( \value -> do
                    (fromJSON value :: Result Probability) `shouldSatisfy` isParseError
                    (fromJSON value :: Result Confidence) `shouldSatisfy` isParseError
                )
                [Number (-0.01), Number 1.01, String "0.5", Null, Number (10 ^ (400 :: Int))]

    describe "applicative requests" $ do
        it "sends the complete example in one request and reconstructs typed results" $ do
            count <- newIORef (0 :: Int)
            withServer
                ( \request respond -> do
                    modifyIORef' count (+ 1)
                    Wai.requestMethod request `shouldBe` "POST"
                    Wai.pathInfo request `shouldBe` ["v1", "systemone"]
                    lookup "Authorization" (Wai.requestHeaders request) `shouldBe` Just "Bearer test-key"
                    lookup "Content-Type" (Wai.requestHeaders request) `shouldBe` Just "application/json"
                    body <- Wai.strictRequestBody request
                    eitherDecode body `shouldBe` Right expectedExampleRequest
                    respond (jsonResponse status200 mixedResponse)
                )
                $ \settings -> do
                    result <- runClient settings runExample
                    case result of
                        Left err -> expectationFailure (show err)
                        Right Response{answers = Report{department, tone, severity, wantsHuman}, metadata} -> do
                            let ChoiceAssessment
                                    { chosen = ChoiceOption{value = chosenDepartment}
                                    , probabilities = departmentProbabilities
                                    } = department
                                ChoiceAssessment{chosen = ChoiceOption{value = chosenTone}} = tone
                                ScoreAssessment{scoreValue, levels, confidence} = severity
                                NoulAssessment{probability} = wantsHuman
                            chosenDepartment `shouldBe` Billing
                            chosenTone `shouldBe` Frustrated
                            fmap (\(ChoiceOption{value}, p) -> (value, probabilityValue p)) departmentProbabilities
                                `shouldBe` (Billing, 0.8) :| [(TechnicalSupport, 0.2)]
                            scoreValue `shouldBe` 1.25
                            fmap (\(description, p) -> (description, probabilityValue p)) levels
                                `shouldBe` ("low", 0)
                                    :| [(ObjectContent (KM.fromList [("description", String "medium")]), 0.75), ("high", 0.25)]
                            confidenceValue confidence `shouldBe` 0.6
                            probabilityValue probability `shouldBe` 0.98
                            metadata
                                `shouldBe` Just CallMetadata{model = "jev-test-version", inputTokens = 123, outputTokens = 45}
            readIORef count `shouldReturn` 1

        it "preserves structured state and criteria, optional fields, and prefix URLs"
            $ withServer
                ( \request respond -> do
                    Wai.pathInfo request `shouldBe` ["gateway", "v1", "systemone"]
                    body <- Wai.strictRequestBody request
                    eitherDecode body `shouldBe` Right structuredRequest
                    respond (jsonResponse status200 structuredResponse)
                )
            $ \settings@JevSettings{baseUrl} -> do
                result <-
                    runClient
                        settings{baseUrl = baseUrl <> "/gateway/", model = "pinned-model"}
                        (assess structuredState structuredBatch)
                result `shouldSatisfy` isRightResult

        it "accepts arbitrary option values without Eq, Ord, Show, or JSON instances" $
            withResponse (singleAnswer (choiceAnswer "run" [("run", 1)] 1)) $ \settings -> do
                let question :: ChoiceQuestion (Int -> Int)
                    question = ChoiceQuestion Nothing (ChoiceOption "run" (+ 1) Nothing :| [])
                result <- runClient settings (assess "state" (choice question))
                case result of
                    Right Response{answers = ChoiceAssessment{chosen = ChoiceOption{value}}} -> value 41 `shouldBe` 42
                    Left err -> expectationFailure (show err)

        it "allows distinct names for equal domain values and preserves both probabilities" $
            withResponse (singleAnswer (choiceAnswer "first" [("first", 0.6), ("second", 0.4)] 0.2)) $ \settings -> do
                let question :: ChoiceQuestion ()
                    question =
                        ChoiceQuestion
                            Nothing
                            (ChoiceOption "first" () Nothing :| [ChoiceOption "second" () Nothing])
                result <- runClient settings (assess "state" (choice question))
                case result of
                    Right Response{answers = ChoiceAssessment{probabilities}} -> NonEmpty.length probabilities `shouldBe` 2
                    Left err -> expectationFailure (show err)

        it "rejects duplicate names before HTTP" $ do
            let question :: ChoiceQuestion Int
                question =
                    ChoiceQuestion Nothing (ChoiceOption "same" 1 Nothing :| [ChoiceOption "same" 2 Nothing])
            result <- runClient unreachableSettings (assess "state" (choice question))
            result `shouldSatisfy` isInvalidRequest

        it "evaluates pure batches and empty traversals without HTTP or metadata" $ do
            runClient unreachableSettings (assess "state" (pure (42 :: Int)))
                `shouldReturn` Right Response{answers = 42, metadata = Nothing}
            runClient unreachableSettings (assess "state" (traverse noul ([] :: [NoulQuestion])))
                `shouldReturn` Right Response{answers = [], metadata = Nothing}

        it "keeps dynamic traversals in one call with stable IDs"
            $ withServer
                ( \request respond -> do
                    body <- Wai.strictRequestBody request
                    let parsed :: Either String Value
                        parsed = eitherDecode body
                    fmap (lookupPath ["questions"]) parsed
                        `shouldBe` Right
                            ( Just
                                ( object
                                    [ Key.fromText ("q" <> Text.pack (show index)) .= object ["type" .= String "noul"]
                                    | index <- [0 :: Int .. 9]
                                    ]
                                )
                            )
                    respond
                        ( jsonResponse
                            status200
                            ( responseWithAnswers
                                [ ("q" <> Text.pack (show index), noulAnswer (fromIntegral index / 10))
                                | index <- [0 :: Int .. 9]
                                ]
                            )
                        )
                )
            $ \settings -> do
                result <-
                    runClient
                        settings
                        (assess "state" (traverse noul (replicate 10 (NoulQuestion Nothing Nothing))))
                fmap
                    ( \Response{answers} -> map (\NoulAssessment{probability} -> probabilityValue probability) answers
                    )
                    result
                    `shouldBe` Right [fromIntegral index / 10 | index <- [0 :: Int .. 9]]

        it "supports applicative composition and local values without extra questions" $
            withResponse (singleAnswer (noulAnswer 0.75)) $ \settings -> do
                let batch :: Batch (Text, Double)
                    batch =
                        (,)
                            <$> pure "local"
                            <*> (probabilityValue . noulProbability <$> noul (NoulQuestion Nothing Nothing))
                result <- runClient settings (assess "state" batch)
                fmap (\Response{answers} -> answers) result `shouldBe` Right ("local", 0.75)

    describe "response validation" $ do
        mapM_
            ( \(description, response) ->
                it description $ withResponse response $ \settings -> do
                    result <- runClient settings runExample
                    result `shouldSatisfy` isInvalidResponse
            )
            malformedResponses
        it "accepts rounded distributions without recomputing confidence or score"
            $ withResponse
                ( singleAnswer
                    ( object
                        [ "type" .= String "score"
                        , "score" .= (1.2 :: Double)
                        , "legend" .= object ["0" .= String "a", "1" .= String "b", "2" .= String "c"]
                        , "probabilities"
                            .= object ["0" .= (0.33 :: Double), "1" .= (0.33 :: Double), "2" .= (0.33 :: Double)]
                        , "confidence" .= (0.123 :: Double)
                        ]
                    )
                )
            $ \settings -> do
                result <- runClient settings (assess "state" (score severityQuestion))
                case result of
                    Right Response{answers = ScoreAssessment{scoreValue, confidence}} -> do
                        scoreValue `shouldBe` 1.2
                        confidenceValue confidence `shouldBe` 0.123
                    Left err -> expectationFailure (show err)

    describe "HTTP interpreter" $ do
        it "does not retry a POST when a reused connection closes before its response" $ do
            count <- newIORef (0 :: Int)
            peers <- newIORef []
            withServer
                ( \request respond -> do
                    _ <- Wai.strictRequestBody request
                    number <- atomicModifyIORef' count (\previous -> (previous + 1, previous + 1))
                    modifyIORef' peers (Wai.remoteHost request :)
                    if number == 1
                        then respond (jsonResponse status200 (singleAnswer (noulAnswer 1)))
                        else respond (Wai.responseRaw (\_ _ -> pure ()) (Wai.responseLBS status200 [] ""))
                )
                $ \settings -> do
                    result <- runClient settings do
                        _ <- assess "first" (noul humanQuestion)
                        assess "second" (noul humanQuestion)
                    case result of
                        Left (TransportFailure _) -> pure ()
                        Left err -> expectationFailure (show err)
                        Right _ -> expectationFailure "Expected the closed connection to fail"
            readIORef count `shouldReturn` 2
            readIORef peers >>= \case
                [first, second] -> first `shouldBe` second
                _ -> expectationFailure "Expected two requests over the same connection"

        mapM_
            ( \status ->
                it ("preserves HTTP " <> show status <> " without retries") $ do
                    count <- newIORef (0 :: Int)
                    withServer
                        ( \_ respond -> do
                            modifyIORef' count (+ 1)
                            respond
                                ( Wai.responseLBS
                                    status
                                    [("x-typesafe-request-id", "request-123"), ("Location", "http://127.0.0.1:1/redirect")]
                                    "failure-body"
                                )
                        )
                        $ \settings -> do
                            result <- runClient settings (assess "state" (noul humanQuestion))
                            case result of
                                Left HttpFailure{statusCode, responseBody, requestId} -> do
                                    statusCode `shouldBe` HTTP.statusCode status
                                    responseBody `shouldBe` "failure-body"
                                    requestId `shouldBe` Just "request-123"
                                Left err -> expectationFailure (show err)
                                Right _ -> expectationFailure "Expected HTTP failure"
                    readIORef count `shouldReturn` 1
            )
            [status302, status422, status429, mkStatus 529 "Overloaded"]

        it "reports invalid JSON as an invalid response" $
            withServer (\_ respond -> respond (Wai.responseLBS status200 [] "not-json")) $ \settings -> do
                result <- runClient settings (assess "state" (noul humanQuestion))
                result `shouldSatisfy` isInvalidResponse

        it "reports connection failures without leaking authorization headers" $ do
            result <- runClient unreachableSettings (assess "state" (noul humanQuestion))
            case result of
                Left (TransportFailure message) -> Text.isInfixOf "secret-test-key" message `shouldBe` False
                Left err -> expectationFailure (show err)
                Right _ -> expectationFailure "Expected connection failure"

        it "rejects invalid settings before HTTP" $
            mapM_
                ( \settings -> do
                    result <- runClient settings (assess "state" (noul humanQuestion))
                    result `shouldSatisfy` isInvalidRequest
                )
                [ unreachableSettings{baseUrl = "not a URL"}
                , unreachableSettings{baseUrl = "http://127.0.0.1:1?query=yes"}
                , unreachableSettings{requestTimeout = 0}
                , unreachableSettings{requestTimeout = -1}
                , unreachableSettings{apiKey = "secret\n"}
                , unreachableSettings{apiKey = "secret\r"}
                , unreachableSettings{apiKey = "secret\0"}
                , unreachableSettings{apiKey = "secret\DEL"}
                ]

        it "bounds the whole HTTP request with the configured timeout"
            $ withServer
                ( \_ respond -> do
                    runEff (runConcurrent (threadDelay 500_000))
                    respond (jsonResponse status200 (singleAnswer (noulAnswer 1)))
                )
            $ \settings -> do
                result <- runClient settings{requestTimeout = 0.05} (assess "state" (noul humanQuestion))
                result `shouldBe` Left (RequestTimedOut 0.05)

        it "preserves asynchronous cancellation during an HTTP request" $ do
            entered <- newIORef False
            withServer
                ( \_ respond -> do
                    modifyIORef' entered (const True)
                    runEff (runConcurrent (threadDelay 1_000_000))
                    respond (jsonResponse status200 (singleAnswer (noulAnswer 1)))
                )
                $ \settings -> runEff $
                    runConcurrent $
                        withAsync (liftIO (runClient settings (assess "state" (noul humanQuestion)))) $ \worker -> do
                            waitForRequest entered
                            cancel worker
                            result <- waitCatch worker
                            liftIO (result `shouldSatisfy` isLeft)

runClient :: JevSettings -> Eff '[Jev, Error JevError, IOE] a -> IO (Either JevError a)
runClient settings = runEff . runErrorNoCallStack @JevError . runJev settings

unreachableSettings :: JevSettings
unreachableSettings =
    (defaultJevSettings "secret-test-key"){baseUrl = "http://127.0.0.1:1", requestTimeout = 1}

withServer :: Wai.Application -> (JevSettings -> IO a) -> IO a
withServer application action = testWithApplication (pure application) $ \port ->
    action
        (defaultJevSettings "test-key"){baseUrl = "http://127.0.0.1:" <> Text.pack (show port)}

withResponse :: Value -> (JevSettings -> IO a) -> IO a
withResponse response = withServer (\_ respond -> respond (jsonResponse status200 response))

jsonResponse :: Status -> Value -> Wai.Response
jsonResponse status = Wai.responseLBS status [("Content-Type", "application/json")] . encode

isParseError :: Result a -> Bool
isParseError = \case
    Error _ -> True
    Success _ -> False

isInvalidRequest :: Either JevError a -> Bool
isInvalidRequest = \case
    Left (InvalidRequest _) -> True
    Left _ -> False
    Right _ -> False

isInvalidResponse :: Either JevError a -> Bool
isInvalidResponse = \case
    Left (InvalidResponse _) -> True
    Left _ -> False
    Right _ -> False

isRightResult :: Either a b -> Bool
isRightResult = \case
    Left _ -> False
    Right _ -> True

numericRoundTrip :: forall a. (FromJSON a, ToJSON a, Eq a, Show a) => Double -> Property
numericRoundTrip number = case fromJSON (toJSON number) of
    Error err -> counterexample err False
    Success (value :: a) -> eitherDecode (encode value) === Right value

genContent :: Gen Content
genContent =
    oneof
        [ TextContent . Text.pack <$> arbitrary
        , ObjectContent . KM.fromList
            <$> listOf ((,) <$> (Key.fromString <$> arbitrary) <*> resize 3 genValue)
        , ArrayContent <$> listOf (resize 3 genValue)
        ]

genValue :: Gen Value
genValue = sized $ \size ->
    if size <= 0
        then
            oneof
                [ pure Null
                , Bool <$> arbitrary
                , String . Text.pack <$> arbitrary
                , toJSON <$> (arbitrary :: Gen Int)
                ]
        else
            oneof
                [ resize 0 genValue
                , Array . Vector.fromList <$> listOf (resize (size `div` 2) genValue)
                , Object . KM.fromList
                    <$> listOf ((,) <$> (Key.fromString <$> arbitrary) <*> resize (size `div` 2) genValue)
                ]

noulProbability :: NoulAssessment -> Probability
noulProbability NoulAssessment{probability} = probability

waitForRequest :: forall es. (Concurrent :> es, IOE :> es) => IORef Bool -> Eff es ()
waitForRequest entered = go 5_000
  where
    go :: Int -> Eff es ()
    go remaining = do
        ready <- liftIO (readIORef entered)
        if ready
            then pure ()
            else
                if remaining <= 0
                    then liftIO (expectationFailure "HTTP request did not reach the fixture server")
                    else threadDelay 1_000 >> go (remaining - 1)

responseWithAnswers :: [(Text, Value)] -> Value
responseWithAnswers answers =
    object
        [ "model" .= String "jev-test-version"
        , "answers" .= object [Key.fromText key .= value | (key, value) <- answers]
        , "usage" .= object ["input_tokens" .= (123 :: Int), "output_tokens" .= (45 :: Int)]
        ]

singleAnswer :: Value -> Value
singleAnswer answer = responseWithAnswers [("q0", answer)]

noulAnswer :: Double -> Value
noulAnswer probability = object ["type" .= String "noul", "noul" .= probability]

choiceAnswer :: Text -> [(Text, Double)] -> Double -> Value
choiceAnswer chosen probabilities confidence =
    object
        [ "type" .= String "choice"
        , "choice" .= chosen
        , "probabilities"
            .= object [Key.fromText name .= probability | (name, probability) <- probabilities]
        , "confidence" .= confidence
        ]

mixedResponse :: Value
mixedResponse =
    responseWithAnswers
        [ ("q3", noulAnswer 0.98)
        ,
            ( "q2"
            , object
                [ "type" .= String "score"
                , "score" .= (1.25 :: Double)
                , "legend"
                    .= object
                        [ "2" .= String "high"
                        , "0" .= String "low"
                        , "1" .= object ["description" .= String "medium"]
                        ]
                , "probabilities"
                    .= object ["2" .= (0.25 :: Double), "0" .= (0 :: Double), "1" .= (0.75 :: Double)]
                , "confidence" .= (0.6 :: Double)
                ]
            )
        , ("q1", choiceAnswer "Frustrated" [("Angry", 0.1), ("Frustrated", 0.7), ("Calm", 0.2)] 0.4)
        , ("q0", choiceAnswer "Billing" [("TechnicalSupport", 0.2), ("Billing", 0.8)] 0.5)
        ]

expectedExampleRequest :: Value
expectedExampleRequest =
    object
        [ "state" .= customerState
        , "model" .= String "jev-latest"
        , "questions"
            .= object
                [ "q0"
                    .= object
                        [ "type" .= String "choice"
                        , "instructions" .= String "Which department should handle this?"
                        , "criteria"
                            .= object
                                [ "Billing" .= String "Payments, invoices, subscriptions, and refunds"
                                , "TechnicalSupport" .= String "Bugs, outages, and problems using the product"
                                ]
                        ]
                , "q1"
                    .= object
                        [ "type" .= String "choice"
                        , "instructions" .= String "What is the customer's tone?"
                        , "criteria"
                            .= object
                                [ "Calm" .= String "Neutral, patient, or satisfied"
                                , "Frustrated" .= String "Dissatisfied or impatient, but civil"
                                , "Angry" .= String "Hostile or strongly confrontational"
                                ]
                        ]
                , "q2"
                    .= object
                        [ "type" .= String "score"
                        , "instructions" .= String "How severe is the customer's problem?"
                        , "criteria"
                            .= ( [ "Minor inconvenience with no financial or functional impact"
                                 , "Financial impact or impaired functionality, but the product remains usable"
                                 , "The customer cannot use the product at all"
                                 ]
                                    :: [Text]
                               )
                        ]
                , "q3"
                    .= object
                        [ "type" .= String "noul"
                        , "instructions" .= String "Is the customer explicitly asking to speak to a person?"
                        , "criteria"
                            .= object
                                [ "true" .= String "Explicitly requests a human, agent, or representative"
                                , "false" .= String "Asks for help without requesting a person"
                                ]
                        ]
                ]
        ]

structuredState :: Content
structuredState =
    ObjectContent
        (KM.fromList [("message", String "hello"), ("count", Number 2), ("active", Bool True)])

structuredBatch :: Batch (ChoiceAssessment Bool, ScoreAssessment, NoulAssessment)
structuredBatch =
    (,,)
        <$> choice
            ( ChoiceQuestion
                Nothing
                ( ChoiceOption "yes" True Nothing
                    :| [ChoiceOption "no" False (Just (ArrayContent [String "negative", Number 1]))]
                )
            )
        <*> score
            ScoreQuestion
                { instructions = Just (ObjectContent (KM.fromList [("question", String "rate")]))
                , levels = ObjectContent (KM.fromList [("level", String "only")]) :| []
                }
        <*> noul (NoulQuestion Nothing (Just (NoulCriteria Nothing (Just "absent"))))

structuredRequest :: Value
structuredRequest =
    object
        [ "state" .= structuredState
        , "model" .= String "pinned-model"
        , "questions"
            .= object
                [ "q0"
                    .= object
                        [ "type" .= String "choice"
                        , "criteria" .= object ["yes" .= Null, "no" .= [String "negative", Number 1]]
                        ]
                , "q1"
                    .= object
                        [ "type" .= String "score"
                        , "instructions" .= object ["question" .= String "rate"]
                        , "criteria" .= [object ["level" .= String "only"]]
                        ]
                , "q2"
                    .= object ["type" .= String "noul", "criteria" .= object ["false" .= String "absent"]]
                ]
        ]

structuredResponse :: Value
structuredResponse =
    responseWithAnswers
        [ ("q0", choiceAnswer "yes" [("yes", 1), ("no", 0)] 1)
        ,
            ( "q1"
            , object
                [ "type" .= String "score"
                , "score" .= (0 :: Int)
                , "legend" .= object ["0" .= object ["level" .= String "only"]]
                , "probabilities" .= object ["0" .= (1 :: Int)]
                , "confidence" .= (1 :: Int)
                ]
            )
        , ("q2", noulAnswer 0)
        ]

malformedResponses :: [(String, Value)]
malformedResponses =
    [ ("rejects missing answers", deletePath ["answers", "q1"] mixedResponse)
    , ("rejects extra answers", setPath ["answers", "q4"] (noulAnswer 1) mixedResponse)
    ,
        ( "rejects mismatched answer kinds"
        , setPath ["answers", "q0"] (noulAnswer 1) mixedResponse
        )
    ,
        ( "rejects unknown answer kinds"
        , setPath ["answers", "q0", "type"] (String "other") mixedResponse
        )
    ,
        ( "rejects an unoffered constructor"
        , setPath ["answers", "q0", "choice"] (String "Sales") mixedResponse
        )
    ,
        ( "rejects missing choice probabilities"
        , deletePath ["answers", "q0", "probabilities", "Billing"] mixedResponse
        )
    ,
        ( "rejects extra choice probabilities"
        , setPath ["answers", "q0", "probabilities", "Sales"] (Number 0) mixedResponse
        )
    ,
        ( "rejects invalid probabilities"
        , setPath ["answers", "q0", "probabilities", "Billing"] (Number 2) mixedResponse
        )
    ,
        ( "rejects invalid confidence"
        , setPath ["answers", "q0", "confidence"] (Number (-1)) mixedResponse
        )
    , ("rejects invalid nouls", setPath ["answers", "q3", "noul"] (Number 2) mixedResponse)
    , ("rejects missing confidence", deletePath ["answers", "q2", "confidence"] mixedResponse)
    ,
        ( "rejects scores outside the rubric"
        , setPath ["answers", "q2", "score"] (Number 3) mixedResponse
        )
    ,
        ( "rejects negative scores"
        , setPath ["answers", "q2", "score"] (Number (-1)) mixedResponse
        )
    ,
        ( "rejects nonfinite decoded scores"
        , setPath ["answers", "q2", "score"] (Number (10 ^ (400 :: Int))) mixedResponse
        )
    ,
        ( "rejects missing score probabilities"
        , deletePath ["answers", "q2", "probabilities", "1"] mixedResponse
        )
    ,
        ( "rejects extra score indexes"
        , setPath ["answers", "q2", "legend", "3"] (String "extra") mixedResponse
        )
    ,
        ( "rejects missing score indexes"
        , deletePath ["answers", "q2", "legend", "1"] mixedResponse
        )
    ,
        ( "rejects null score descriptions"
        , setPath ["answers", "q2", "legend", "1"] Null mixedResponse
        )
    , ("rejects missing metadata", deletePath ["usage"] mixedResponse)
    ,
        ( "rejects negative token usage"
        , setPath ["usage", "input_tokens"] (Number (-1)) mixedResponse
        )
    ]

lookupPath :: [Text] -> Value -> Maybe Value
lookupPath path value = case path of
    [] -> Just value
    key : rest -> case value of
        Object fields -> KM.lookup (Key.fromText key) fields >>= lookupPath rest
        _ -> Nothing

setPath :: [Text] -> Value -> Value -> Value
setPath path replacement value = case path of
    [] -> replacement
    key : rest -> case value of
        Object fields ->
            Object
                ( runIdentity $
                    KM.alterF
                        (Identity . Just . setPath rest replacement . maybe (Object KM.empty) id)
                        (Key.fromText key)
                        fields
                )
        _ -> value

deletePath :: [Text] -> Value -> Value
deletePath path value = case path of
    [] -> Null
    [key] -> case value of
        Object fields -> Object (KM.delete (Key.fromText key) fields)
        _ -> value
    key : rest -> case value of
        Object fields ->
            Object
                (runIdentity (KM.alterF (Identity . fmap (deletePath rest)) (Key.fromText key) fields))
        _ -> value
