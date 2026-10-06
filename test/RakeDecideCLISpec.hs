{-# LANGUAGE NoOverloadedLists #-}

module RakeDecideCLISpec (spec) where

import Control.Exception (bracket)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types (status200, status429)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import RakeDecideCLI
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hClose, openBinaryTempFile)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck (choose, forAll, property, (===))
import TypeSafe.Jev qualified as Jev
import Prelude

spec :: Spec
spec = describe "RakeDecideCLI" $ do
    describe "arguments" $ do
        it "takes positional choices and defaults to stdin" $ do
            options <- parsed ["choice", "approve=Ready to merge", "reject"]
            options
                `shouldBe` DecideOptions
                    { decision =
                        Choose
                            ( Jev.ChoiceQuestion
                                Nothing
                                ( Jev.ChoiceOption "approve" "approve" (Just "Ready to merge")
                                    :| [Jev.ChoiceOption "reject" "reject" Nothing]
                                )
                            )
                    , contextFiles = []
                    , jsonContext = False
                    , model = "jev-latest"
                    , baseUrl = "https://api.typesafe.ai"
                    , requestTimeout = 30
                    }
        it "accepts multiple paths and repeated context flags" $ do
            DecideOptions{contextFiles, model, requestTimeout} <-
                parsed
                    [ "choice"
                    , "yes"
                    , "no"
                    , "-c"
                    , "a.txt"
                    , "b.txt"
                    , "--model"
                    , "jev-latest"
                    , "--context"
                    , "c.txt"
                    , "--timeout"
                    , "0.5"
                    ]
            contextFiles `shouldBe` ["a.txt", "b.txt", "c.txt"]
            model `shouldBe` "jev-latest"
            requestTimeout `shouldBe` 0.5
        it "keeps option-looking question text intact" $ do
            DecideOptions{decision} <- parsed ["choice", "yes", "--question", "-c"]
            decision
                `shouldBe` Choose (Jev.ChoiceQuestion (Just "-c") (Jev.ChoiceOption "yes" "yes" Nothing :| []))
        it "treats attached context forms like separated context flags" $ do
            expected <- parsed ["choice", "yes", "no", "-c", "a.txt", "b.txt"]
            parsed ["choice", "yes", "no", "--context=a.txt", "b.txt"] `shouldReturn` expected
            parsed ["choice", "yes", "no", "-ca.txt", "b.txt"] `shouldReturn` expected
        it "supports score rubrics and noul criteria" $ do
            DecideOptions{decision = scoreDecision} <-
                parsed ["score", "Low", "High", "-q", "Urgency?"]
            scoreDecision `shouldBe` Rate (Jev.ScoreQuestion (Just "Urgency?") ("Low" :| ["High"]))
            DecideOptions{decision = noulDecision} <-
                parsed ["noul", "Spam?", "--true", "Advertising"]
            noulDecision
                `shouldBe` Ask
                    (Jev.NoulQuestion (Just "Spam?") (Just (Jev.NoulCriteria (Just "Advertising") Nothing)))
        it "rejects ambiguous or incomplete commands before reading input" $
            mapM_
                (\args -> parseDecideArgs args `shouldSatisfy` isLeft)
                [ ["unknown", "yes"]
                , ["jev", "yes"]
                , ["score"]
                , ["noul"]
                , ["noul", "Spam?", "-q", "Other?"]
                , ["choice", "yes", "--noul"]
                , ["choice", "yes", "-m", "unknown"]
                , ["choice", "yes", "-m", "jev-latest", "--model=jev-latest"]
                , ["choice", "yes", "-m"]
                , ["choice"]
                , ["choice", "same", "same=Another description"]
                , ["choice", "=No name"]
                , ["choice", "yes", "-c"]
                , ["choice", "yes", "--context", "--json"]
                , ["choice", "--score"]
                , ["noul", "Spam?", "yes"]
                , ["noul", "Spam?", "--score"]
                , ["choice", "yes", "--true", "Yes"]
                , ["choice", "yes", "--timeout", "0"]
                , ["choice", "yes", "--timeout", "NaN"]
                , ["choice", "yes", "--timeout", "Infinity"]
                , ["choice", "yes", "--timeout", "1e30"]
                , ["choice", "yes", "-c", "-", "-"]
                , ["choice", "yes", "--unknown"]
                ]
        it "shows help without requiring a provider or credentials" $ do
            parseDecideArgs ["--help"] `shouldBe` Right Nothing
            parseDecideArgs ["choice", "--help"] `shouldBe` Right Nothing
            parseDecideArgs ["-m", "typesafe/jev-latest", "--help"] `shouldBe` Right Nothing
            parseDecideArgs ["choice", "--help", "-m", "unknown"] `shouldSatisfy` isLeft
        it "accepts the requested choice syntax with a question and file context" $ do
            DecideOptions{decision, contextFiles} <-
                parsed
                    [ "choice"
                    , "Give a fuck"
                    , "Give no fuck"
                    , "-q"
                    , "Should we give a fuck?"
                    , "-c"
                    , "message.txt"
                    ]
            decision
                `shouldBe` Choose
                    ( Jev.ChoiceQuestion
                        (Just "Should we give a fuck?")
                        ( Jev.ChoiceOption "Give a fuck" "Give a fuck" Nothing
                            :| [Jev.ChoiceOption "Give no fuck" "Give no fuck" Nothing]
                        )
                    )
            contextFiles `shouldBe` ["message.txt"]
        it "resolves model flags before or after the decision type" $ do
            expected <- parsed ["choice", "yes", "no"]
            mapM_
                (\args -> parsed args `shouldReturn` expected)
                [ ["-m", "typesafe/jev-latest", "choice", "yes", "no"]
                , ["choice", "yes", "no", "-m", "jev-latest"]
                , ["choice", "--model=wrong/jev-latest", "yes", "no"]
                ]
        it "keeps model flags used as question text or literal choices" $ do
            DecideOptions{decision} <- parsed ["choice", "-q", "-m", "--", "--model=unknown"]
            decision
                `shouldBe` Choose
                    ( Jev.ChoiceQuestion
                        (Just "-m")
                        (Jev.ChoiceOption "--model" "--model" (Just "unknown") :| [])
                    )

    describe "context" $ do
        it "preserves Unicode text" $ property $ \raw ->
            let text :: Text.Text
                text = Text.pack raw
             in decodeContext False (("stdin", Text.encodeUtf8 text) :| [])
                    === Right (Jev.TextContent text)
        it "includes filenames when combining files" $
            decodeContext False (("a.txt", "first\n") :| [("b.txt", "second")])
                `shouldBe` Right
                    ( Jev.ArrayContent
                        [ JSON.object ["file" JSON..= JSON.String "a.txt", "content" JSON..= JSON.String "first\n"]
                        , JSON.object ["file" JSON..= JSON.String "b.txt", "content" JSON..= JSON.String "second"]
                        ]
                    )
        it "only parses JSON when explicitly requested" $ do
            decodeContext False (("stdin", "{\"flag\":true}") :| [])
                `shouldBe` Right "{\"flag\":true}"
            fmap JSON.toJSON (decodeContext True (("stdin", "{\"flag\":true}") :| []))
                `shouldBe` Right (JSON.object ["flag" JSON..= True])
            decodeContext True (("stdin", "true") :| []) `shouldSatisfy` isLeft
            decodeContext False (("broken.txt", BS.pack [255]) :| []) `shouldSatisfy` isLeft

    describe "executable" $ do
        it "reads stdin without -c and emits a validated choice with metadata"
            $ withServer
                "Please refund me\n"
                ( JSON.object
                    [ "type" JSON..= JSON.String "choice"
                    , "criteria" JSON..= JSON.object ["Billing" JSON..= JSON.Null, "Support" JSON..= JSON.Null]
                    ]
                )
                choiceResponse
            $ \baseUrl -> do
                (code, stdout, stderr) <-
                    cli
                        ["choice", "Billing", "Support", "--base-url", baseUrl]
                        "Please refund me\n"
                        (Just "test-key")
                code `shouldBe` ExitSuccess
                stderr `shouldBe` ""
                (JSON.eitherDecode (LBS.pack stdout) :: Either String JSON.Value)
                    `shouldBe` Right expectedChoiceOutput
        it "reads every -c file in one request without using stdin" $
            withFile "first" $ \firstPath -> withFile "second" $ \secondPath ->
                testWithApplication
                    ( pure
                        ( \request respond -> do
                            body <- Wai.strictRequestBody request
                            let expected :: JSON.Value
                                expected =
                                    JSON.object
                                        [ "state"
                                            JSON..= [ JSON.object ["file" JSON..= firstPath, "content" JSON..= JSON.String "first"]
                                                    , JSON.object ["file" JSON..= secondPath, "content" JSON..= JSON.String "second"]
                                                    ]
                                        , "model" JSON..= JSON.String "jev-latest"
                                        , "questions"
                                            JSON..= JSON.object
                                                [ "q0"
                                                    JSON..= JSON.object
                                                        ["type" JSON..= JSON.String "noul", "instructions" JSON..= JSON.String "Relevant?"]
                                                ]
                                        ]
                            JSON.eitherDecode body `shouldBe` Right expected
                            respond
                                ( Wai.responseLBS
                                    status200
                                    []
                                    ( JSON.encode
                                        (response (JSON.object ["type" JSON..= JSON.String "noul", "noul" JSON..= (0.8 :: Double)]))
                                    )
                                )
                        )
                    )
                    $ \port -> do
                        (code, stdout, stderr) <-
                            cli
                                [ "noul"
                                , "Relevant?"
                                , "-c"
                                , firstPath
                                , secondPath
                                , "--base-url"
                                , "http://127.0.0.1:" <> show port
                                ]
                                "ignored"
                                (Just "test-key")
                        code `shouldBe` ExitSuccess
                        stderr `shouldBe` ""
                        (JSON.eitherDecode (LBS.pack stdout) :: Either String Output)
                            `shouldSatisfy` either (const False) (const True)
        it "encodes numeric score indexes and fractional scores"
            $ withServer
                "state"
                ( JSON.object
                    [ "type" JSON..= JSON.String "score"
                    , "criteria" JSON..= [JSON.String "Low", JSON.String "High"]
                    ]
                )
                scoreResponse
            $ \baseUrl -> do
                (code, stdout, stderr) <-
                    cli ["score", "Low", "High", "--base-url", baseUrl] "state" (Just "test-key")
                code `shouldBe` ExitSuccess
                stderr `shouldBe` ""
                case JSON.eitherDecode @Output (LBS.pack stdout) of
                    Right Output{answers} -> case Map.lookup "result" answers of
                        Just ScoreAnswer{score, legend} -> do
                            score `shouldBe` 0.25
                            legend `shouldBe` Map.fromList [("0", "Low"), ("1", "High")]
                        _ -> expectationFailure "Expected a score answer"
                    Left err -> expectationFailure err
        it "reports missing credentials on stderr with a nonzero exit" $ do
            (code, stdout, stderr) <- cli ["choice", "yes", "no"] "state" Nothing
            code `shouldBe` ExitFailure 1
            stdout `shouldBe` ""
            stderr `shouldContain` "TYPESAFE_API_KEY"
        it "reports HTTP errors without contaminating stdout"
            $ testWithApplication
                ( pure
                    ( \_ respond ->
                        respond (Wai.responseLBS status429 [("x-typesafe-request-id", "req-123")] "rate limited")
                    )
                )
            $ \port -> do
                (code, stdout, stderr) <-
                    cli
                        ["choice", "yes", "no", "--base-url", "http://127.0.0.1:" <> show port]
                        "state"
                        (Just "test-key")
                code `shouldBe` ExitFailure 1
                stdout `shouldBe` ""
                stderr `shouldContain` "HTTP 429 (request req-123): rate limited"
        it "shows help and rejects bad arguments without credentials or input" $ do
            (code, stdout, stderr) <- cli ["--help"] "" Nothing
            code `shouldBe` ExitSuccess
            stdout `shouldContain` "rake-decide choice"
            stderr `shouldBe` ""
            (badCode, badStdout, badStderr) <- cli ["choice", "yes", "-c"] "" Nothing
            badCode `shouldBe` ExitFailure 1
            badStdout `shouldBe` ""
            badStderr `shouldContain` "requires an argument"
            (modelCode, modelStdout, modelStderr) <-
                cli ["noul", "Spam?", "-m", "unknown", "-c", "/nonexistent/context.txt"] "" Nothing
            modelCode `shouldBe` ExitFailure 1
            modelStdout `shouldBe` ""
            modelStderr `shouldContain` "Unknown model: unknown"

    describe "output codecs" $ do
        it "round-trips each answer kind and metadata" $
            forAll (choose (0, 1)) $ \probability ->
                let values :: [JSON.Value]
                    values =
                        [ expectedChoiceOutput
                        , response scoreResponse
                        , response
                            (JSON.object ["type" JSON..= JSON.String "noul", "noul" JSON..= (probability :: Double)])
                        ]
                 in map
                        ( \value -> do
                            decoded <- JSON.eitherDecode @Output (JSON.encode value)
                            roundTripped <- JSON.eitherDecode (JSON.encode decoded)
                            pure (roundTripped == decoded)
                        )
                        values
                        === replicate 3 (Right True)

parsed :: [String] -> IO DecideOptions
parsed args = case parseDecideArgs args of
    Right (Just options) -> pure options
    Right Nothing -> fail "Unexpected help"
    Left err -> fail err

cli :: [String] -> String -> Maybe String -> IO (ExitCode, String, String)
cli args input key = do
    inherited <- filter ((/= "TYPESAFE_API_KEY") . fst) <$> getEnvironment
    let environment :: [(String, String)]
        environment = maybe inherited (\value -> ("TYPESAFE_API_KEY", value) : inherited) key
    readCreateProcessWithExitCode (proc "rake-decide" args){env = Just environment} input

withFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withFile content action = do
    directory <- getTemporaryDirectory
    bracket
        ( do
            (path, handle) <- openBinaryTempFile directory "rake-decide-context.txt"
            BS.hPut handle content
            hClose handle
            pure path
        )
        removeFile
        action

withServer :: Jev.Content -> JSON.Value -> JSON.Value -> (String -> IO a) -> IO a
withServer state question answer action =
    testWithApplication
        ( pure
            ( \request respond -> do
                Wai.requestMethod request `shouldBe` "POST"
                Wai.pathInfo request `shouldBe` ["v1", "systemone"]
                lookup "Authorization" (Wai.requestHeaders request) `shouldBe` Just "Bearer test-key"
                body <- Wai.strictRequestBody request
                JSON.eitherDecode body
                    `shouldBe` Right
                        ( JSON.object
                            [ "state" JSON..= state
                            , "model" JSON..= JSON.String "jev-latest"
                            , "questions" JSON..= JSON.object ["q0" JSON..= question]
                            ]
                        )
                respond (Wai.responseLBS status200 [] (JSON.encode (response answer)))
            )
        )
        (\port -> action ("http://127.0.0.1:" <> show port))

response :: JSON.Value -> JSON.Value
response answer =
    JSON.object
        [ "model" JSON..= JSON.String "jev-test"
        , "answers" JSON..= JSON.object ["q0" JSON..= answer]
        , "usage"
            JSON..= JSON.object ["input_tokens" JSON..= (12 :: Int), "output_tokens" JSON..= (3 :: Int)]
        ]

choiceResponse :: JSON.Value
choiceResponse =
    JSON.object
        [ "type" JSON..= JSON.String "choice"
        , "choice" JSON..= JSON.String "Billing"
        , "confidence" JSON..= (0.9 :: Double)
        , "probabilities"
            JSON..= JSON.object ["Billing" JSON..= (0.8 :: Double), "Support" JSON..= (0.2 :: Double)]
        ]

scoreResponse :: JSON.Value
scoreResponse =
    JSON.object
        [ "type" JSON..= JSON.String "score"
        , "score" JSON..= (0.25 :: Double)
        , "confidence" JSON..= (0.5 :: Double)
        , "legend"
            JSON..= JSON.object ["0" JSON..= JSON.String "Low", "1" JSON..= JSON.String "High"]
        , "probabilities"
            JSON..= JSON.object ["0" JSON..= (0.75 :: Double), "1" JSON..= (0.25 :: Double)]
        ]

expectedChoiceOutput :: JSON.Value
expectedChoiceOutput =
    JSON.object
        [ "model" JSON..= JSON.String "jev-test"
        , "answers" JSON..= JSON.object ["result" JSON..= choiceResponse]
        , "usage"
            JSON..= JSON.object ["input_tokens" JSON..= (12 :: Int), "output_tokens" JSON..= (3 :: Int)]
        ]
