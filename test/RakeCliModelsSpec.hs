{-# LANGUAGE NoOverloadedLists #-}

module RakeCliModelsSpec (spec) where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import RakeCliModels
import RakeDecideCLI (decideHelp, decideModels)
import RakeImageCLI
import RakeTTSCLI
import RakeVideoCLI
import Test.Hspec
import Test.QuickCheck (elements, forAll, (===))
import Prelude

spec :: Spec
spec = describe "CLI model selection" $ do
    describe "resolution" $ do
        it "prefers an exact qualified match even when the model name is ambiguous" $
            resolveModel catalog (Just "second/shared")
                `shouldBe` Right (CliModel "second" "shared" 2)
        it "uses the first catalog entry by default" $
            resolveModel catalog Nothing `shouldBe` Right (CliModel "first" "shared" 1)
        it "resolves a unique bare model or a mismatched provider prefix" $ do
            resolveModel catalog (Just "unique") `shouldBe` Right (CliModel "first" "unique" 3)
            resolveModel catalog (Just "wrong/unique") `shouldBe` Right (CliModel "first" "unique" 3)
        it "reports ambiguous and unknown selections" $ do
            resolveModel catalog (Just "shared")
                `shouldBe` Left "Ambiguous model: shared. Choose one of: first/shared, second/shared"
            resolveModel catalog (Just "wrong/shared") `shouldSatisfy` isLeft
            resolveModel catalog (Just "missing") `shouldSatisfy` isLeft
            resolveModel catalog (Just "UNIQUE") `shouldSatisfy` isLeft
        it "round-trips displayed qualified names to their entries" $
            forAll (elements (NonEmpty.toList catalog)) $
                \entry -> resolveModel catalog (Just (qualifiedModel entry)) === Right entry

    describe "model arguments" $ do
        it "extracts short, long, and attached forms before or after prompts" $
            mapM_
                ( \args ->
                    parseModelArguments [] args
                        `shouldBe` Right (ModelArguments (Just "first/unique") ["prompt"] False)
                )
                [ ["-m", "first/unique", "prompt"]
                , ["prompt", "--model", "first/unique"]
                , ["prompt", "--model=first/unique"]
                , ["-mfirst/unique", "prompt"]
                ]
        it "never interprets another option's argument as a model flag" $
            parseModelArguments ["--voice"] ["--voice", "-m", "words", "--model=first/unique"]
                `shouldBe` Right (ModelArguments (Just "first/unique") ["--voice", "-m", "words"] False)
        it "preserves the end-of-options marker and following literals" $
            parseModelArguments [] ["--", "-m", "first/unique", "--help"]
                `shouldBe` Right (ModelArguments Nothing ["--", "-m", "first/unique", "--help"] False)
        it "rejects repeated or missing model values" $
            mapM_
                (\args -> parseModelArguments [] args `shouldSatisfy` isLeft)
                [["-m"], ["--model="], ["-m", "--help"], ["-m", "unique", "--model=unique"]]

    describe "catalogs and help" $ do
        it "uses the agreed defaults" $ do
            qualifiedModel (NonEmpty.head imageModels) `shouldBe` "xai/grok-imagine-image"
            qualifiedModel (NonEmpty.head videoModels) `shouldBe` "xai/grok-imagine-video"
            qualifiedModel (NonEmpty.head speechModels) `shouldBe` "xai/tts"
            qualifiedModel (NonEmpty.head decideModels) `shouldBe` "typesafe/jev-latest"
        it "lists every selectable model and marks the default" $ do
            checkHelp imageModels (renderGenImageHelp "rake-image" GenImageHelpGeneral)
            checkHelp videoModels (renderGenVideoHelp "rake-video" GenVideoHelpGeneral)
            checkHelp speechModels (renderGenSpeechHelp "rake-tts" GenSpeechHelpGeneral)
            checkHelp decideModels (Text.pack decideHelp)
        it "selects the matching provider without treating the selector as prompt text" $ do
            case parseGenImageArgs ["a cat", "-m", "wrong/gpt-image-2"] of
                ParseGenImageArgsSuccess
                    ( GenImageOpenAI
                            OpenAIGenImageOptions
                                { openAIModel
                                , openAICommonOptions = CommonGenImageOptions{commonPromptText}
                                }
                        ) -> do
                    openAIModel `shouldBe` "gpt-image-2"
                    commonPromptText `shouldBe` "a cat"
                result -> expectationFailure (show result)
            case parseGenVideoArgs ["--image", "-m", "prompt", "--model=google/veo-3.1-generate-preview"] of
                ParseGenVideoArgsSuccess (GenVideoVeo VeoGenVideoOptions{veoVideoImageSource}) -> veoVideoImageSource `shouldBe` Just "-m"
                result -> expectationFailure (show result)
        it "does not interpret literal help or model flags after --" $ do
            case parseGenImageArgs ["--", "--help", "-m", "gpt-image-2"] of
                ParseGenImageArgsSuccess
                    (GenImageXAI XAIGenImageOptions{xaiCommonOptions = CommonGenImageOptions{commonPromptText}}) -> commonPromptText `shouldBe` "--help -m gpt-image-2"
                result -> expectationFailure (show result)
        it "requires unique model selection even when asking for help" $ do
            parseGenSpeechArgs ["-m", "missing", "--help"]
                `shouldSatisfy` (\case ParseGenSpeechArgsError{} -> True; _ -> False)
            parseGenImageArgs ["-m", "gpt-image-2", "--model=gpt-image-2", "--help"]
                `shouldSatisfy` (\case ParseGenImageArgsError{} -> True; _ -> False)

catalog :: NonEmpty (CliModel Int)
catalog =
    CliModel "first" "shared" 1 :| [CliModel "second" "shared" 2, CliModel "first" "unique" 3]

checkHelp :: NonEmpty (CliModel a) -> Text -> Expectation
checkHelp models help = do
    mapM_ (\entry -> help `shouldSatisfy` Text.isInfixOf (qualifiedModel entry)) models
    help
        `shouldSatisfy` Text.isInfixOf (qualifiedModel (NonEmpty.head models) <> " (default)")
