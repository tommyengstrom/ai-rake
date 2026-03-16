module GenImageCLISpec where

import GenImageCLI
import Data.Text qualified as T
import Relude
import Test.Hspec

spec :: Spec
spec = describe "GenImageCLI" $ do
    describe "parseGenImageArgs" $ do
        it "requires a provider" $ do
            parseGenImageArgs []
                `shouldBe` ParseGenImageArgsError
                    "A provider is required. Use `openai` or `grok`."
                    GenImageHelpGeneral

        it "parses a minimal OpenAI command" $ do
            parseGenImageArgs ["openai", "a man riding a horse on the moon"]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageOpenAI
                        OpenAIGenImageOptions
                            { openAICommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "a man riding a horse on the moon"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 1
                                    }
                            , openAIModel = "gpt-image-1.5"
                            , openAISize = Nothing
                            , openAIQuality = Nothing
                            , openAIOutputFormat = "png"
                            , openAIOutputCompression = Nothing
                            , openAIBackground = Nothing
                            , openAIModeration = Nothing
                            , openAIUser = Nothing
                            , openAIInputImageSources = []
                            , openAIInputFileIds = []
                            , openAIMaskSource = Nothing
                            , openAIMaskFileId = Nothing
                            , openAIInputFidelity = Nothing
                            }
                    )

        it "parses a minimal Grok command" $ do
            parseGenImageArgs ["grok", "a man riding a horse on the moon"]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageGrok
                        GrokGenImageOptions
                            { grokCommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "a man riding a horse on the moon"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 1
                                    }
                            , grokModel = "grok-imagine-image"
                            , grokInputImageSources = []
                            , grokAspectRatio = Nothing
                            , grokResolution = Nothing
                            , grokResponseFormat = "b64_json"
                            }
                    )

        it "parses Grok-specific options" $ do
            parseGenImageArgs
                [ "grok"
                , "--count=2"
                , "--aspect-ratio=16:9"
                , "--resolution=2k"
                , "--response-format=url"
                , "--image=base.png"
                , "moon"
                , "horse"
                ]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageGrok
                        GrokGenImageOptions
                            { grokCommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "moon horse"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 2
                                    }
                            , grokModel = "grok-imagine-image"
                            , grokInputImageSources = ["base.png"]
                            , grokAspectRatio = Just "16:9"
                            , grokResolution = Just "2k"
                            , grokResponseFormat = "url"
                            }
                    )

        it "parses OpenAI-specific options" $ do
            parseGenImageArgs
                [ "openai"
                , "--size=1536x1024"
                , "--quality=high"
                , "--output-format=jpeg"
                , "--output-compression=80"
                , "--background=transparent"
                , "--moderation=auto"
                , "--user=user-123"
                , "--image=base.png"
                , "--image-file-id=file-123"
                , "--mask=mask.png"
                , "--input-fidelity=high"
                , "-o"
                , "out.jpg"
                , "glass"
                , "city"
                ]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageOpenAI
                        OpenAIGenImageOptions
                            { openAICommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "glass city"
                                    , commonOutputPath = Just "out.jpg"
                                    , commonImageCount = 1
                                    }
                            , openAIModel = "gpt-image-1.5"
                            , openAISize = Just "1536x1024"
                            , openAIQuality = Just "high"
                            , openAIOutputFormat = "jpeg"
                            , openAIOutputCompression = Just 80
                            , openAIBackground = Just "transparent"
                            , openAIModeration = Just "auto"
                            , openAIUser = Just "user-123"
                            , openAIInputImageSources = ["base.png"]
                            , openAIInputFileIds = ["file-123"]
                            , openAIMaskSource = Just "mask.png"
                            , openAIMaskFileId = Nothing
                            , openAIInputFidelity = Just "high"
                            }
                    )

        it "shows general help" $ do
            parseGenImageArgs ["--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpGeneral

        it "shows provider-specific help" $ do
            parseGenImageArgs ["grok", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpGrok
            parseGenImageArgs ["openai", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpOpenAI

        it "errors on unknown providers" $ do
            parseGenImageArgs ["imagine", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "Unknown provider: imagine. Use `openai` or `grok`."
                    GenImageHelpGeneral

    describe "renderGenImageHelp" $ do
        it "general help lists both provider option sets" $ do
            let helpText = renderGenImageHelp "gen-image" GenImageHelpGeneral
            helpText `shouldSatisfy` T.isInfixOf "--image-file-id FILE_ID"
            helpText `shouldSatisfy` T.isInfixOf "--response-format FORMAT"
            helpText `shouldSatisfy` T.isInfixOf "gen-image openai --help"
            helpText `shouldSatisfy` T.isInfixOf "gen-image grok --help"

        it "grok help focuses on grok options" $ do
            let helpText = renderGenImageHelp "gen-image" GenImageHelpGrok
            helpText `shouldSatisfy` T.isInfixOf "--aspect-ratio RATIO"
            helpText `shouldSatisfy` T.isInfixOf "--response-format FORMAT"
            helpText `shouldNotSatisfy` T.isInfixOf "--mask-file-id FILE_ID"

        it "openai help focuses on openai options" $ do
            let helpText = renderGenImageHelp "gen-image" GenImageHelpOpenAI
            helpText `shouldSatisfy` T.isInfixOf "--mask-file-id FILE_ID"
            helpText `shouldSatisfy` T.isInfixOf "--output-compression N"
            helpText `shouldNotSatisfy` T.isInfixOf "--aspect-ratio RATIO"

    describe "slugifyPrompt" $ do
        it "creates stable ascii-friendly slugs" $ do
            slugifyPrompt "A man riding a horse on the moon!!!"
                `shouldBe` "a-man-riding-a-horse-on-the-moon"

        it "falls back when the prompt has no slug characters" $ do
            slugifyPrompt "!!!"
                `shouldBe` "image"
