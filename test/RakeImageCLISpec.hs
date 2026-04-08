module RakeImageCLISpec where

import RakeImageCLI
import Data.Text qualified as T
import Relude
import Test.Hspec

spec :: Spec
spec = describe "RakeImageCLI" $ do
    describe "parseGenImageArgs" $ do
        it "requires a model command" $ do
            parseGenImageArgs []
                `shouldBe` ParseGenImageArgsError
                    "A model is required. Use `gptimage`, `xai`, or `banana2`."
                    GenImageHelpGeneral

        it "parses a minimal gptimage command" $ do
            parseGenImageArgs ["gptimage", "a man riding a horse on the moon"]
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

        it "parses a minimal xai command" $ do
            parseGenImageArgs ["xai", "a man riding a horse on the moon"]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageXAI
                        XAIGenImageOptions
                            { xaiCommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "a man riding a horse on the moon"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 1
                                    }
                            , xaiModel = "grok-imagine-image"
                            , xaiInputImageSources = []
                            , xaiAspectRatio = Nothing
                            , xaiResolution = Nothing
                            , xaiResponseFormat = "b64_json"
                            }
                    )

        it "parses a minimal banana2 command" $ do
            parseGenImageArgs ["banana2", "a man riding a horse on the moon"]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageBanana2
                        Banana2GenImageOptions
                            { banana2CommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "a man riding a horse on the moon"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 1
                                    }
                            , banana2Model = "gemini-2.5-flash-image"
                            , banana2InputImageSources = []
                            , banana2AspectRatio = Nothing
                            , banana2ImageSize = Nothing
                            }
                    )

        it "parses xai-specific options" $ do
            parseGenImageArgs
                [ "xai"
                , "--count=2"
                , "--aspect-ratio=16:9"
                , "--resolution=2k"
                , "--response-format=url"
                , "--image=base.png"
                , "moon"
                , "horse"
                ]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageXAI
                        XAIGenImageOptions
                            { xaiCommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "moon horse"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 2
                                    }
                            , xaiModel = "grok-imagine-image"
                            , xaiInputImageSources = ["base.png"]
                            , xaiAspectRatio = Just "16:9"
                            , xaiResolution = Just "2k"
                            , xaiResponseFormat = "url"
                            }
                    )

        it "parses gptimage-specific options" $ do
            parseGenImageArgs
                [ "gptimage"
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

        it "parses banana2-specific options" $ do
            parseGenImageArgs
                [ "banana2"
                , "--aspect-ratio=16:9"
                , "--image-size=2K"
                , "--image=base.png"
                , "glass"
                , "city"
                ]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageBanana2
                        Banana2GenImageOptions
                            { banana2CommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "glass city"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 1
                                    }
                            , banana2Model = "gemini-2.5-flash-image"
                            , banana2InputImageSources = ["base.png"]
                            , banana2AspectRatio = Just "16:9"
                            , banana2ImageSize = Just "2K"
                            }
                    )

        it "rejects banana2 count options" $ do
            parseGenImageArgs ["banana2", "--count=2", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "banana2 does not support --count."
                    GenImageHelpBanana2
            parseGenImageArgs ["banana2", "-n", "2", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "banana2 does not support --count."
                    GenImageHelpBanana2

        it "shows general help" $ do
            parseGenImageArgs ["--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpGeneral

        it "shows model-specific help" $ do
            parseGenImageArgs ["xai", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpXAI
            parseGenImageArgs ["gptimage", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpOpenAI
            parseGenImageArgs ["banana2", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpBanana2

        it "errors on removed provider names" $ do
            parseGenImageArgs ["openai", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "Unknown model: openai. Use `gptimage`, `xai`, or `banana2`."
                    GenImageHelpGeneral
            parseGenImageArgs ["imagine", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "Unknown model: imagine. Use `gptimage`, `xai`, or `banana2`."
                    GenImageHelpGeneral
            parseGenImageArgs ["grok", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "Unknown model: grok. Use `gptimage`, `xai`, or `banana2`."
                    GenImageHelpGeneral

    describe "renderGenImageHelp" $ do
        it "general help lists all command option sets" $ do
            let helpText = renderGenImageHelp "rake-image" GenImageHelpGeneral
            helpText `shouldSatisfy` T.isInfixOf "--image-file-id FILE_ID"
            helpText `shouldSatisfy` T.isInfixOf "--response-format FORMAT"
            helpText `shouldSatisfy` T.isInfixOf "--image-size SIZE"
            helpText `shouldSatisfy` T.isInfixOf "rake-image gptimage --help"
            helpText `shouldSatisfy` T.isInfixOf "rake-image xai --help"
            helpText `shouldSatisfy` T.isInfixOf "rake-image banana2 --help"

        it "xai help focuses on xai options" $ do
            let helpText = renderGenImageHelp "rake-image" GenImageHelpXAI
            helpText `shouldSatisfy` T.isInfixOf "--aspect-ratio RATIO"
            helpText `shouldSatisfy` T.isInfixOf "--response-format FORMAT"
            helpText `shouldNotSatisfy` T.isInfixOf "--mask-file-id FILE_ID"
            helpText `shouldNotSatisfy` T.isInfixOf "--image-size SIZE"

        it "gptimage help focuses on gptimage options" $ do
            let helpText = renderGenImageHelp "rake-image" GenImageHelpOpenAI
            helpText `shouldSatisfy` T.isInfixOf "--mask-file-id FILE_ID"
            helpText `shouldSatisfy` T.isInfixOf "--output-compression N"
            helpText `shouldNotSatisfy` T.isInfixOf "--aspect-ratio RATIO"

        it "banana2 help focuses on banana2 options" $ do
            let helpText = renderGenImageHelp "rake-image" GenImageHelpBanana2
            helpText `shouldSatisfy` T.isInfixOf "--image-size SIZE"
            helpText `shouldSatisfy` T.isInfixOf "GEMINI_API_KEY"
            helpText `shouldNotSatisfy` T.isInfixOf "--count N"
            helpText `shouldNotSatisfy` T.isInfixOf "--mask-file-id FILE_ID"

    describe "slugifyPrompt" $ do
        it "creates stable ascii-friendly slugs" $ do
            slugifyPrompt "A man riding a horse on the moon!!!"
                `shouldBe` "a-man-riding-a-horse-on-the-moon"

        it "falls back when the prompt has no slug characters" $ do
            slugifyPrompt "!!!"
                `shouldBe` "image"
