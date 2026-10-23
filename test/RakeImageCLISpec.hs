module RakeImageCLISpec where

import Data.Text qualified as T
import Rake.Providers.OpenAI.Images (OpenAIImageModeration (..))
import RakeImageCLI
import Relude
import Test.Hspec

spec :: Spec
spec = describe "RakeImageCLI" $ do
    describe "parseGenImageArgs" $ do
        it "requires a prompt" $ do
            parseGenImageArgs []
                `shouldBe` ParseGenImageArgsError
                    "A prompt is required."
                    GenImageHelpXAI

        it "parses a minimal gptimage command" $ do
            parseGenImageArgs ["--model=openai/gpt-image-2", "a man riding a horse on the moon"]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageOpenAI
                        OpenAIGenImageOptions
                            { openAICommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "a man riding a horse on the moon"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 1
                                    }
                            , openAIModel = "gpt-image-2"
                            , openAISize = Nothing
                            , openAIQuality = Nothing
                            , openAIOutputFormat = "png"
                            , openAIOutputCompression = Nothing
                            , openAIBackground = Nothing
                            , openAIModeration = Just OpenAIImageModerationLow
                            , openAIUser = Nothing
                            , openAISafetyIdentifier = Nothing
                            , openAIInputImageSources = []
                            , openAIInputFileIds = []
                            , openAIMaskSource = Nothing
                            , openAIMaskFileId = Nothing
                            , openAIInputFidelity = Nothing
                            }
                    )

        it "defaults to xAI without a model flag" $ do
            parseGenImageArgs ["a man riding a horse on the moon"]
                `shouldBe` ParseGenImageArgsSuccess
                    ( GenImageXAI
                        XAIGenImageOptions
                            { xaiCommonOptions =
                                CommonGenImageOptions
                                    { commonPromptText = "a man riding a horse on the moon"
                                    , commonOutputPath = Nothing
                                    , commonImageCount = 1
                                    }
                            , xaiModel = "grok-imagine-image-2.0"
                            , xaiInputImageSources = []
                            , xaiAspectRatio = Nothing
                            , xaiResolution = Nothing
                            , xaiResponseFormat = "b64_json"
                            , xaiQuality = Just "low"
                            }
                    )

        it "parses a minimal banana2 command" $ do
            parseGenImageArgs
                ["--model=google/gemini-2.5-flash-image", "a man riding a horse on the moon"]
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
                [ "--model=xai/grok-imagine-image"
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
                            , xaiQuality = Nothing
                            }
                    )

        it "parses gptimage-specific options" $ do
            parseGenImageArgs
                [ "--model=openai/gpt-image-2"
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
                            , openAIModel = "gpt-image-2"
                            , openAISize = Just "1536x1024"
                            , openAIQuality = Just "high"
                            , openAIOutputFormat = "jpeg"
                            , openAIOutputCompression = Just 80
                            , openAIBackground = Just "transparent"
                            , openAIModeration = Just OpenAIImageModerationAuto
                            , openAIUser = Just "user-123"
                            , openAISafetyIdentifier = Nothing
                            , openAIInputImageSources = ["base.png"]
                            , openAIInputFileIds = ["file-123"]
                            , openAIMaskSource = Just "mask.png"
                            , openAIMaskFileId = Nothing
                            , openAIInputFidelity = Just "high"
                            }
                    )

        it "rejects unknown OpenAI image moderation values" $ do
            parseGenImageArgs ["--model=openai/gpt-image-2", "--moderation=spicy", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "Invalid value for --moderation: spicy. Use auto or low."
                    GenImageHelpOpenAI

        it "parses banana2-specific options" $ do
            parseGenImageArgs
                [ "--model=google/gemini-2.5-flash-image"
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
            parseGenImageArgs ["--model=google/gemini-2.5-flash-image", "--count=2", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "banana2 does not support --count."
                    GenImageHelpBanana2
            parseGenImageArgs ["--model=google/gemini-2.5-flash-image", "-n", "2", "horse"]
                `shouldBe` ParseGenImageArgsError
                    "banana2 does not support --count."
                    GenImageHelpBanana2

        it "shows general help" $ do
            parseGenImageArgs ["--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpGeneral

        it "shows model-specific help" $ do
            parseGenImageArgs ["--model=xai/grok-imagine-image", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpXAI
            parseGenImageArgs ["--model=openai/gpt-image-2", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpOpenAI
            parseGenImageArgs ["--model=google/gemini-2.5-flash-image", "--help"]
                `shouldBe` ParseGenImageArgsHelp GenImageHelpBanana2

        it "rejects models outside the supported catalog" $ do
            parseGenImageArgs ["-m", "unknown", "hello"]
                `shouldSatisfy` (\case ParseGenImageArgsError{} -> True; _ -> False)

        forM_ (["low", "medium", "auto"] :: [Text]) $ \quality ->
            forM_ ([["--quality", quality], ["--quality=" <> quality]] :: [[Text]]) $ \flags ->
                it ("overrides Grok quality with " <> show flags) $ do
                    case parseGenImageArgs (flags <> ["draw a lighthouse"]) of
                        ParseGenImageArgsSuccess (GenImageXAI XAIGenImageOptions{xaiModel, xaiQuality}) -> do
                            xaiModel `shouldBe` "grok-imagine-image-2.0"
                            xaiQuality `shouldBe` Just quality
                        other -> expectationFailure (show other)

        it "rejects invalid Grok quality and a missing quality value" $ do
            parseGenImageArgs ["--quality=high", "draw a lighthouse"]
                `shouldBe` ParseGenImageArgsError "xAI image quality must be low, medium, or auto" GenImageHelpXAI
            parseGenImageArgs ["draw a lighthouse", "--quality"]
                `shouldBe` ParseGenImageArgsError "--quality requires an argument" GenImageHelpGeneral

        it "rejects explicit quality on legacy Grok" $ do
            parseGenImageArgs ["-m", "grok-imagine-image", "--quality=low", "draw a lighthouse"]
                `shouldBe` ParseGenImageArgsError
                    "xAI image model grok-imagine-image does not support quality"
                    GenImageHelpXAI

        forM_
            ( [ "gpt-image-2.5-flare"
              , "gpt-image-2.5-sunburst"
              , "gpt-image-2.5-flare-2026-09-08"
              , "gpt-image-2.5-sunburst-2026-09-08"
              ]
                :: [Text]
            )
            $ \modelName ->
                it ("selects " <> toString modelName <> " with automatic quality") $ do
                    case parseGenImageArgs ["-m", modelName, "draw a lighthouse"] of
                        ParseGenImageArgsSuccess
                            (GenImageOpenAI OpenAIGenImageOptions{openAIModel, openAIQuality}) -> do
                            openAIModel `shouldBe` modelName
                            openAIQuality `shouldBe` Nothing
                        other -> expectationFailure (show other)

        forM_ (["auto", "low", "medium", "high", "xhigh", "max"] :: [Text]) $ \quality ->
            forM_ ([["--quality", quality], ["--quality=" <> quality]] :: [[Text]]) $ \flags ->
                it ("overrides OpenAI quality with " <> show flags) $ do
                    case parseGenImageArgs (["-m", "openai/gpt-image-2.5-flare"] <> flags <> ["draw a lighthouse"]) of
                        ParseGenImageArgsSuccess (GenImageOpenAI OpenAIGenImageOptions{openAIQuality}) ->
                            openAIQuality `shouldBe` Just quality
                        other -> expectationFailure (show other)

        forM_
            ([["--safety-identifier", "hashed-user"], ["--safety-identifier=hashed-user"]] :: [[Text]]) $ \flags ->
            it ("accepts an OpenAI safety identifier with " <> show flags) $ do
                case parseGenImageArgs
                    ( ["-m", "openai/gpt-image-2.5-flare", "--user=legacy-user"]
                        <> flags
                        <> ["draw a lighthouse"]
                    ) of
                    ParseGenImageArgsSuccess
                        (GenImageOpenAI OpenAIGenImageOptions{openAISafetyIdentifier, openAIUser}) -> do
                        openAISafetyIdentifier `shouldBe` Just "hashed-user"
                        openAIUser `shouldBe` Just "legacy-user"
                    other -> expectationFailure (show other)

    describe "renderGenImageHelp" $ do
        it "general help lists all command option sets" $ do
            let helpText = renderGenImageHelp "rake-image" GenImageHelpGeneral
            helpText `shouldSatisfy` T.isInfixOf "--image-file-id FILE_ID"
            helpText `shouldSatisfy` T.isInfixOf "--response-format FORMAT"
            helpText `shouldSatisfy` T.isInfixOf "--image-size SIZE"
            helpText `shouldSatisfy` T.isInfixOf "rake-image -m openai/gpt-image-2.5-flare --help"
            helpText `shouldSatisfy` T.isInfixOf "rake-image -m xai/grok-imagine-image-2.0 --help"
            helpText `shouldSatisfy` T.isInfixOf "rake-image -m google/gemini-2.5-flash-image --help"

        it "xai help focuses on xai options" $ do
            let helpText = renderGenImageHelp "rake-image" GenImageHelpXAI
            helpText `shouldSatisfy` T.isInfixOf "--aspect-ratio RATIO"
            helpText `shouldSatisfy` T.isInfixOf "--response-format FORMAT"
            helpText `shouldSatisfy` T.isInfixOf "--quality QUALITY"
            helpText `shouldSatisfy` T.isInfixOf "low, medium, auto. Default: low"
            helpText `shouldNotSatisfy` T.isInfixOf "--mask-file-id FILE_ID"
            helpText `shouldNotSatisfy` T.isInfixOf "--image-size SIZE"

        it "OpenAI help focuses on OpenAI options" $ do
            let helpText = renderGenImageHelp "rake-image" GenImageHelpOpenAI
            helpText
                `shouldSatisfy` T.isInfixOf "rake-image -m openai/gpt-image-2.5-flare [OPTIONS] PROMPT"
            helpText `shouldSatisfy` T.isInfixOf "--mask-file-id FILE_ID"
            helpText `shouldSatisfy` T.isInfixOf "--output-compression N"
            helpText `shouldSatisfy` T.isInfixOf "xhigh, max. Default: auto"
            helpText `shouldSatisfy` T.isInfixOf "--safety-identifier ID"
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
