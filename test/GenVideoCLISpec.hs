module GenVideoCLISpec where

import Data.Text qualified as T
import GenVideoCLI
import Relude
import Test.Hspec

spec :: Spec
spec = describe "GenVideoCLI" $ do
    describe "parseGenVideoArgs" $ do
        it "requires a provider" $ do
            parseGenVideoArgs []
                `shouldBe` ParseGenVideoArgsError
                    "A provider is required. Use `grok`."
                    GenVideoHelpGeneral

        it "parses a minimal Grok video command with the prompt before options" $ do
            parseGenVideoArgs ["grok", "She walk away", "--image", "girl.jpg"]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoGrok
                        GrokGenVideoOptions
                            { grokVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "She walk away"
                                    , commonVideoOutputPath = Nothing
                                    }
                            , grokVideoModel = "grok-imagine-video"
                            , grokVideoImageSource = Just "girl.jpg"
                            , grokVideoEditSource = Nothing
                            , grokVideoExtendSource = Nothing
                            , grokVideoDuration = Nothing
                            , grokVideoAspectRatio = Nothing
                            , grokVideoResolution = Nothing
                            , grokVideoPollIntervalMilliseconds = 5000
                            , grokVideoMaxPollAttempts = 120
                            }
                    )

        it "parses --extend as the existing-video route" $ do
            parseGenVideoArgs ["grok", "--extend=clip.mp4", "continue the scene"]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoGrok
                        GrokGenVideoOptions
                            { grokVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "continue the scene"
                                    , commonVideoOutputPath = Nothing
                                    }
                            , grokVideoModel = "grok-imagine-video"
                            , grokVideoImageSource = Nothing
                            , grokVideoEditSource = Nothing
                            , grokVideoExtendSource = Just "clip.mp4"
                            , grokVideoDuration = Nothing
                            , grokVideoAspectRatio = Nothing
                            , grokVideoResolution = Nothing
                            , grokVideoPollIntervalMilliseconds = 5000
                            , grokVideoMaxPollAttempts = 120
                            }
                    )

        it "parses Grok video options" $ do
            parseGenVideoArgs
                [ "grok"
                , "--model=my-video-model"
                , "--image=girl.jpg"
                , "--duration=8"
                , "--aspect-ratio=16:9"
                , "--resolution=720p"
                , "--poll-interval-ms=1500"
                , "--max-poll-attempts=20"
                , "-o"
                , "out.mp4"
                , "Animate"
                , "her"
                ]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoGrok
                        GrokGenVideoOptions
                            { grokVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "Animate her"
                                    , commonVideoOutputPath = Just "out.mp4"
                                    }
                            , grokVideoModel = "my-video-model"
                            , grokVideoImageSource = Just "girl.jpg"
                            , grokVideoEditSource = Nothing
                            , grokVideoExtendSource = Nothing
                            , grokVideoDuration = Just 8
                            , grokVideoAspectRatio = Just "16:9"
                            , grokVideoResolution = Just "720p"
                            , grokVideoPollIntervalMilliseconds = 1500
                            , grokVideoMaxPollAttempts = 20
                            }
                    )

        it "shows help" $ do
            parseGenVideoArgs ["--help"]
                `shouldBe` ParseGenVideoArgsHelp GenVideoHelpGeneral
            parseGenVideoArgs ["grok", "--help"]
                `shouldBe` ParseGenVideoArgsHelp GenVideoHelpGrok

        it "errors on unknown providers" $ do
            parseGenVideoArgs ["openai", "horse"]
                `shouldBe` ParseGenVideoArgsError
                    "Unknown provider: openai. Use `grok`."
                    GenVideoHelpGeneral

        it "requires exactly one source" $ do
            parseGenVideoArgs ["grok", "She walk away"]
                `shouldBe` ParseGenVideoArgsError
                    "Use --image SOURCE for image-to-video generation, --edit SOURCE to update an existing video, or --extend SOURCE to append a continuation."
                    GenVideoHelpGrok

            parseGenVideoArgs ["grok", "--image=girl.jpg", "--video=clip.mp4", "She walk away"]
                `shouldBe` ParseGenVideoArgsError
                    "Use exactly one of --image, --edit/--video, or --extend."
                    GenVideoHelpGrok

    describe "renderGenVideoHelp" $ do
        it "general help lists the important grok options" $ do
            let helpText = renderGenVideoHelp "gen-video" GenVideoHelpGeneral
            helpText `shouldSatisfy` T.isInfixOf "--image SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--edit SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--extend SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--video SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--poll-interval-ms N"
            helpText `shouldSatisfy` T.isInfixOf "gen-video grok --help"

        it "grok help focuses on video options" $ do
            let helpText = renderGenVideoHelp "gen-video" GenVideoHelpGrok
            helpText `shouldSatisfy` T.isInfixOf "--duration SECONDS"
            helpText `shouldSatisfy` T.isInfixOf "--max-poll-attempts N"
            helpText `shouldNotSatisfy` T.isInfixOf "--mask-file-id FILE_ID"
        it "parses --edit as the update route" $ do
            parseGenVideoArgs ["grok", "--edit=clip.mp4", "make the lighting moodier"]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoGrok
                        GrokGenVideoOptions
                            { grokVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "make the lighting moodier"
                                    , commonVideoOutputPath = Nothing
                                    }
                            , grokVideoModel = "grok-imagine-video"
                            , grokVideoImageSource = Nothing
                            , grokVideoEditSource = Just "clip.mp4"
                            , grokVideoExtendSource = Nothing
                            , grokVideoDuration = Nothing
                            , grokVideoAspectRatio = Nothing
                            , grokVideoResolution = Nothing
                            , grokVideoPollIntervalMilliseconds = 5000
                            , grokVideoMaxPollAttempts = 120
                            }
                    )
