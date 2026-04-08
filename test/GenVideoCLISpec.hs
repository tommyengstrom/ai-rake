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
                    "A provider is required. Use `xai`."
                    GenVideoHelpGeneral

        it "parses a minimal xai video command with the prompt before options" $ do
            parseGenVideoArgs ["xai", "She walk away", "--image", "girl.jpg"]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoXAI
                        XAIGenVideoOptions
                            { xaiVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "She walk away"
                                    , commonVideoOutputPath = Nothing
                                    }
                            , xaiVideoModel = "grok-imagine-video"
                            , xaiVideoImageSource = Just "girl.jpg"
                            , xaiVideoEditSource = Nothing
                            , xaiVideoExtendSource = Nothing
                            , xaiVideoDuration = Nothing
                            , xaiVideoAspectRatio = Nothing
                            , xaiVideoResolution = Nothing
                            , xaiVideoPollIntervalMilliseconds = 5000
                            , xaiVideoMaxPollAttempts = 120
                            }
                    )

        it "parses a minimal text-to-video command without a source" $ do
            parseGenVideoArgs ["xai", "A paper crane unfolds into a bird"]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoXAI
                        XAIGenVideoOptions
                            { xaiVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "A paper crane unfolds into a bird"
                                    , commonVideoOutputPath = Nothing
                                    }
                            , xaiVideoModel = "grok-imagine-video"
                            , xaiVideoImageSource = Nothing
                            , xaiVideoEditSource = Nothing
                            , xaiVideoExtendSource = Nothing
                            , xaiVideoDuration = Nothing
                            , xaiVideoAspectRatio = Nothing
                            , xaiVideoResolution = Nothing
                            , xaiVideoPollIntervalMilliseconds = 5000
                            , xaiVideoMaxPollAttempts = 120
                            }
                    )

        it "parses --extend as the existing-video route" $ do
            parseGenVideoArgs ["xai", "--extend=clip.mp4", "continue the scene"]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoXAI
                        XAIGenVideoOptions
                            { xaiVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "continue the scene"
                                    , commonVideoOutputPath = Nothing
                                    }
                            , xaiVideoModel = "grok-imagine-video"
                            , xaiVideoImageSource = Nothing
                            , xaiVideoEditSource = Nothing
                            , xaiVideoExtendSource = Just "clip.mp4"
                            , xaiVideoDuration = Nothing
                            , xaiVideoAspectRatio = Nothing
                            , xaiVideoResolution = Nothing
                            , xaiVideoPollIntervalMilliseconds = 5000
                            , xaiVideoMaxPollAttempts = 120
                            }
                    )

        it "parses xai video options" $ do
            parseGenVideoArgs
                [ "xai"
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
                    ( GenVideoXAI
                        XAIGenVideoOptions
                            { xaiVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "Animate her"
                                    , commonVideoOutputPath = Just "out.mp4"
                                    }
                            , xaiVideoModel = "my-video-model"
                            , xaiVideoImageSource = Just "girl.jpg"
                            , xaiVideoEditSource = Nothing
                            , xaiVideoExtendSource = Nothing
                            , xaiVideoDuration = Just 8
                            , xaiVideoAspectRatio = Just "16:9"
                            , xaiVideoResolution = Just "720p"
                            , xaiVideoPollIntervalMilliseconds = 1500
                            , xaiVideoMaxPollAttempts = 20
                            }
                    )

        it "shows help" $ do
            parseGenVideoArgs ["--help"]
                `shouldBe` ParseGenVideoArgsHelp GenVideoHelpGeneral
            parseGenVideoArgs ["xai", "--help"]
                `shouldBe` ParseGenVideoArgsHelp GenVideoHelpXAI

        it "errors on unknown providers" $ do
            parseGenVideoArgs ["openai", "horse"]
                `shouldBe` ParseGenVideoArgsError
                    "Unknown provider: openai. Use `xai`."
                    GenVideoHelpGeneral
            parseGenVideoArgs ["grok", "horse"]
                `shouldBe` ParseGenVideoArgsError
                    "Unknown provider: grok. Use `xai`."
                    GenVideoHelpGeneral

        it "rejects conflicting source modes" $ do
            parseGenVideoArgs ["xai", "--image=girl.jpg", "--video=clip.mp4", "She walk away"]
                `shouldBe` ParseGenVideoArgsError
                    "Use exactly one of --image, --edit/--video, or --extend."
                    GenVideoHelpXAI

    describe "renderGenVideoHelp" $ do
        it "general help lists the important xai options" $ do
            let helpText = renderGenVideoHelp "rake-video" GenVideoHelpGeneral
            helpText `shouldSatisfy` T.isInfixOf "With no source option, the CLI sends a text-to-video request."
            helpText `shouldSatisfy` T.isInfixOf "--image SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--edit SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--extend SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--video SOURCE"
            helpText `shouldSatisfy` T.isInfixOf "--poll-interval-ms N"
            helpText `shouldSatisfy` T.isInfixOf "rake-video xai --help"

        it "xai help focuses on video options" $ do
            let helpText = renderGenVideoHelp "rake-video" GenVideoHelpXAI
            helpText `shouldSatisfy` T.isInfixOf "--duration SECONDS"
            helpText `shouldSatisfy` T.isInfixOf "--max-poll-attempts N"
            helpText `shouldNotSatisfy` T.isInfixOf "--mask-file-id FILE_ID"
        it "parses --edit as the update route" $ do
            parseGenVideoArgs ["xai", "--edit=clip.mp4", "make the lighting moodier"]
                `shouldBe` ParseGenVideoArgsSuccess
                    ( GenVideoXAI
                        XAIGenVideoOptions
                            { xaiVideoCommonOptions =
                                CommonGenVideoOptions
                                    { commonVideoPromptText = "make the lighting moodier"
                                    , commonVideoOutputPath = Nothing
                                    }
                            , xaiVideoModel = "grok-imagine-video"
                            , xaiVideoImageSource = Nothing
                            , xaiVideoEditSource = Just "clip.mp4"
                            , xaiVideoExtendSource = Nothing
                            , xaiVideoDuration = Nothing
                            , xaiVideoAspectRatio = Nothing
                            , xaiVideoResolution = Nothing
                            , xaiVideoPollIntervalMilliseconds = 5000
                            , xaiVideoMaxPollAttempts = 120
                            }
                    )
