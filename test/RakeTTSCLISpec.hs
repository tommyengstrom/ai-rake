module RakeTTSCLISpec where

import Data.Text qualified as T
import Rake (Audio (..))
import Rake.Providers.ElevenLabs.TTS qualified as EL
import Rake.Providers.OpenAI.TTS
import Rake.Providers.OpenRouter.TTS qualified as OR
import Rake.Providers.XAI.TTS
import RakeTTSCLI
import Relude
import Test.Hspec

spec :: Spec
spec = describe "RakeTTSCLI" $ do
    describe "parseGenSpeechArgs" $ do
        it "requires a provider" $ do
            parseGenSpeechArgs []
                `shouldBe` ParseGenSpeechArgsError
                    "A provider is required. Use `openai`, `xai`, `elevenlabs`, or `openrouter`."
                    GenSpeechHelpGeneral

        it "parses a minimal openai command" $ do
            parseGenSpeechArgs ["openai", "Hello", "world"]
                `shouldBe` ParseGenSpeechArgsSuccess
                    ( GenSpeechOpenAI
                        OpenAIGenSpeechOptions
                            { openAISpeechCommonOptions =
                                CommonGenSpeechOptions
                                    { commonSpeechText = "Hello world"
                                    , commonSpeechOutputPath = Nothing
                                    }
                            , openAISpeechModel = OpenAITTSModelGPT4OMiniTTS
                            , openAISpeechVoice = OpenAIVoiceAlloy
                            , openAISpeechInstructions = Nothing
                            , openAISpeechFormat = Nothing
                            , openAISpeechSpeed = Nothing
                            }
                    )

        it "parses a minimal xai command" $ do
            parseGenSpeechArgs ["xai", "Hello", "world"]
                `shouldBe` ParseGenSpeechArgsSuccess
                    ( GenSpeechXAI
                        XAIGenSpeechOptions
                            { xaiSpeechCommonOptions =
                                CommonGenSpeechOptions
                                    { commonSpeechText = "Hello world"
                                    , commonSpeechOutputPath = Nothing
                                    }
                            , xaiSpeechVoice = XAITTSVoiceEve
                            , xaiSpeechLanguage = XAITTSLanguageAuto
                            , xaiSpeechCodec = Nothing
                            , xaiSpeechSampleRate = Nothing
                            , xaiSpeechBitRate = Nothing
                            }
                    )

        it "parses openai-specific options" $ do
            parseGenSpeechArgs
                [ "openai"
                , "--model=tts-1-hd"
                , "--voice=verse"
                , "--instructions=Speak slowly"
                , "--format=wav"
                , "--speed=1.25"
                , "-o"
                , "out.wav"
                , "status"
                , "update"
                ]
                `shouldBe` ParseGenSpeechArgsSuccess
                    ( GenSpeechOpenAI
                        OpenAIGenSpeechOptions
                            { openAISpeechCommonOptions =
                                CommonGenSpeechOptions
                                    { commonSpeechText = "status update"
                                    , commonSpeechOutputPath = Just "out.wav"
                                    }
                            , openAISpeechModel = OpenAITTSModelTTS1HD
                            , openAISpeechVoice = OpenAIVoiceVerse
                            , openAISpeechInstructions = Just "Speak slowly"
                            , openAISpeechFormat = Just OpenAIAudioFormatWav
                            , openAISpeechSpeed = Just 1.25
                            }
                    )

        it "parses xai-specific options" $ do
            parseGenSpeechArgs
                [ "xai"
                , "--voice=rex"
                , "--language=en"
                , "--codec=mp3"
                , "--sample-rate=24000"
                , "--bit-rate=128000"
                , "-o"
                , "out.mp3"
                , "status"
                , "update"
                ]
                `shouldBe` ParseGenSpeechArgsSuccess
                    ( GenSpeechXAI
                        XAIGenSpeechOptions
                            { xaiSpeechCommonOptions =
                                CommonGenSpeechOptions
                                    { commonSpeechText = "status update"
                                    , commonSpeechOutputPath = Just "out.mp3"
                                    }
                            , xaiSpeechVoice = XAITTSVoiceRex
                            , xaiSpeechLanguage = XAITTSLanguageEnglish
                            , xaiSpeechCodec = Just XAISpeechCodecMp3
                            , xaiSpeechSampleRate = Just XAISampleRate24000
                            , xaiSpeechBitRate = Just XAIMP3BitRate128000
                            }
                    )

        it "shows help" $ do
            parseGenSpeechArgs ["--help"]
                `shouldBe` ParseGenSpeechArgsHelp GenSpeechHelpGeneral
            parseGenSpeechArgs ["openai", "--help"]
                `shouldBe` ParseGenSpeechArgsHelp GenSpeechHelpOpenAI
            parseGenSpeechArgs ["xai", "--help"]
                `shouldBe` ParseGenSpeechArgsHelp GenSpeechHelpXAI

        it "errors on unknown providers" $ do
            parseGenSpeechArgs ["grok", "hello"]
                `shouldBe` ParseGenSpeechArgsError
                    "Unknown provider: grok. Use `openai`, `xai`, `elevenlabs`, or `openrouter`."
                    GenSpeechHelpGeneral

        it "rejects xai sample-rate without a codec" $ do
            parseGenSpeechArgs ["xai", "--sample-rate=24000", "hello"]
                `shouldBe` ParseGenSpeechArgsError
                    "Use --codec when setting --sample-rate."
                    GenSpeechHelpXAI

        it "rejects xai bit-rate without a codec" $ do
            parseGenSpeechArgs ["xai", "--bit-rate=128000", "hello"]
                `shouldBe` ParseGenSpeechArgsError
                    "Use --codec when setting --bit-rate."
                    GenSpeechHelpXAI

        it "rejects xai bit-rate on non-mp3 codecs" $ do
            parseGenSpeechArgs ["xai", "--codec=wav", "--bit-rate=128000", "hello"]
                `shouldBe` ParseGenSpeechArgsError
                    "--bit-rate requires --codec=mp3."
                    GenSpeechHelpXAI

    describe "ElevenLabs and OpenRouter commands" $ do
        it "uses the selected defaults for both routes" $ do
            parseGenSpeechArgs ["elevenlabs", "Hello", "world"]
                `shouldBe` ParseGenSpeechArgsSuccess
                    ( GenSpeechElevenLabs
                        (CommonGenSpeechOptions "Hello world" Nothing)
                        EL.defaultElevenLabsTTSOptions
                    )
            parseGenSpeechArgs ["openrouter", "Hello"]
                `shouldBe` ParseGenSpeechArgsSuccess
                    ( GenSpeechOpenRouter
                        (CommonGenSpeechOptions "Hello" Nothing)
                        OR.defaultOpenRouterTTSOptions
                    )

        it "parses direct model, voice, native format and every voice control"
            $ parseGenSpeechArgs
                [ "elevenlabs"
                , "--model=eleven_flash_v2_5"
                , "--voice"
                , "custom"
                , "--format=wav_24000"
                , "--language=sv"
                , "--speed"
                , "1.1"
                , "--stability=0.4"
                , "--similarity=0.7"
                , "--style=0.2"
                , "--speaker-boost=false"
                , "-o"
                , "out.wav"
                , "Hej"
                ]
            `shouldBe` ParseGenSpeechArgsSuccess
                ( GenSpeechElevenLabs
                    (CommonGenSpeechOptions "Hej" (Just "out.wav"))
                    EL.defaultElevenLabsTTSOptions
                        { EL.model = "eleven_flash_v2_5"
                        , EL.voice = "custom"
                        , EL.outputFormat = EL.ElevenLabsWav_24000
                        , EL.controls =
                            EL.ElevenLabsControls
                                (Just "sv")
                                (EL.ElevenLabsVoiceSettings (Just 0.4) (Just 0.7) (Just 0.2) (Just 1.1) (Just False))
                        }
                )

        it "parses OpenRouter overrides and preserves text after --"
            $ parseGenSpeechArgs
                [ "openrouter"
                , "--model"
                , "elevenlabs/eleven-flash-v2.5"
                , "--voice=custom"
                , "--format=pcm"
                , "--speaker-boost"
                , "true"
                , "--output=out.pcm"
                , "--"
                , "--literal"
                , "text"
                ]
            `shouldBe` ParseGenSpeechArgsSuccess
                ( GenSpeechOpenRouter
                    (CommonGenSpeechOptions "--literal text" (Just "out.pcm"))
                    OR.defaultOpenRouterTTSOptions
                        { OR.model = "elevenlabs/eleven-flash-v2.5"
                        , OR.voice = "custom"
                        , OR.responseFormat = OR.OpenRouterAudioFormatPcm
                        , OR.elevenLabsControls =
                            EL.defaultElevenLabsControls
                                { EL.voiceSettings = EL.defaultElevenLabsVoiceSettings{EL.useSpeakerBoost = Just True}
                                }
                        }
                )

        it
            "rejects invalid controls, formats, empty identifiers, missing values and unknown flags"
            $ forM_
                ( [ ["--speed=NaN"]
                  , ["--speed=Infinity"]
                  , ["--speed=-1"]
                  , ["--stability=1.1"]
                  , ["--similarity=-0.1"]
                  , ["--style=abc"]
                  , ["--speaker-boost=yes"]
                  , ["--format=mp3"]
                  , ["--model="]
                  , ["--voice= "]
                  , ["--language="]
                  , ["--unknown=value"]
                  ]
                    :: [[Text]]
                )
            $ \flags ->
                parseGenSpeechArgs (["elevenlabs"] <> flags <> ["hello"]) `shouldSatisfy` isParseError

        it "rejects missing text and missing option values" $ do
            parseGenSpeechArgs ["elevenlabs"] `shouldSatisfy` isParseError
            parseGenSpeechArgs ["openrouter", "hello", "--voice"] `shouldSatisfy` isParseError
            parseGenSpeechArgs ["openrouter", "--format=wav", "hello"] `shouldSatisfy` isParseError
            parseGenSpeechArgs ["openrouter", "--model=other/model", "--speed=1.1", "hello"]
                `shouldSatisfy` isParseError

        it "does not consume another option as a missing value" $ do
            parseGenSpeechArgs ["openrouter", "hello", "--voice", "--help"]
                `shouldSatisfy` isParseError
            parseGenSpeechArgs ["elevenlabs", "hello", "--output", "--voice=foo"]
                `shouldSatisfy` isParseError
            parseGenSpeechArgs ["elevenlabs", "hello", "--model", "--"] `shouldSatisfy` isParseError
            parseGenSpeechArgs ["elevenlabs", "hello", "--output="] `shouldSatisfy` isParseError

        it "renders provider help with formats, voice controls, and credentials" $ do
            parseGenSpeechArgs ["elevenlabs", "--help"]
                `shouldBe` ParseGenSpeechArgsHelp GenSpeechHelpElevenLabs
            parseGenSpeechArgs ["openrouter", "--help"]
                `shouldBe` ParseGenSpeechArgsHelp GenSpeechHelpOpenRouter
            renderGenSpeechHelp "rake-tts" GenSpeechHelpElevenLabs
                `shouldSatisfy` T.isInfixOf "ELEVENLABS_API_KEY"
            renderGenSpeechHelp "rake-tts" GenSpeechHelpOpenRouter
                `shouldSatisfy` T.isInfixOf "OPENROUTER_API_KEY"
            renderGenSpeechHelp "rake-tts" GenSpeechHelpOpenRouter
                `shouldSatisfy` T.isInfixOf "--speaker-boost"

        it "uses requested formats when response metadata is absent" $ do
            suggestedSpeechExtension
                ( GenSpeechElevenLabs
                    (CommonGenSpeechOptions "" Nothing)
                    EL.defaultElevenLabsTTSOptions{EL.outputFormat = EL.ElevenLabsWav_24000}
                )
                (Audio mempty Nothing Nothing)
                `shouldBe` ".wav"
            suggestedSpeechExtension
                ( GenSpeechOpenRouter
                    (CommonGenSpeechOptions "" Nothing)
                    OR.defaultOpenRouterTTSOptions{OR.responseFormat = OR.OpenRouterAudioFormatPcm}
                )
                (Audio mempty Nothing Nothing)
                `shouldBe` ".pcm"

    describe "renderGenSpeechHelp" $ do
        it "general help lists both provider option sets" $ do
            let helpText = renderGenSpeechHelp "rake-tts" GenSpeechHelpGeneral
            helpText `shouldSatisfy` T.isInfixOf "--format FORMAT"
            helpText `shouldSatisfy` T.isInfixOf "--codec CODEC"
            helpText
                `shouldSatisfy` T.isInfixOf "With no `--output`, the CLI plays the audio locally."
            helpText `shouldSatisfy` T.isInfixOf "rake-tts openai --help"
            helpText `shouldSatisfy` T.isInfixOf "rake-tts xai --help"

        it "openai help focuses on openai options" $ do
            let helpText = renderGenSpeechHelp "rake-tts" GenSpeechHelpOpenAI
            helpText `shouldSatisfy` T.isInfixOf "--instructions TEXT"
            helpText `shouldSatisfy` T.isInfixOf "--format FORMAT"
            helpText `shouldSatisfy` T.isInfixOf "ffplay"
            helpText `shouldNotSatisfy` T.isInfixOf "--bit-rate BPS"

        it "xai help focuses on xai options" $ do
            let helpText = renderGenSpeechHelp "rake-tts" GenSpeechHelpXAI
            helpText `shouldSatisfy` T.isInfixOf "--bit-rate BPS"
            helpText `shouldSatisfy` T.isInfixOf "--sample-rate HZ"
            helpText
                `shouldSatisfy` T.isInfixOf "With no `--output`, the CLI plays the audio locally."
            helpText `shouldNotSatisfy` T.isInfixOf "--instructions TEXT"

    describe "helpers" $ do
        it "creates stable ascii-friendly speech slugs" $ do
            slugifySpeechText "Speech test, now!"
                `shouldBe` "speech-test-now"

        it "falls back when the speech text has no slug characters" $ do
            slugifySpeechText "!!!"
                `shouldBe` "speech"

        it "prefers the provider filename extension when present" $ do
            suggestedSpeechExtension
                ( GenSpeechOpenAI
                    OpenAIGenSpeechOptions
                        { openAISpeechCommonOptions =
                            CommonGenSpeechOptions
                                { commonSpeechText = ""
                                , commonSpeechOutputPath = Nothing
                                }
                        , openAISpeechModel = OpenAITTSModelGPT4OMiniTTS
                        , openAISpeechVoice = OpenAIVoiceAlloy
                        , openAISpeechInstructions = Nothing
                        , openAISpeechFormat = Nothing
                        , openAISpeechSpeed = Nothing
                        }
                )
                Audio
                    { audioBytes = mempty
                    , mimeType = Just "audio/wav"
                    , fileName = Just "provider.flac"
                    }
                `shouldBe` ".flac"

        it "prefers the mime type over the explicit format when no filename is present" $ do
            suggestedSpeechExtension
                ( GenSpeechOpenAI
                    OpenAIGenSpeechOptions
                        { openAISpeechFormat = Just OpenAIAudioFormatMp3
                        , openAISpeechCommonOptions =
                            CommonGenSpeechOptions
                                { commonSpeechText = ""
                                , commonSpeechOutputPath = Nothing
                                }
                        , openAISpeechModel = OpenAITTSModelGPT4OMiniTTS
                        , openAISpeechVoice = OpenAIVoiceAlloy
                        , openAISpeechInstructions = Nothing
                        , openAISpeechSpeed = Nothing
                        }
                )
                Audio
                    { audioBytes = mempty
                    , mimeType = Just "audio/wav"
                    , fileName = Nothing
                    }
                `shouldBe` ".wav"

        it "falls back to the explicit codec when the mime type is ambiguous" $ do
            suggestedSpeechExtension
                ( GenSpeechXAI
                    XAIGenSpeechOptions
                        { xaiSpeechCodec = Just XAISpeechCodecALaw
                        , xaiSpeechCommonOptions =
                            CommonGenSpeechOptions
                                { commonSpeechText = ""
                                , commonSpeechOutputPath = Nothing
                                }
                        , xaiSpeechVoice = XAITTSVoiceEve
                        , xaiSpeechLanguage = XAITTSLanguageAuto
                        , xaiSpeechSampleRate = Nothing
                        , xaiSpeechBitRate = Nothing
                        }
                )
                Audio
                    { audioBytes = mempty
                    , mimeType = Just "audio/basic"
                    , fileName = Nothing
                    }
                `shouldBe` ".alaw"

        it "falls back to mp3 when nothing else is available" $ do
            suggestedSpeechExtension
                ( GenSpeechXAI
                    XAIGenSpeechOptions
                        { xaiSpeechCommonOptions =
                            CommonGenSpeechOptions
                                { commonSpeechText = ""
                                , commonSpeechOutputPath = Nothing
                                }
                        , xaiSpeechVoice = XAITTSVoiceEve
                        , xaiSpeechLanguage = XAITTSLanguageAuto
                        , xaiSpeechCodec = Nothing
                        , xaiSpeechSampleRate = Nothing
                        , xaiSpeechBitRate = Nothing
                        }
                )
                Audio
                    { audioBytes = mempty
                    , mimeType = Nothing
                    , fileName = Nothing
                    }
                `shouldBe` ".mp3"

isParseError :: ParseGenSpeechArgsResult -> Bool
isParseError = \case
    ParseGenSpeechArgsError _ _ -> True
    ParseGenSpeechArgsHelp _ -> False
    ParseGenSpeechArgsSuccess _ -> False
