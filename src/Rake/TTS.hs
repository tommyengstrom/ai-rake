module Rake.TTS
    ( TTSSettings (..)
    , TTSStreamingSettings (..)
    , TTSStreamCallbacks (..)
    , defaultTTSStreamCallbacks
    , tts
    , ttsStreaming
    ) where

import Effectful
import Effectful.Error.Static
import Rake.Error (RakeError)
import Rake.Media
import Rake.Providers.ElevenLabs.TTS
import Rake.Providers.OpenAI.TTS
import Rake.Providers.OpenRouter.TTS
import Rake.Providers.XAI.TTS
import Rake.TTS.Types
import Relude

data TTSSettings es
    = TTSOpenAI (OpenAITTSSettings es)
    | TTSXAI (XAITTSSettings es)
    | TTSElevenLabs (ElevenLabsTTSSettings es)
    | TTSOpenRouter (OpenRouterTTSSettings es)

data TTSStreamingSettings es
    = TTSStreamingOpenAI (OpenAITTSStreamingSettings es)
    | TTSStreamingXAI (XAITTSStreamingSettings es)
    | TTSStreamingElevenLabs (ElevenLabsTTSStreamingSettings es)
    | TTSStreamingOpenRouter (OpenRouterTTSStreamingSettings es)

tts
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => TTSSettings es
    -> Text
    -> Eff es Audio
tts settings inputText = case settings of
    TTSElevenLabs providerSettings ->
        generateElevenLabsSpeech providerSettings inputText
    TTSOpenRouter providerSettings ->
        generateOpenRouterSpeech providerSettings inputText
    TTSOpenAI openAISettings ->
        generateOpenAISpeech openAISettings inputText
    TTSXAI xaiSettings ->
        generateXAISpeech xaiSettings inputText

ttsStreaming
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => TTSStreamCallbacks es
    -> TTSStreamingSettings es
    -> Text
    -> Eff es Audio
ttsStreaming callbacks settings inputText = case settings of
    TTSStreamingElevenLabs providerSettings ->
        streamElevenLabsSpeech callbacks providerSettings inputText
    TTSStreamingOpenRouter providerSettings ->
        streamOpenRouterSpeech callbacks providerSettings inputText
    TTSStreamingOpenAI openAISettings ->
        streamOpenAISpeech callbacks openAISettings inputText
    TTSStreamingXAI xaiSettings ->
        streamXAISpeech callbacks xaiSettings inputText
