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
import Rake.Providers.OpenAI.TTS
import Rake.Providers.XAI.TTS
import Rake.TTS.Types
import Relude

data TTSSettings es
    = TTSOpenAI (OpenAITTSSettings es)
    | TTSXAI (XAITTSSettings es)

data TTSStreamingSettings es
    = TTSStreamingOpenAI (OpenAITTSStreamingSettings es)
    | TTSStreamingXAI (XAITTSStreamingSettings es)

tts
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => TTSSettings es
    -> Text
    -> Eff es Audio
tts settings inputText = case settings of
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
    TTSStreamingOpenAI openAISettings ->
        streamOpenAISpeech callbacks openAISettings inputText
    TTSStreamingXAI xaiSettings ->
        streamXAISpeech callbacks xaiSettings inputText
