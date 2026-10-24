module Rake.TTS
    ( module Rake.TTS.Types
    , tts
    , ttsStreaming
    ) where

import Effectful
import Effectful.Error.Static
import Rake.Error
import Rake.Media
import Rake.Providers.OpenRouter.Settings
import Rake.Providers.OpenRouter.TTS
import Rake.TTS.Types
import Relude

tts
    :: (OpenRouterSpeechModel model, IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> model -> Text -> Eff es Audio
tts = generateOpenRouterSpeech

ttsStreaming
    :: (OpenRouterSpeechModel model, IOE :> es, Error RakeError :> es)
    => TTSStreamCallbacks es -> OpenRouterSettings es -> model -> Text -> Eff es Audio
ttsStreaming = streamOpenRouterSpeech
