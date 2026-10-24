module Rake.TTS.Types
    ( TTSStreamCallbacks (..)
    , defaultTTSStreamCallbacks
    ) where

import Effectful
import Relude

data TTSStreamCallbacks es = TTSStreamCallbacks
    { onAudioChunk :: ByteString -> Eff es ()
    }

defaultTTSStreamCallbacks :: TTSStreamCallbacks es
defaultTTSStreamCallbacks =
    TTSStreamCallbacks
        { onAudioChunk = \_ -> pure ()
        }
