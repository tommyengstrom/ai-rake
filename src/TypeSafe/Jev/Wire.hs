module TypeSafe.Jev.Wire
    ( WireQuestion (..)
    , WireNoulCriteria (..)
    , WireAnswer (..)
    , WireRequest (..)
    , WireResponse (..)
    , WireUsage (..)
    ) where

import Data.Aeson
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import TypeSafe.Jev.Types (Confidence, Content, Probability)
import Prelude

data WireNoulCriteria = WireNoulCriteria
    { true :: Maybe Content
    , false :: Maybe Content
    }
    deriving stock (Generic)

instance ToJSON WireNoulCriteria where
    toJSON = genericToJSON wireOptions

data WireQuestion
    = Choice
        { instructions :: Maybe Content
        , choiceCriteria :: Map Text (Maybe Content)
        }
    | Score
        { instructions :: Maybe Content
        , scoreCriteria :: [Content]
        }
    | Noul
        { instructions :: Maybe Content
        , noulCriteria :: Maybe WireNoulCriteria
        }
    deriving stock (Generic)

instance ToJSON WireQuestion where
    toJSON =
        genericToJSON
            wireOptions
                { fieldLabelModifier = \case
                    "choiceCriteria" -> "criteria"
                    "scoreCriteria" -> "criteria"
                    "noulCriteria" -> "criteria"
                    field -> field
                }

data WireAnswer
    = ChoiceAnswer
        { choice :: Text
        , confidence :: Confidence
        , probabilities :: Map Text Probability
        }
    | ScoreAnswer
        { score :: Double
        , legend :: Map Text Content
        , confidence :: Confidence
        , probabilities :: Map Text Probability
        }
    | NoulAnswer
        { noul :: Probability
        }
    deriving stock (Generic)

instance FromJSON WireAnswer where
    parseJSON = genericParseJSON wireOptions{constructorTagModifier = map toLower . takeWhile (/= 'A')}

data WireRequest = WireRequest
    { state :: Content
    , model :: Text
    , questions :: Map Text WireQuestion
    }
    deriving stock (Generic)

instance ToJSON WireRequest where
    toJSON = genericToJSON wireOptions

data WireUsage = WireUsage
    { inputTokens :: Int
    , outputTokens :: Int
    }
    deriving stock (Generic)

instance FromJSON WireUsage where
    parseJSON = genericParseJSON wireOptions{fieldLabelModifier = camelTo2 '_'}

data WireResponse = WireResponse
    { model :: Text
    , answers :: Map Text WireAnswer
    , usage :: WireUsage
    }
    deriving stock (Generic)

instance FromJSON WireResponse where
    parseJSON = genericParseJSON wireOptions

wireOptions :: Options
wireOptions =
    defaultOptions
        { sumEncoding = TaggedObject "type" "contents"
        , constructorTagModifier = map toLower
        , omitNothingFields = True
        }
