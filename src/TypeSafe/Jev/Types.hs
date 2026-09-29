module TypeSafe.Jev.Types
    ( Content (..)
    , Probability
    , probabilityValue
    , Confidence
    , confidenceValue
    , ChoiceOption (..)
    , shownOption
    , ChoiceQuestion (..)
    , ScoreQuestion (..)
    , NoulCriteria (..)
    , NoulQuestion (..)
    , ChoiceAssessment (..)
    , ScoreAssessment (..)
    , NoulAssessment (..)
    , CallMetadata (..)
    , Response (..)
    ) where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..))
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Prelude

data Content
    = TextContent Text
    | ObjectContent Object
    | ArrayContent [Value]
    deriving stock (Show, Eq)

instance IsString Content where
    fromString = TextContent . Text.pack

instance ToJSON Content where
    toJSON = \case
        TextContent text -> String text
        ObjectContent fields -> Object fields
        ArrayContent entries -> Array (Vector.fromList entries)

instance FromJSON Content where
    parseJSON = \case
        String text -> pure (TextContent text)
        Object fields -> pure (ObjectContent fields)
        Array entries -> pure (ArrayContent (Vector.toList entries))
        Number _ -> fail "Content must be a string, object, or array"
        Bool _ -> fail "Content must be a string, object, or array"
        Null -> fail "Content must be a string, object, or array"

newtype Probability = Probability Double
    deriving stock (Show, Eq)
    deriving newtype (ToJSON)

probabilityValue :: Probability -> Double
probabilityValue (Probability value) = value

instance FromJSON Probability where
    parseJSON value = Probability <$> parseUnitInterval value

newtype Confidence = Confidence Double
    deriving stock (Show, Eq)
    deriving newtype (ToJSON)

confidenceValue :: Confidence -> Double
confidenceValue (Confidence value) = value

instance FromJSON Confidence where
    parseJSON value = Confidence <$> parseUnitInterval value

parseUnitInterval :: Value -> Parser Double
parseUnitInterval value = do
    number <- parseJSON value
    if isNaN number || isInfinite number || number < 0 || number > 1
        then fail "Expected a finite number between 0 and 1"
        else pure number

data ChoiceOption a = ChoiceOption
    { name :: Text
    , value :: a
    , description :: Maybe Content
    }
    deriving stock (Show, Eq)

shownOption :: Show a => a -> Maybe Content -> ChoiceOption a
shownOption value description =
    ChoiceOption{name = Text.pack (show value), value, description}

data ChoiceQuestion a = ChoiceQuestion
    { instructions :: Maybe Content
    , options :: NonEmpty (ChoiceOption a)
    }
    deriving stock (Show, Eq)

data ScoreQuestion = ScoreQuestion
    { instructions :: Maybe Content
    , levels :: NonEmpty Content
    }
    deriving stock (Show, Eq)

data NoulCriteria = NoulCriteria
    { whenTrue :: Maybe Content
    , whenFalse :: Maybe Content
    }
    deriving stock (Show, Eq)

data NoulQuestion = NoulQuestion
    { instructions :: Maybe Content
    , criteria :: Maybe NoulCriteria
    }
    deriving stock (Show, Eq)

data ChoiceAssessment a = ChoiceAssessment
    { chosen :: ChoiceOption a
    , probabilities :: NonEmpty (ChoiceOption a, Probability)
    , confidence :: Confidence
    }
    deriving stock (Show, Eq)

data ScoreAssessment = ScoreAssessment
    { scoreValue :: Double
    , levels :: NonEmpty (Content, Probability)
    , confidence :: Confidence
    }
    deriving stock (Show, Eq)

newtype NoulAssessment = NoulAssessment
    { probability :: Probability
    }
    deriving stock (Show, Eq)

data CallMetadata = CallMetadata
    { model :: Text
    , inputTokens :: Int
    , outputTokens :: Int
    }
    deriving stock (Show, Eq)

data Response a = Response
    { answers :: a
    , metadata :: Maybe CallMetadata
    }
    deriving stock (Show, Eq)
