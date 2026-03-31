module StructuredSchemaTestTypes where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Rake
import Relude

data ExampleToolCall = ExampleToolCall
    { tool :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data NonNullarySum
    = SumText
        { value :: Text
        }
    | SumCount
        { count :: Int
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data NullaryEnum
    = Alpha
    | Beta
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data RecordWithMaybe = RecordWithMaybe
    { response :: Text
    , translated_response :: Text
    , toolCall :: Maybe ExampleToolCall
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data NestedChild = NestedChild
    { maybeText :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data NestedRecord = NestedRecord
    { label :: Text
    , child :: Maybe NestedChild
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

maybeTool :: ToolDef es
maybeTool =
    defineToolWithArgument
        @RecordWithMaybe
        "lookup"
        "lookup tool"
        (\_ -> pure (Right "ok"))
