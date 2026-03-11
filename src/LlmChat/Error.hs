{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LlmChat.Error where

import Data.Generics.Labels ()
import Servant.Client (ClientError)
import Relude

data LlmChatError
    = LlmClientError ClientError
    | LlmExpectationError String
    | ToolLoopLimitExceeded Int
    deriving stock (Show, Eq, Generic)
