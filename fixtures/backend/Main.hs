{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (unless)
import Effectful
import Effectful.Error.Static
import Rake.Backend
import Rake.Effect
import Rake.Types

-- This component intentionally depends on core alone, like an external adapter.
runExampleBackend :: (IOE :> es, Error RakeError :> es) => Eff (Rake ': es) a -> Eff es a
runExampleBackend = runChatProvider roundTrip streamingRound
  where
    roundTrip
        :: [ToolDeclaration]
        -> ResponseFormat
        -> SamplingOptions
        -> [HistoryItem]
        -> Eff es ProviderRound
    roundTrip _ _ _ _ = pure (ProviderRound [assistantText "external backend"] [] ProviderRoundDone)
    streamingRound
        :: StreamCallbacks es
        -> [ToolDeclaration]
        -> ResponseFormat
        -> SamplingOptions
        -> [HistoryItem]
        -> Eff es ProviderRound
    streamingRound _ = roundTrip

main :: IO ()
main = do
    result <-
        runEff $
            runErrorNoCallStack @RakeError $
                runExampleBackend $
                    getLlmResponse [] Unstructured defaultSamplingOptions [user "hello"]
    unless
        (result == Right (ProviderRound [assistantText "external backend"] [] ProviderRoundDone)) $
        fail (show result)
