{-# LANGUAGE NoOverloadedLists #-}

module RakeCliModels
    ( CliModel (..)
    , ModelArguments (..)
    , qualifiedModel
    , resolveModel
    , parseModelArguments
    , modelHelpLines
    , modelOptionLines
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude

data CliModel a = CliModel
    { provider :: Text
    , modelName :: Text
    , target :: a
    }
    deriving stock (Show, Eq)

data ModelArguments = ModelArguments
    { selector :: Maybe Text
    , remainingArguments :: [Text]
    , wantsHelp :: Bool
    }
    deriving stock (Show, Eq)

qualifiedModel :: CliModel a -> Text
qualifiedModel CliModel{provider, modelName} = provider <> "/" <> modelName

resolveModel :: NonEmpty (CliModel a) -> Maybe Text -> Either Text (CliModel a)
resolveModel catalog@(defaultModel :| _) = \case
    Nothing -> Right defaultModel
    Just requested ->
        case filter ((== requested) . qualifiedModel) (NonEmpty.toList catalog) of
            [] ->
                unique
                    requested
                    ( filter
                        (\CliModel{modelName} -> modelName == snd (Text.breakOnEnd "/" requested))
                        (NonEmpty.toList catalog)
                    )
            matches -> unique requested matches
  where
    unique :: Text -> [CliModel a] -> Either Text (CliModel a)
    unique requested = \case
        [entry] -> Right entry
        [] ->
            Left
                ( "Unknown model: "
                    <> requested
                    <> ". Available models: "
                    <> Text.intercalate ", " (map qualifiedModel (NonEmpty.toList catalog))
                )
        matches ->
            Left
                ( "Ambiguous model: "
                    <> requested
                    <> ". Choose one of: "
                    <> Text.intercalate ", " (map qualifiedModel matches)
                )

parseModelArguments :: [Text] -> [Text] -> Either Text ModelArguments
parseModelArguments valueOptions = go Nothing False []
  where
    go :: Maybe Text -> Bool -> [Text] -> [Text] -> Either Text ModelArguments
    go selector wantsHelp reversed = \case
        [] -> Right ModelArguments{selector, wantsHelp, remainingArguments = reverse reversed}
        "--" : rest ->
            Right
                ModelArguments
                    { selector
                    , wantsHelp
                    , remainingArguments = reverse reversed <> ("--" : rest)
                    }
        arg : rest
            | arg == "-m" || arg == "--model" -> case rest of
                [] -> Left (arg <> " requires a model")
                value : remaining -> select selector wantsHelp reversed value remaining
            | Just value <- Text.stripPrefix "--model=" arg ->
                select selector wantsHelp reversed value rest
            | Just value <- Text.stripPrefix "-m" arg
            , not (Text.null value) ->
                select selector wantsHelp reversed value rest
            | arg == "--help" || arg == "-h" -> go selector True ("--help" : reversed) rest
            | arg `elem` valueOptions -> case rest of
                [] -> Left (arg <> " requires an argument")
                value : remaining -> go selector wantsHelp (value : arg : reversed) remaining
            | otherwise -> go selector wantsHelp (arg : reversed) rest

    select :: Maybe Text -> Bool -> [Text] -> Text -> [Text] -> Either Text ModelArguments
    select previous wantsHelp reversed value remaining
        | Text.null value || "-" `Text.isPrefixOf` value = Left "--model requires a model"
        | otherwise = case previous of
            Just _ -> Left "Use -m or --model only once"
            Nothing -> go (Just value) wantsHelp reversed remaining

modelHelpLines :: NonEmpty (CliModel a) -> [Text]
modelHelpLines (defaultModel :| others) =
    ["Models:", "  " <> qualifiedModel defaultModel <> " (default)"]
        <> map (("  " <>) . qualifiedModel) others
        <> [ ""
           , "Use provider/model or a unique model name. An unmatched provider prefix falls back to the model name."
           ]

modelOptionLines :: [Text]
modelOptionLines = ["  -m, --model MODEL            Select a supported model and its provider."]
