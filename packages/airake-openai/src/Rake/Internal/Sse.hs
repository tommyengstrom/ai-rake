{-# LANGUAGE ScopedTypeVariables #-}

module Rake.Internal.Sse
    ( SseStep (..)
    , consumeServerSentEvents
    ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC8
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client (BodyReader)
import Relude

data SseStep a
    = SseContinue
    | SseFinish a
    | SseStop

consumeServerSentEvents
    :: forall a
     . BodyReader
    -> (Maybe Text -> BS.ByteString -> IO (SseStep a))
    -> IO (Maybe a)
consumeServerSentEvents bodyReader onEvent =
    loop BS.empty []
  where
    loop :: BS.ByteString -> [BS.ByteString] -> IO (Maybe a)
    loop leftover currentEventLines = do
        chunk <- bodyReader
        if BS.null chunk
            then drainBuffered leftover currentEventLines
            else do
                let buffered = leftover <> chunk
                    (completeLines, nextLeftover) = splitCompleteLines buffered
                nextStep <- processLines completeLines currentEventLines
                case nextStep of
                    Left maybeFinalResult ->
                        pure maybeFinalResult
                    Right nextEventLines ->
                        loop nextLeftover nextEventLines

    drainBuffered leftover currentEventLines =
        dispatchEvent (currentEventLines <> maybe [] pure (nonEmptyLine leftover)) <&> \case
            SseContinue ->
                Nothing
            SseFinish finalResult ->
                Just finalResult
            SseStop ->
                Nothing

    splitCompleteLines bytes =
        case reverse (BC8.split '\n' bytes) of
            [] ->
                ([], BS.empty)
            trailingLine : reversedCompleteLines ->
                (reverse reversedCompleteLines, trailingLine)

    processLines :: [BS.ByteString] -> [BS.ByteString] -> IO (Either (Maybe a) [BS.ByteString])
    processLines [] currentEventLines =
        pure (Right currentEventLines)
    processLines (rawLine : remainingLines) currentEventLines =
        let line = stripTrailingCarriageReturn rawLine
         in
            if BS.null line
                then do
                    nextStep <- dispatchEvent currentEventLines
                    case nextStep of
                        SseContinue ->
                            processLines remainingLines []
                        SseFinish finalResult ->
                            pure (Left (Just finalResult))
                        SseStop ->
                            pure (Left Nothing)
                else
                    processLines remainingLines (currentEventLines ++ [line])

    dispatchEvent :: [BS.ByteString] -> IO (SseStep a)
    dispatchEvent [] =
        pure SseContinue
    dispatchEvent eventLines =
        case eventPayload eventLines of
            Nothing ->
                pure SseContinue
            Just (maybeEventName, payload) ->
                onEvent maybeEventName payload

    eventPayload eventLines =
        let (maybeEventName, dataLines) = foldl' collectField (Nothing, []) eventLines
         in
            if null dataLines
                then Nothing
                else Just (maybeEventName, BC8.intercalate "\n" dataLines)

    collectField (currentEventName, currentDataLines) line
        | ":" `BS.isPrefixOf` line =
            (currentEventName, currentDataLines)
        | otherwise =
            case BC8.break (== ':') line of
                (fieldName, rest)
                    | rest == "" ->
                        (currentEventName, currentDataLines)
                    | otherwise ->
                        let rawFieldValue = BS.drop 1 rest
                            fieldValue = fromMaybe rawFieldValue (BS.stripPrefix " " rawFieldValue)
                         in
                            case fieldName of
                                "event" ->
                                    (Just (decodeUtf8Lenient fieldValue), currentDataLines)
                                "data" ->
                                    (currentEventName, currentDataLines <> [fieldValue])
                                _ ->
                                    (currentEventName, currentDataLines)

    stripTrailingCarriageReturn =
        fromMaybe <*> BS.stripSuffix "\r"

    nonEmptyLine bytes =
        let line = stripTrailingCarriageReturn bytes
         in
            if BS.null line
                then Nothing
                else Just line

    decodeUtf8Lenient =
        TextEncoding.decodeUtf8With lenientDecode
