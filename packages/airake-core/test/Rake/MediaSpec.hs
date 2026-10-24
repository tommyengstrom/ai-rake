{-# LANGUAGE NoOverloadedLists #-}
module Rake.MediaSpec where
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Rake.Media
import Relude
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, elements, forAll, listOf, oneof, (===))

spec :: Spec
spec = describe "media providers" $ do
    describe "image response parsing" $ do
        it "round-trips generated image payloads and MIME metadata"
            $ forAll genGeneratedImage
            $ \image ->
                fromJSON (toJSON image) === Success image

        it "round-trips image responses with optional provider metadata"
            $ forAll genImageGenerationResponse
            $ \response ->
                fromJSON (toJSON response) === Success response

        it "preserves OpenAI output metadata and nested token usage" $ do
            let responseValue :: Value
                responseValue =
                    object
                        [ "created" .= (123 :: Int)
                        , "model" .= ("gpt-image-2.5-flare" :: Text)
                        , "data" .= [object ["b64_json" .= ("YWJj" :: Text)]]
                        , "output_format" .= ("webp" :: Text)
                        , "size" .= ("1536x864" :: Text)
                        , "quality" .= ("max" :: Text)
                        , "background" .= ("transparent" :: Text)
                        , "usage"
                            .= object
                                [ "input_tokens" .= (12 :: Int)
                                , "output_tokens" .= (34 :: Int)
                                , "input_tokens_details" .= object ["image_tokens" .= (8 :: Int)]
                                ]
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success responseValue

        it "preserves xAI MIME types, resolved models, and exact cost ticks" $ do
            let responseValue :: Value
                responseValue =
                    object
                        [ "model" .= ("grok-imagine-image-2.0" :: Text)
                        , "data"
                            .= [ object
                                    ["url" .= ("https://example.com/image" :: Text), "mime_type" .= ("image/jpeg" :: Text)]
                               ]
                        , "usage"
                            .= object
                                [ "cost_in_usd_ticks" .= (9007199254740993 :: Integer)
                                , "input_tokens" .= Null
                                , "provider_extension" .= object ["value" .= True]
                                ]
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success responseValue

        it "normalizes Gemini model and usage metadata and snake-case inline images" $ do
            let usageValue :: Value
                usageValue = object ["totalTokenCount" .= (42 :: Int)]
                responseValue :: Value
                responseValue =
                    object
                        [ "modelVersion" .= ("gemini-2.5-flash-image" :: Text)
                        , "usageMetadata" .= usageValue
                        , "candidates"
                            .= [ object
                                    [ "content"
                                        .= object
                                            [ "parts"
                                                .= [ object
                                                        [ "inline_data"
                                                            .= object
                                                                ["mime_type" .= ("image/png" :: Text), "data" .= ("YWJj" :: Text)]
                                                        ]
                                                   ]
                                            ]
                                    ]
                               ]
                        ]
                expectedValue :: Value
                expectedValue =
                    object
                        [ "model" .= ("gemini-2.5-flash-image" :: Text)
                        , "usage" .= usageValue
                        , "data" .= [object ["b64_json" .= ("YWJj" :: Text), "mime_type" .= ("image/png" :: Text)]]
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success expectedValue

        it "accepts null optional response metadata" $ do
            let responseValue :: Value
                responseValue =
                    object
                        [ "created" .= Null
                        , "data" .= ([] :: [Value])
                        , "model" .= Null
                        , "usage" .= Null
                        , "output_format" .= Null
                        , "size" .= Null
                        , "quality" .= Null
                        , "background" .= Null
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success (object ["data" .= ([] :: [Value])])

        it "parses GPT Image responses with base64 payloads" $ do
            let responseValue =
                    object
                        [ "created" .= (123 :: Int)
                        , "data"
                            .= ( [ object
                                    [ "b64_json" .= ("YWJj" :: Text)
                                    , "revised_prompt" .= ("A refined prompt" :: Text)
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Just 123
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Nothing
                                , b64Json = Just "YWJj"
                                , revisedPrompt = Just "A refined prompt"
                                , mimeType = Nothing
                                }
                            ]
                        }

        it "parses URL-based image responses" $ do
            let responseValue =
                    object
                        [ "data"
                            .= ( [ object
                                    [ "url" .= ("https://example.com/image.png" :: Text)
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Nothing
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Just "https://example.com/image.png"
                                , b64Json = Nothing
                                , revisedPrompt = Nothing
                                , mimeType = Nothing
                                }
                            ]
                        }

        it "parses Gemini inlineData image responses" $ do
            let responseValue =
                    object
                        [ "candidates"
                            .= ( [ object
                                    [ "content"
                                        .= object
                                            [ "parts"
                                                .= ( [ object
                                                        [ "inlineData"
                                                            .= object
                                                                [ "mimeType" .= ("image/jpeg" :: Text)
                                                                , "data" .= ("YWJj" :: Text)
                                                                ]
                                                        ]
                                                     ]
                                                        :: [Value]
                                                   )
                                            ]
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Nothing
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Nothing
                                , b64Json = Just "YWJj"
                                , revisedPrompt = Nothing
                                , mimeType = Just "image/jpeg"
                                }
                            ]
                        }

        it "ignores non-image Gemini content parts" $ do
            let responseValue =
                    object
                        [ "candidates"
                            .= ( [ object
                                    [ "content"
                                        .= object
                                            [ "parts"
                                                .= ( [ object ["text" .= ("ignore me" :: Text)]
                                                     , object
                                                        [ "inlineData"
                                                            .= object
                                                                [ "mimeType" .= ("image/png" :: Text)
                                                                , "data" .= ("YWJj" :: Text)
                                                                ]
                                                        ]
                                                     ]
                                                        :: [Value]
                                                   )
                                            ]
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Nothing
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Nothing
                                , b64Json = Just "YWJj"
                                , revisedPrompt = Nothing
                                , mimeType = Just "image/png"
                                }
                            ]
                        }

genOptionalText :: Gen (Maybe Text)
genOptionalText = fmap toText <$> arbitrary @(Maybe String)

genGeneratedImage :: Gen GeneratedImage
genGeneratedImage = do
    payload <- toText <$> arbitrary @String
    (url, b64Json) <-
        elements [(Just payload, Nothing), (Nothing, Just payload), (Just payload, Just payload)]
    revisedPrompt <- genOptionalText
    mimeType <- genOptionalText
    pure GeneratedImage{url, b64Json, revisedPrompt, mimeType}

genImageGenerationResponse :: Gen ImageGenerationResponse
genImageGenerationResponse = do
    created <- arbitrary
    images <- listOf genGeneratedImage
    model <- genOptionalText
    outputFormat <- genOptionalText
    size <- genOptionalText
    quality <- genOptionalText
    background <- genOptionalText
    ticks <- arbitrary @Integer
    usage <-
        oneof
            [ pure Nothing
            , pure
                ( Just
                    ( KM.fromList
                        [ ("cost_in_usd_ticks", toJSON ticks)
                        , ("input_tokens_details", object ["text_tokens" .= ticks])
                        ]
                    )
                )
            ]
    pure
        ImageGenerationResponse
            { created
            , images
            , model
            , outputFormat
            , size
            , quality
            , background
            , usage
            }

