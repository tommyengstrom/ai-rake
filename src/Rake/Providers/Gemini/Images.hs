module Rake.Providers.Gemini.Images
    ( GeminiImagesSettings (..)
    , defaultGeminiImagesSettings
    , GeminiHarmCategory (..)
    , GeminiHarmBlockThreshold (..)
    , GeminiSafetySetting (..)
    , leastRestrictiveGeminiSafetySettings
    , GeminiInlineImage (..)
    , GeminiImageRequest (..)
    , defaultGeminiImageRequest
    , generateGeminiImage
    ) where

import Data.Aeson
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Rake.Effect
import Rake.Media
import Rake.Providers.Internal (defaultWarningLogger)
import Relude
import Servant.API (Capture, Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data GeminiImagesSettings es = GeminiImagesSettings
    { apiKey :: Text
    , baseUrl :: Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultGeminiImagesSettings :: Text -> GeminiImagesSettings es
defaultGeminiImagesSettings apiKey =
    GeminiImagesSettings
        { apiKey
        , baseUrl = "https://generativelanguage.googleapis.com"
        , requestLogger = defaultWarningLogger "gemini.images"
        }

data GeminiHarmCategory
    = GeminiHarmCategoryHarassment
    | GeminiHarmCategoryHateSpeech
    | GeminiHarmCategorySexuallyExplicit
    | GeminiHarmCategoryDangerousContent
    | GeminiHarmCategoryCivicIntegrity
    deriving stock (Show, Eq, Generic)

instance ToJSON GeminiHarmCategory where
    toJSON =
        String . \case
            GeminiHarmCategoryHarassment ->
                "HARM_CATEGORY_HARASSMENT"
            GeminiHarmCategoryHateSpeech ->
                "HARM_CATEGORY_HATE_SPEECH"
            GeminiHarmCategorySexuallyExplicit ->
                "HARM_CATEGORY_SEXUALLY_EXPLICIT"
            GeminiHarmCategoryDangerousContent ->
                "HARM_CATEGORY_DANGEROUS_CONTENT"
            GeminiHarmCategoryCivicIntegrity ->
                "HARM_CATEGORY_CIVIC_INTEGRITY"

data GeminiHarmBlockThreshold
    = GeminiHarmBlockThresholdOff
    | GeminiHarmBlockThresholdBlockNone
    | GeminiHarmBlockThresholdBlockOnlyHigh
    | GeminiHarmBlockThresholdBlockMediumAndAbove
    | GeminiHarmBlockThresholdBlockLowAndAbove
    | GeminiHarmBlockThresholdUnspecified
    deriving stock (Show, Eq, Generic)

instance ToJSON GeminiHarmBlockThreshold where
    toJSON =
        String . \case
            GeminiHarmBlockThresholdOff ->
                "OFF"
            GeminiHarmBlockThresholdBlockNone ->
                "BLOCK_NONE"
            GeminiHarmBlockThresholdBlockOnlyHigh ->
                "BLOCK_ONLY_HIGH"
            GeminiHarmBlockThresholdBlockMediumAndAbove ->
                "BLOCK_MEDIUM_AND_ABOVE"
            GeminiHarmBlockThresholdBlockLowAndAbove ->
                "BLOCK_LOW_AND_ABOVE"
            GeminiHarmBlockThresholdUnspecified ->
                "HARM_BLOCK_THRESHOLD_UNSPECIFIED"

data GeminiSafetySetting = GeminiSafetySetting
    { category :: GeminiHarmCategory
    , threshold :: GeminiHarmBlockThreshold
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON GeminiSafetySetting where
    toJSON GeminiSafetySetting{category, threshold} =
        object
            [ "category" .= category
            , "threshold" .= threshold
            ]

leastRestrictiveGeminiSafetySettings :: [GeminiSafetySetting]
leastRestrictiveGeminiSafetySettings =
    map
        ( \category ->
            GeminiSafetySetting
                { category
                , threshold = GeminiHarmBlockThresholdOff
                }
        )
        [ GeminiHarmCategoryHarassment
        , GeminiHarmCategoryHateSpeech
        , GeminiHarmCategorySexuallyExplicit
        , GeminiHarmCategoryDangerousContent
        ]

data GeminiInlineImage = GeminiInlineImage
    { mimeType :: Text
    , base64Data :: Text
    }
    deriving stock (Show, Eq, Generic)

data GeminiImageRequest = GeminiImageRequest
    { model :: Text
    , prompt :: Text
    , inputImages :: [GeminiInlineImage]
    , aspectRatio :: Maybe Text
    , imageSize :: Maybe Text
    , safetySettings :: [GeminiSafetySetting]
    }
    deriving stock (Show, Eq, Generic)

defaultGeminiImageRequest :: Text -> GeminiImageRequest
defaultGeminiImageRequest prompt =
    GeminiImageRequest
        { model = "gemini-2.5-flash-image"
        , prompt
        , inputImages = []
        , aspectRatio = Nothing
        , imageSize = Nothing
        , safetySettings = leastRestrictiveGeminiSafetySettings
        }

instance ToJSON GeminiImageRequest where
    toJSON GeminiImageRequest{prompt, inputImages, aspectRatio, imageSize, safetySettings} =
        object $
            [ "contents"
                .= ( [ object
                            [ "role" .= ("user" :: Text)
                            , "parts"
                                .= ( textPart prompt
                                        : map inlineImagePart inputImages
                                   )
                                ]
                      ]
                        :: [Value]
                   )
            , "generationConfig"
                .= object
                    [ "responseModalities" .= (["TEXT", "IMAGE"] :: [Text])
                    , "imageConfig" .= object (catMaybes [("aspectRatio" .=) <$> aspectRatio, ("imageSize" .=) <$> imageSize])
                    ]
            ]
                <> if null safetySettings
                    then []
                    else ["safetySettings" .= safetySettings]
      where
        textPart promptText =
            object ["text" .= promptText]

        inlineImagePart GeminiInlineImage{mimeType, base64Data} =
            object
                [ "inline_data"
                    .= object
                        [ "mime_type" .= mimeType
                        , "data" .= base64Data
                        ]
                ]

type GeminiImagesAPI =
    "v1beta"
        Servant.:> "models"
        Servant.:> Capture "model_action" Text
        Servant.:> Header "x-goog-api-key" Text
        Servant.:> ReqBody '[JSON] Value
        Servant.:> Post '[JSON] Value

geminiImagesApi :: Proxy GeminiImagesAPI
geminiImagesApi = Proxy

generateGeminiImage
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => GeminiImagesSettings es
    -> GeminiImageRequest
    -> Eff es ImageGenerationResponse
generateGeminiImage GeminiImagesSettings{apiKey, baseUrl, requestLogger} request@GeminiImageRequest{model} = do
    manager <- liftIO $ newTlsManagerWith tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 180_000_000}
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postGenerateContent = client geminiImagesApi
        requestBody = toJSON request
        clientCall =
            postGenerateContent
                (model <> ":generateContent")
                (Just apiKey)
                requestBody

    requestLogger (NativeMsgOut requestBody)
    responseValue <-
        liftIO (runClientM clientCall clientEnv) >>= \case
            Left err -> do
                requestLogger (NativeRequestFailure err)
                throwError (LlmClientError err)
            Right responseValue -> pure responseValue
    requestLogger (NativeMsgIn responseValue)
    decodeResponse responseValue
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

    decodeResponse responseValue =
        case fromJSON responseValue of
            Error err ->
                throwError (LlmExpectationError ("Failed to decode Gemini image response: " <> err))
            Success response ->
                pure response
