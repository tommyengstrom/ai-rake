module LlmChat.Providers.OpenAI.Images
    ( OpenAIImagesSettings (..)
    , defaultOpenAIImagesSettings
    , OpenAIImageReference (..)
    , OpenAIImageRequest (..)
    , defaultOpenAIImageRequest
    , generateOpenAIImage
    ) where

import Data.Aeson
import Effectful
import Effectful.Error.Static
import LlmChat.Effect
import LlmChat.Media
import LlmChat.Providers.Responses.Internal (defaultWarningLogger)
import Network.HTTP.Client.TLS (newTlsManager)
import Relude
import Servant.API (Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data OpenAIImagesSettings es = OpenAIImagesSettings
    { apiKey :: Text
    , baseUrl :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenAIImagesSettings :: Text -> OpenAIImagesSettings es
defaultOpenAIImagesSettings apiKey =
    OpenAIImagesSettings
        { apiKey
        , baseUrl = "https://api.openai.com"
        , organizationId = Nothing
        , projectId = Nothing
        , requestLogger = defaultWarningLogger "openai.images"
        }

data OpenAIImageReference
    = OpenAIImageUrl Text
    | OpenAIImageFileId Text
    deriving stock (Show, Eq, Generic)

instance ToJSON OpenAIImageReference where
    toJSON = \case
        OpenAIImageUrl imageUrl ->
            object ["image_url" .= imageUrl]
        OpenAIImageFileId fileId ->
            object ["file_id" .= fileId]

data OpenAIImageRequest = OpenAIImageRequest
    { model :: Text
    , prompt :: Text
    , inputImages :: [OpenAIImageReference]
    , mask :: Maybe OpenAIImageReference
    , background :: Maybe Text
    , inputFidelity :: Maybe Text
    , moderation :: Maybe Text
    , n :: Maybe Int
    , outputCompression :: Maybe Int
    , outputFormat :: Maybe Text
    , quality :: Maybe Text
    , size :: Maybe Text
    , user :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

defaultOpenAIImageRequest :: Text -> OpenAIImageRequest
defaultOpenAIImageRequest prompt =
    OpenAIImageRequest
        { model = "gpt-image-1.5"
        , prompt
        , inputImages = []
        , mask = Nothing
        , background = Nothing
        , inputFidelity = Nothing
        , moderation = Nothing
        , n = Nothing
        , outputCompression = Nothing
        , outputFormat = Nothing
        , quality = Nothing
        , size = Nothing
        , user = Nothing
        }

instance ToJSON OpenAIImageRequest where
    toJSON OpenAIImageRequest{model, prompt, inputImages, mask, background, inputFidelity, moderation, n, outputCompression, outputFormat, quality, size, user} =
        object $
            [ "model" .= model
            , "prompt" .= prompt
            ]
                <> catMaybes
                    [ ("images" .=) <$> viaNonEmpty identity (fromList inputImages)
                    , ("mask" .=) <$> mask
                    , ("background" .=) <$> background
                    , ("input_fidelity" .=) <$> inputFidelity
                    , ("moderation" .=) <$> moderation
                    , ("n" .=) <$> n
                    , ("output_compression" .=) <$> outputCompression
                    , ("output_format" .=) <$> outputFormat
                    , ("quality" .=) <$> quality
                    , ("size" .=) <$> size
                    , ("user" .=) <$> user
                    ]

type OpenAIImagesAPI =
    "v1"
        Servant.:> "images"
        Servant.:> ( "generations"
                        Servant.:> Header "Authorization" Text
                        Servant.:> Header "OpenAI-Organization" Text
                        Servant.:> Header "OpenAI-Project" Text
                        Servant.:> ReqBody '[JSON] Value
                        Servant.:> Post '[JSON] Value
                  Servant.:<|> "edits"
                        Servant.:> Header "Authorization" Text
                        Servant.:> Header "OpenAI-Organization" Text
                        Servant.:> Header "OpenAI-Project" Text
                        Servant.:> ReqBody '[JSON] Value
                        Servant.:> Post '[JSON] Value
                  )

openAIImagesApi :: Proxy OpenAIImagesAPI
openAIImagesApi = Proxy

generateOpenAIImage
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => OpenAIImagesSettings es
    -> OpenAIImageRequest
    -> Eff es ImageGenerationResponse
generateOpenAIImage OpenAIImagesSettings{apiKey, baseUrl, organizationId, projectId, requestLogger} request@OpenAIImageRequest{inputImages, mask} = do
    when (null inputImages && isJust mask) $
        throwError (LlmExpectationError "OpenAI image masks require at least one input image")

    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        createImage Servant.:<|> editImage = client openAIImagesApi
        requestBody = toJSON (requestBodyForRoute request)
        clientCall
            | null inputImages =
                createImage
                    (Just ("Bearer " <> apiKey))
                    organizationId
                    projectId
                    requestBody
            | otherwise =
                editImage
                    (Just ("Bearer " <> apiKey))
                    organizationId
                    projectId
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
                throwError (LlmExpectationError ("Failed to decode OpenAI image response: " <> err))
            Success response ->
                pure response

requestBodyForRoute :: OpenAIImageRequest -> OpenAIImageRequest
requestBodyForRoute request@OpenAIImageRequest{inputImages}
    | null inputImages =
        request{mask = Nothing, inputFidelity = Nothing}
    | otherwise =
        request
