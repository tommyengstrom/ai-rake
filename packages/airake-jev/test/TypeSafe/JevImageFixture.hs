{-# LANGUAGE NoOverloadedLists #-}

module TypeSafe.JevImageFixture
    ( readImageRequest
    , lookupJsonPath
    , pngBytes
    , jpegBytes
    ) where

import Data.Aeson (Value (..), eitherDecodeStrict')
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Network.Wai qualified as Wai
import Network.Wai.Parse qualified as Multipart
import Test.Hspec
import Prelude

readImageRequest
    :: Wai.Request -> IO (Value, Map BS.ByteString (BS.ByteString, LBS.ByteString))
readImageRequest request = do
    Wai.requestMethod request `shouldBe` "POST"
    Wai.pathInfo request `shouldBe` ["v1", "systemone"]
    lookup "Authorization" (Wai.requestHeaders request) `shouldBe` Just "Bearer test-key"
    lookup "TypeSafe-Danger-Zone-Images" (Wai.requestHeaders request) `shouldBe` Just "true"
    length (filter ((== "Content-Type") . fst) (Wai.requestHeaders request)) `shouldBe` 1
    bodyType <- case Multipart.getRequestBodyType request of
        Just contentType@(Multipart.Multipart _) -> pure contentType
        Just Multipart.UrlEncoded -> fail "Expected multipart, got URL-encoded request"
        Nothing -> fail "Missing multipart Content-Type"
    rawBody <- LBS.toStrict <$> Wai.strictRequestBody request
    let firstHeaders :: BS.ByteString
        firstHeaders = fst (BS.breakSubstring "\r\n\r\n" rawBody)
    firstHeaders `shouldSatisfy` BS.isInfixOf "name=\"request\""
    firstHeaders `shouldSatisfy` BS.isInfixOf "Content-Type: application/json"
    remaining <- newIORef rawBody
    (parameters, files) <-
        Multipart.sinkRequestBody
            Multipart.lbsBackEnd
            bodyType
            (atomicModifyIORef' remaining (\bytes -> (BS.empty, bytes)))
    map fst parameters `shouldBe` ["request"]
    body <- case lookup "request" parameters of
        Nothing -> fail "Missing request JSON part"
        Just bytes -> either fail pure (eitherDecodeStrict' bytes)
    let uploaded :: Map BS.ByteString (BS.ByteString, LBS.ByteString)
        uploaded =
            Map.fromList
                [ (name, (Multipart.fileContentType file, Multipart.fileContent file))
                | (name, file) <- files
                ]
    Map.size uploaded `shouldBe` length files
    pure (body, uploaded)

lookupJsonPath :: [Key] -> Value -> Maybe Value
lookupJsonPath [] value = Just value
lookupJsonPath (key : keys) value = case value of
    Object fields -> KeyMap.lookup key fields >>= lookupJsonPath keys
    String _ -> Nothing
    Array _ -> Nothing
    Number _ -> Nothing
    Bool _ -> Nothing
    Null -> Nothing

pngBytes :: BS.ByteString
pngBytes = BS.pack [137, 80, 78, 71, 13, 10, 26, 10, 0, 255, 13, 10]

jpegBytes :: BS.ByteString
jpegBytes = BS.pack [255, 216, 255, 224, 0, 13, 10, 128, 255, 217]
