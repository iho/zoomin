{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    decode,
    fromEncoding,
  )
import Data.ByteString.Lazy as BL (fromChunks)
import qualified Data.ByteString.Lazy as L
import Data.Text as T (Text)
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import NeatInterpolation (text)
import Network.HTTP.Types
  ( hContentType,
    status200,
    status400,
    status404,
    status405,
  )
import Network.Wai
  ( Application,
    Request (queryString, rawPathInfo, requestHeaders, requestMethod),
    Response,
    responseBuilder,
    responseLBS,
    strictRequestBody,
  )
import Network.Wai.Handler.Warp (run)

app :: Application
app request respond = do
  response <- case rawPathInfo request of
    "/api/covid19" -> do
      covid19APIHandler request
    "/covid19.html" -> pure $ covid19Handler request
    _ -> pure $ responseLBS status404 [] "Not Found"
  respond response

covid19APIHandler :: Request -> IO Response
covid19APIHandler request = case requestMethod request of
  "POST" -> case lookup "Content-Type" (requestHeaders request) of
    Just _ -> do
      payload <- strictRequestBody request
      case decode payload of
        Just covid19Data -> pure $ jsonResponse $ Covid19Data (name covid19Data) (tel covid19Data) (covid19 covid19Data) (Just "ok")
        Nothing -> pure $ responseLBS status400 [] "Invalid JSON"
    Nothing -> pure $ responseLBS status400 [] "Invalid Content-Type"
  _ -> pure $ responseLBS status405 [] "Method Not Allowed"

covid19Handler :: Request -> Response
covid19Handler request = do
  let params = queryString request
  case (lookup "name" params, lookup "tel" params) of
    (Just (Just n), Just (Just t)) ->
      responseLBS status200 [(hContentType, "text/html")] $ toByteString $ responseWithParams (decodeUtf8 n) (decodeUtf8 t) (decodeUtf8 $ maybe "false" (const "true") (lookup "covid19" params))
    _ -> responseLBS status200 [(hContentType, "text/html")] $ toByteString defaultResponse

toByteString :: Text -> L.ByteString
toByteString = fromChunks . return . encodeUtf8

jsonResponse :: ToJSON a => a -> Response
jsonResponse =
  responseBuilder status200 [(hContentType, "application/json")]
    . fromEncoding
    . toEncoding

main :: IO ()
main = do
  Prelude.putStrLn "http://localhost:8080/"
  run 8080 app

data Covid19Data = Covid19Data
  { name :: String,
    tel :: String,
    covid19 :: Maybe Bool,
    status :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Covid19Data

instance ToJSON Covid19Data

defaultResponse :: Text
defaultResponse =
  [text|<html>
  <head>
    <title>Shabak COVID-19 Registration</title>
  </head>
  <body>
    <h1>Shabak COVID-19 Registration</h1>
    <form>
      <div>
        <label for="name">Name: </label>
        <input type="text" name="name" id="name" />
      </div>
      <div>
        <label for="tel">Tel: </label>
        <input type="text" name="tel" id="tel" />
      </div>
      <div>
        <input type="checkbox" name="covid19" id="covid19" />
        <label for="covid19"> Tested positive for COVID-19</label>
      </div>
      <div>
        <input type="submit" />
      </div>
    </form>
  </body>
</html>
|]

responseWithParams :: Text -> Text -> Text -> Text
responseWithParams name_ tel_ covid19_ =
  [text|
<html>
  <head>
    <title>Shabak COVID-19 Registration</title>
  </head>
  <body>
    <h1>Shabak COVID-19 Registration</h1>
    <h2>Registration successful:</h2>
    <ul>
      <li>Name: $name_</li>
      <li>Tel: $tel_</li>
      <li>COVID-19: $covid19_</li>
    </ul>
  </body>
</html>
|]
