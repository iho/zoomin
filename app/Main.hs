{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson
import Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as L
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Data.Text.Lazy.IO as TL
import GHC.Generics
import NeatInterpolation (text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse
  ( Param,
    lbsBackEnd,
    parseRequestBody,
  )

app request respond = do
  (params, _) <- parseRequestBody lbsBackEnd request
  payload <- strictRequestBody request
  Prelude.putStrLn $ "request: " ++ (show params)
  respond $ case rawPathInfo request of
    "/api/covid19" -> do
      covid19APIHandler request payload
    "/covid19.html" -> covid19Handler request params
    _ -> responseLBS status404 [] "Not Found"

covid19APIHandler :: Request -> L.ByteString -> Response
covid19APIHandler request payload = case requestMethod request of
  "POST" -> case lookup "Content-Type" (requestHeaders request) of
    Just _ ->
      case (decode payload) :: Maybe Covid19Data of
        Just covid19Data -> jsonResponse $ Covid19Data (name covid19Data) (tel covid19Data) (Just True)
        Nothing -> responseLBS status400 [] "Invalid JSON"
    Nothing -> responseLBS status400 [] "Invalid Content-Type"
  _ -> responseLBS status405 [] "Method Not Allowed"

covid19Handler :: Request -> [Param] -> Response
covid19Handler request params = case requestMethod request of
  "GET" -> responseLBS status200 [(hContentType, "text/html")] $ BL.fromChunks . return . T.encodeUtf8 $ defaultResponse
  "POST" -> case lookup "Content-Type" (requestHeaders request) of
    Just _ -> do
      let name = lookup "name" params
      let tel = lookup "tel" params
      let covid19 = lookup "covid19" params
      case (name, tel, covid19) of
        (Just name, Just tel, _) -> responseLBS status200 [(hContentType, "text/html")] $ BL.fromChunks . return . T.encodeUtf8 $ responseWithParams (T.decodeUtf8 name) (T.decodeUtf8 tel) (T.decodeUtf8 $ maybe "false" (\x -> "true") covid19)
        _ -> responseLBS status400 [] $ TL.encodeUtf8 . TL.pack $ "Not enough parameters" ++ "name: " ++ (show name) ++ ", tel: " ++ (show tel) ++ ", covid19: " ++ (show covid19) ++ ", params: " ++ (show params)
    Nothing -> responseLBS status400 [] "Invalid Content-Type"
  _ -> responseLBS status405 [] "Method Not Allowed"

jsonResponse :: ToJSON a => a -> Response
jsonResponse =
  responseBuilder status200 [(hContentType, "application/json")]
    . fromEncoding
    . toEncoding

main :: IO ()
main = do
  TL.putStrLn $ "http://localhost:8080/"
  run 8080 app

data Covid19Data = Covid19Data
  { name :: String,
    tel :: String,
    covid19 :: Maybe Bool
  }
  deriving (Show, Generic)

instance FromJSON Covid19Data

instance ToJSON Covid19Data

defaultResponse =
  [text|<html>
  <head>
    <title>Shabak COVID-19 Registration</title>
  </head>
  <body>
    <h1>Shabak COVID-19 Registration</h1>
    <form method="POST">
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

responseWithParams :: T.Text -> T.Text -> T.Text -> T.Text
responseWithParams name tel covid19 =
  [text|
<html>
  <head>
    <title>Shabak COVID-19 Registration</title>
  </head>
  <body>
    <h1>Shabak COVID-19 Registration</h1>
    <h2>Registration successful:</h2>
    <ul>
      <li>Name: $name</li>
      <li>Tel: $tel</li>
      <li>COVID-19: $covid19</li>
    </ul>
  </body>
</html>
|]
