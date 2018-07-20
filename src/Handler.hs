-- |
-- Module      : Handler
-- Description : Effectful request handling logic
--
module Handler (response) where

import           Prelude

import           Data.Aeson           ((.=))
import qualified Data.Aeson           as Aeson
import           Data.ByteString.Lazy (ByteString, fromStrict)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text (encodeUtf8)
import           Network.HTTP.Req
    ( GET(..)
    , NoReqBody(..)
    , POST(..)
    , Req
    , ReqBodyJson(..)
    , https
    , jsonResponse
    , req
    , (/:)
    )
import qualified Network.HTTP.Types   as Http
import           Network.Wai          (Request, Response, responseLBS)

import           Control.Monad.Effect (Effect, JSON)
import qualified Control.Monad.Effect as Effect


response :: Request -> Effect Response
response _ = do
    -- Fire some request sequentially
    r1 <- Effect.requestJSON exampleGET
    r2 <- Effect.requestJSON examplePOST
    case (r1, r2) of
        (Left _, Left _) -> Effect.logError "serial requests: everything broke"
        (Left _, _     ) -> Effect.logError "serial requests: GET failed"
        (_     , Left _) -> Effect.logError "serial requests: POST failed"
        _                -> Effect.logInfo "serial requests: ok"

    -- Fire a bunch of requests concurrently
    results <- Effect.runConcurrently
        [ Effect.requestJSON exampleGET
        , Effect.requestJSON exampleGET
        , Effect.requestJSON exampleGET
        , Effect.requestJSON exampleGET
        , Effect.requestJSON exampleGET
        , Effect.requestJSON exampleGET
        , Effect.requestJSON exampleGET
        , Effect.requestJSON exampleGET
        ]
    case sequence results of
        Left  _ -> Effect.logError "async requests: failed"
        Right _ -> Effect.logInfo "async requests: ok"

    -- Read in a file
    cabal <- Effect.readFile "free-server.cabal"
    case cabal of
        Left  _ -> Effect.logError "file open: failed"
        Right _ -> Effect.logInfo "file open: ok"


    count <- Effect.getCounter
    pure $ plain200 ("count is " <> showText count)


-- | An example GET request.
exampleGET :: Req JSON
exampleGET =
    req GET (https "httpbin.org" /: "get") NoReqBody jsonResponse mempty


-- | An example POST request.
examplePOST :: Req JSON
examplePOST = req POST
                  (https "httpbin.org" /: "post")
                  (ReqBodyJson payload)
                  jsonResponse
                  mempty
    where payload = Aeson.object ["foo" .= (10 :: Int), "bar" .= (20 :: Int)]


-- | Construct a plain text HTTP response (status 200).
plain200 :: Text -> Response
plain200 body = responseLBS
    Http.status200
    [(Http.hContentType, "text/plain; charset=utf-8")]
    (encodeUtf8 body)


encodeUtf8 :: Text -> ByteString
encodeUtf8 = fromStrict . Text.encodeUtf8


showText :: Show a => a -> Text
showText = Text.pack . show
