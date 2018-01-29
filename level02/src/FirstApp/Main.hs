{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Method, Status, hContentType,
                                           status200, status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types           (ContentType (PlainText), Error (EmptyCommentError, EmptyTopicError, UnknownRequestError),
                                           RqType (AddRq, ListRq, ViewRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType body =
  responseLBS status
     [("Content-Type", renderContentType contentType)]
     body

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t c = AddRq <$> (mkTopic t) <*> (mkCommentText . decodeUtf8 . LBS.toStrict $ c)

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify. -- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t = ViewRq <$> mkTopic t

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopicError     = resp400 PlainText "Empty Topic Oops"
mkErrorResponse EmptyCommentError   = resp400 PlainText "Empty Comment Oops"
mkErrorResponse UnknownRequestError = resp404 PlainText "Not Found"


-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r = do
  body <- strictRequestBody r
  let path   = pathInfo r
      method = requestMethod r
  pure $ getRequest body path method

getRequest :: LBS.ByteString -> [Text] -> Method -> Either Error RqType
getRequest _ ("list":[]) "GET"          = mkListRequest
getRequest _ (topic:"view":[]) "GET"    = mkViewRequest topic
getRequest body (topic:"add":[]) "POST" = mkAddRequest topic body
getRequest _ _ _                        = Left UnknownRequestError

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right $ resp200 PlainText "It was added"
handleRequest (ViewRq _)  = Right $ resp200 PlainText "Here are some details"
handleRequest ListRq      = Right $ resp200 PlainText "Here is a list"


-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app request cb = do
  madeRequest <- mkRequest request
  let result = madeRequest >>= handleRequest
  case result of
    (Right r) -> cb r
    (Left e)  -> cb $ mkErrorResponse e

runApp :: IO ()
runApp = run 3000 app
