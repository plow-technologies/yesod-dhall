{-|
Helpers to parse and export Dhall expressions via Yesod.
|-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Yesod.Dhall where

-- Yesod/WAI
import           Yesod
import           Network.Wai
import qualified Network.HTTP.Types as HTTP

-- Making responses
import Control.Exception (throwIO)
import Yesod.Core.Types (HandlerContents(HCContent))


-- Dhall
import Dhall

-- Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- Pretty printing
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Dhall.Pretty

-- ByteString
import qualified Data.ByteString.Lazy as ByteString (toStrict)

-- | A value to encode as Dhall and send as response body, or an error message
data DhallResponse a
  = DhallError Text  -- ^ A plain-text error message
  | DhallResponse a  -- ^ A value to be encoded in Dhall and sent as the response body
  deriving (Eq, Ord, Show, Functor, Generic)

instance Inject a => ToContent (DhallResponse a) where
  toContent (DhallError text) = toContent text
  toContent (DhallResponse a) =
      toContent
    $ renderStrict
    $ layoutCompact
    $ prettyExpr
    $ embed inject a

instance Inject a => ToTypedContent (DhallResponse a) where
  toTypedContent response = TypedContent typePlain $ toContent response

-- | Parse the body of the request as a Dhall expression.
--
-- Note that this will make HTTP requests in order to retrieve files referenced
-- by URL in the uploaded file.
requireDhallBody
  :: MonadHandler m
  => Interpret a
  => m a
requireDhallBody =
      waiRequest -- Get the HTTP request from WAI
  >>= (fmap Text.decodeUtf8 . fmap ByteString.toStrict . liftIO . strictRequestBody)
  >>= (liftIO . input auto)

-- | Send a Dhall response along with an
sendDhallResponseWithStatus
  :: MonadHandler m
  => Inject a
  => HTTP.Status
  -> DhallResponse a
  -> m (DhallResponse a)
sendDhallResponseWithStatus s =
  liftIO . throwIO . HCContent s . toTypedContent

sendDhallSuccessResponse
  :: MonadHandler m
  => Inject a
  => a
  -> m (DhallResponse a)
sendDhallSuccessResponse = sendDhallResponseWithStatus HTTP.status200 . DhallResponse

