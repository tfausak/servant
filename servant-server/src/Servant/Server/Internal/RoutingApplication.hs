{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Servant.Server.Internal.RoutingApplication where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative         (Applicative, (<$>))
import           Data.Monoid                 (Monoid, mappend, mempty)
#endif
import           Control.Error.Util          (exceptT)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.String                 (fromString)
import           Network.HTTP.Types          hiding (Header, ResponseHeaders)
import           Network.Wai                 (Application, Request, Response,
                                              ResponseReceived,
                                              requestBody,
                                              responseLBS,
                                              strictRequestBody)
import           Servant.API                 ((:<|>) (..))
import           Servant.Server.Internal.ServantErr

type RoutingApplication = Request -> (Response -> IO ResponseReceived)
                       -> ServantT IO ResponseReceived

runAction :: ServantT IO a -> (a -> Response) -> RoutingApplication
runAction action s _ respond = action >>= liftIO . respond . s

liftApplication :: Application -> RoutingApplication
liftApplication app req respond = liftIO $ app req respond

feedTo :: Functor f => f (a -> b) -> a -> f b
feedTo f x = ($ x) <$> f

extractL :: Functor f => f (a :<|> b) -> f a
extractL = fmap go
  where go (a :<|> _) = a

extractR :: Functor f => f (a :<|> b) -> f b
extractR = fmap go
  where go (_ :<|> b) = b

