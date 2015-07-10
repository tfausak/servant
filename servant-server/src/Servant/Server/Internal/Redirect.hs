{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Server.Internal.Redirect where

import           Control.Monad.Trans.Either                 (EitherT,
                                                             runEitherT)
import           Data.Proxy                                 (Proxy (..))
import           Data.String                                (fromString)
import           Network.HTTP.Types.Status                  (Status,
                                                             seeOther303)
import           Network.Wai                                (Application,
                                                             lazyRequestBody,
                                                             rawQueryString,
                                                             requestHeaders,
                                                             requestMethod,
                                                             responseLBS)
import           Servant.API
import           Servant.API.Redirect                       (Redirect(..))
import           Servant.Utils.Links

import           Servant.Server.Internal.Class              (HasServer (..))
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr


instance ( KnownMethod method
         ) => HasServer (Redirect code method api) where

  type ServerT (Redirect code method api) m
    = (Monad m) => m Int

  route _ getLink = LeafRouter undefined --route'
    {-where-}
      {-route' req respond-}
        {-| null (parsePathInfo req) && requestMethod req == methodOf pc =-}
            {-runAction getLink respond $ \link -> do-}
              {-succeedWith $ responseLBS (redirectStatusCode red)-}
                            {-[("Location", fromString ("/" ++ show link))] ""-}

        {-| null (parsePathInfo req) && requestMethod req /= methodOf pc =-}
            {-respond $ failWith WrongMethod-}
        {-| otherwise = respond $ failWith NotFound-}

        {-where pc   = Proxy :: Proxy c-}
              {-plnk = Proxy :: Proxy lnk-}
              {-papi = Proxy :: Proxy api-}
              {-red  = Redirect :: Redirect code method api-}
