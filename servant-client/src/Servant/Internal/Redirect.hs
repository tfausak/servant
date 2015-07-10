{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Internal.Redirect where

import           Data.Proxy             (Proxy (..))
import           Servant.API            (Found, MovedPermanently,
                                         PermanentRedirect, SeeOther,
                                         TemporaryRedirect)
import           Servant.Utils.Links    (IsMethod)

import           Servant.Common.BaseUrl
import           Servant.Common.Req
import           Servant.Internal.Class

instance HasClient (Redirect code method link api) where
  type Client (Redirect code method link api) = Client link

  clientWithRoute Proxy req baseurl = do
    _ <- performRequest pmet req [expect] baseurl
    clientWithRoute (Proxy :: Proxy link) req baseurl

  where pmet   = Proxy :: Proxy (IsMethod link)
        expect = natVal (Proxy :: Proxy code)

