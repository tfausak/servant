{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Servant.Server.Internal.Redirect where

import           Data.Proxy                                 (Proxy (..))
import           Data.String                                (fromString)
import           GHC.TypeLits                               (KnownNat)
import           Network.Wai                                (requestMethod,
                                                             responseLBS)
import           Servant.API
import           Servant.API.Redirect
import           Servant.Utils.Links

import           Servant.Server.Internal.Class              (HasServer (..))
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication


-- We write a single instance for @Redirect@ and delegate all other instances
-- to it.
instance (KnownNat code, KnownMethod (IsMethod link), HasLink link, IsElem link api
        ) => HasServer (Redirect code method link api) where

  type ServerT (Redirect code method link api) m
    = m (MkLink link -> URI)

  route _ getLink = LeafRouter route'
    where
      route' req respond
        | null (parsePathInfo req) && requestMethod req == methodOf pmet =
            runAction getLink respond $ \cont -> do
              let link = cont (safeLink papi plink)
              succeedWith $ responseLBS (redirectStatusCode red)
                            [("Location", fromString (show link))] ""
        | null (parsePathInfo req) && requestMethod req /= methodOf pmet =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

        where plink = Proxy :: Proxy link
              pmet  = Proxy :: Proxy (IsMethod link)
              papi  = Proxy :: Proxy api
              red   = Proxy :: Proxy code

-- | Redirects with @Moved Permanently - 301@
--
-- Example:
--
-- >>> type OriginalAPI = Capture "isbn" Text :> Get '[JSON] Book
-- >>> type Redirect = Capture "isbn" Text :> MovedPermanently Get OriginalAPI OriginalAPI
-- >>> type FullAPI = OriginalAPI :<|> Redirect
-- >>> lookupBook = undefined :: Text -> Book
-- >>> server = (return . lookupBook) :<|> (\x -> return $ \fn -> fn x)
                                             --
-- $cycle
{-instance HasServer (Redirect 301 method link api)-}
  {-=> HasServer (MovedPermanently method link api) where-}
  {-type ServerT (MovedPermanently method link api) m-}
    {-= ServerT (Redirect 301 method link api) m-}
  {-route _ = route (Proxy :: Proxy (Redirect 301 method link api))-}

{-instance HasServer (Redirect 302 method link api)-}
  {-=> HasServer (Found method link api) where-}
  {-type ServerT (Found method link api) m-}
    {-= ServerT (Redirect 302 method link api) m-}
  {-route _ = route (Proxy :: Proxy (Redirect 302 method link api))-}

{-instance HasServer (Redirect 303 method link api)-}
  {-=> HasServer (SeeOther method link api) where-}
  {-type ServerT (SeeOther method link api) m-}
    {-= ServerT (Redirect 303 method link api) m-}
  {-route _ = route (Proxy :: Proxy (Redirect 303 method link api))-}

{-instance HasServer (Redirect 307 method link api)-}
  {-=> HasServer (TemporaryRedirect method link api) where-}
  {-type ServerT (TemporaryRedirect method link api) m-}
    {-= ServerT (Redirect 307 method link api) m-}
  {-route _ = route (Proxy :: Proxy (Redirect 307 method link api))-}

{-instance HasServer (Redirect 308 method link api)-}
  {-=> HasServer (PermanentRedirect method link api) where-}
  {-type ServerT (PermanentRedirect method link api) m-}
    {-= ServerT (Redirect 308 method link api) m-}
  {-route _ = route (Proxy :: Proxy (Redirect 308 method link api))-}

-- $cycle
-- Note that types are not allowed to form cycles, so you cannot refer to the
-- full API you are defining. Instead, you should separate keep a separate type
-- synonym for the API without the redirect, and refer to that as the @api@
-- argument.
