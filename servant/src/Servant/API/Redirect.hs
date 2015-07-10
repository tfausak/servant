{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Redirect combinators.
--
--   +-------------------------------------------+-----------+-----------+
--   |                                           | Permanent | Temporary |
--   +-------------------------------------------+-----------+-----------+
--   | Method changes to GET                     | 301       | 302/303   |
--   |                                           |           |           |
--   | Method cannot change                      | 308       | 302/307   |
--   |                                           |           |           |
--   +-------------------------------------------+-----------+-----------+
--
-- That methods are changes are enforced with types.

-- "And laws were then most sure when, like the Draco's, they were writ in
-- types"

module Servant.API.Redirect where

import           Data.Proxy                (Proxy(..))
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           GHC.TypeLits              (KnownNat, natVal)
import           Network.HTTP.Types.Status (status301, status302, status303,
                                            status307, mkStatus, Status(..))


-- | Redirect with @301 - Moved Permanently@.
data MovedPermanently method link api
  deriving (Generic, Typeable)

-- | Redirect with @302 - Found@
-- If compatibility with HTTP/1.0 is not required, prefer 'SeeOther' or
-- 'TemporaryRedirect' over 302, since the treatment given to @Found@ by web
-- browsers is inconsistent.
data Found method link api
  deriving (Generic, Typeable)

-- | Redirect with @303 - See Other@
-- See <https://tools.ietf.org/html/rfc7231#section-6.4.4 RFC7231 6.4.4> for
-- more info.
-- The method of the redirect URL should be @GET@. This is enforced by the type
-- system.
data SeeOther method link api
  deriving (Generic, Typeable)

-- | Redirect with @307 - Temporary Redirect@
-- See <https://tools.ietf.org/html/rfc7231#section-6.4.7 RFC7231 6.4.7> for
-- more info. The method of the URL and the redirect URL should be the same.
-- This is enforced by the type system.
data TemporaryRedirect method link api
  deriving (Generic, Typeable)

-- | Redirect with @308 - Permanent Redirect@
-- See <https://tools.ietf.org/html/rfc7238 RFC7238> for more info. The method
-- of the URL and the redirect URL should be the same. This is enforced by the
-- type system.
data PermanentRedirect method link api
  deriving (Generic, Typeable)


data Redirect code method link api
  deriving (Generic, Typeable)


redirectStatusCode :: KnownNat code => Proxy code -> Status
redirectStatusCode p = case natVal p of
  301 -> status301
  302 -> status302
  303 -> status303
  307 -> status307
  308 -> mkStatus 308 "Permanent Redirect"
  _   -> error "impossible"
