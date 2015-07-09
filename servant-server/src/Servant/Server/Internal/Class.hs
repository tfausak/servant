{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Servant.Server.Internal.Class where

import           Control.Monad.Trans.Either         (EitherT)
import           Data.Proxy                         (Proxy)

import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr

class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (Server layout)) -> Router

type Server layout = ServerT layout (EitherT ServantErr IO)
