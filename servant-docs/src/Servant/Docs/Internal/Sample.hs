module Servant.Docs.Internal.Sample where

import Data.Proxy

class ToSample a where
  toSample :: Proxy a -> a
