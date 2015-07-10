{-# LANGUAGE PolyKinds #-}
module Servant.Common.Methods where

import           Network.HTTP.Types.Method
import           Servant.API.Get
import           Servant.API.Delete
import           Servant.API.Post
import           Servant.API.Put
import           Servant.API.Patch

class KnownMethod c where
  methodOf :: proxy c -> Method

instance KnownMethod Delete where
  methodOf _ = methodDelete

instance KnownMethod Get where
  methodOf _ = methodGet

instance KnownMethod Patch where
  methodOf _ = methodPatch

instance KnownMethod Post where
  methodOf _ = methodPost

instance KnownMethod Put where
  methodOf _ = methodPut
