{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances   #-}
#endif
module Servant.Docs.Internal where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Lens
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.CaseInsensitive       as CI
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord                   (comparing)
import           Data.Proxy
import           Data.ByteString.Conversion (ToByteString, toByteString)
import           Data.String.Conversions
import           Data.Text                  (Text, pack, unpack)
import           GHC.Exts                   (Constraint)
import           GHC.Generics
import           GHC.TypeLits
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Utils.Links

import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Network.HTTP.Media         as M
import qualified Network.HTTP.Types         as HTTP


