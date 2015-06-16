{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.Docs.Internal.Class where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Proxy
import           GHC.TypeLits
import qualified Network.HTTP.Types         as HTTP
import           Servant.API
import           Servant.API.ContentTypes
import qualified Text.Pandoc.Definition     as P

import           Servant.Docs.Internal.RouteTree
import           Servant.Docs.Internal.Sample
import           Servant.Docs.Internal.Types

class HasDocs a where
  hasDocs :: Proxy a -> RouteTree Leaf

-- * Leaves

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Get ctyps a) where
  hasDocs _ = LeafPath $ leafRender HTTP.methodGet
                                    (Proxy :: Proxy ctyps)
                                    (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Post ctyps a) where
  hasDocs _ = LeafPath $ leafRender HTTP.methodPost
                                    (Proxy :: Proxy ctyps)
                                    (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Put ctyps a) where
  hasDocs _ = LeafPath $ leafRender HTTP.methodPut
                                    (Proxy :: Proxy ctyps)
                                    (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Delete ctyps a) where
  hasDocs _ = LeafPath $ leafRender HTTP.methodDelete
                                    (Proxy :: Proxy ctyps)
                                    (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Patch ctyps a) where
  hasDocs _ = LeafPath $ leafRender HTTP.methodPatch
                                    (Proxy :: Proxy ctyps)
                                    (Proxy :: Proxy a)

-- * Intermediate nodes

instance (HasDocs a, HasDocs b) => HasDocs (a :<|> b) where
  hasDocs _ = choice (hasDocs (Proxy :: Proxy a))
                     (hasDocs (Proxy :: Proxy b))

instance (KnownSymbol hdr, ToText val, ToSample val, HasDocs a)
    => HasDocs (Header hdr val :> a) where
  hasDocs _ = fmap ( leafReq . reqHeaders <>~ hdr) sub
    where
      sub  = hasDocs (Proxy :: Proxy a)
      hdrp = Proxy :: Proxy hdr
      valp = Proxy :: Proxy val
      hdr  = [(symbolVal hdrp, show . toText $ toSample valp)]

-- * Utils

leafRender :: (ToSample a, AllMimeRender ctyps a)
    => HTTP.Method
    -> Proxy ctyps
    -> Proxy a
    -> Leaf
leafRender met pct pa = Leaf { _methodType = met
                             , _leafDesc   = P.Null
                             , _leafResps  = resps
                             , _leafReq    = Request [] ""
                             }
  where
    resps = fmap ( \(ctyp, bs) -> Response { _respStatusCode   = HTTP.status200
                                           , _respHeaders      = [("Accept", show ctyp)]
                                           , _respBody         = BS.unpack bs
                                           } )
                 (allMimeRender pct (toSample pa))
