{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.Docs.Internal.Class where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Proxy
import           Network.HTTP.Media
import           Network.HTTP.Types
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
  hasDocs _ = LeafPath $ leafRender methodGet (Proxy :: Proxy ctyps)
                                              (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Post ctyps a) where
  hasDocs _ = LeafPath $ leafRender methodPost (Proxy :: Proxy ctyps)
                                               (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Put ctyps a) where
  hasDocs _ = LeafPath $ leafRender methodPut (Proxy :: Proxy ctyps)
                                              (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Delete ctyps a) where
  hasDocs _ = LeafPath $ leafRender methodDelete (Proxy :: Proxy ctyps)
                                                 (Proxy :: Proxy a)

instance (ToSample a, AllMimeRender ctyps a) => HasDocs (Patch ctyps a) where
  hasDocs _ = LeafPath $ leafRender methodPatch (Proxy :: Proxy ctyps)
                                                (Proxy :: Proxy a)

-- * Intermediate nodes

instance (HasDocs a, HasDocs b) => HasDocs (a :<|> b) where
  hasDocs _ = choice (hasDocs (Proxy :: Proxy a))
                     (hasDocs (Proxy :: Proxy b))


-- * Utils

leafRender :: (ToSample a, AllMimeRender ctyps a)
    => Method
    -> Proxy ctyps
    -> Proxy a
    -> Leaf
leafRender met pct pa = Leaf { _methodType = met
                             , _leafDesc   = P.Null
                             , _leafResps  = resps
                             }
  where
    resps = fmap ( \(ctyp, bs) -> ReqResp { _rrStatusCode   = status200
                                          , _rrHeaders      = [(hAccept, renderHeader ctyp)]
                                          , _rrBody         = BS.unpack bs
                                          } )
                 (allMimeRender pct (toSample pa))
