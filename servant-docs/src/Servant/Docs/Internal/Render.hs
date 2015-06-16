module Servant.Docs.Internal.Render where

import Network.HTTP.Types ()
import qualified Text.Pandoc.Definition as P

import Servant.Docs.Internal.Types

defaultDoc :: Document
defaultDoc = Document
  { _docName   = P.RawBlock "Server API Documentation"
  , _docDesc   = P.Null
  , _endpoints = []
  }

renderResponse :: Response -> [P.Block]
renderResponse resp
  =  P.Header 1 (P.Str $ "Response" +++ statusCode +++ parens ctype)
  <> P.CodeBlock _ (P.Str $ respBody resp)
  where
    statusCode = respStatusCode resp
    ctype      = lookup "Accept" $ respHeaders resp


renderLeaf :: Leaf -> [P.Block]
renderLeaf leaf
  =  P.Header 3 (P.Str . show $ respHeaders leaf)
  <> leafDesc leaf
  <> P.BulletList (fmap renderResponse $ leafResps leaf)

{-routeTreeToDoc :: RouteTree Leaf -> Document-}
{-routeTreeToDoc LeafPath a = endpoints <>~  $ defaultDoc-}


-- * Utils
parens :: String -> String
parens s = "(" ++ s ++ ")"

(+++) :: String -> String
a +++ b = a ++ " " ++ b
