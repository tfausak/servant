module Servant.Docs.Internal.RouteTree where

import qualified Data.Map               as Map

import Servant.Docs.Internal.Types

choice :: RouteTree a -> RouteTree a -> RouteTree a
choice (StaticPath t1) (StaticPath t2) = StaticPath (Map.unionWith choice t1 t2)
choice (CapturePath t1) (CapturePath t2) = CapturePath (Map.unionWith choice t1 t2)
choice rt1 rt2 = ChoicePath rt1 rt2
