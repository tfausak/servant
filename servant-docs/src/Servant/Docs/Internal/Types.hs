{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.Docs.Internal.Types where

import           Control.Lens
import qualified Text.Pandoc.Definition as P
import qualified Network.HTTP.Types     as HTTP
import qualified Data.Map               as Map

data RouteTree a
  = StaticPath  (Map.Map String (RouteTree a))
  | CapturePath (Map.Map CaptureRep (RouteTree a))
  | ChoicePath (RouteTree a) (RouteTree a)
  | LeafPath a
  deriving (Eq, Show, Functor)

data CaptureRep = CaptureRep
  { captureName   :: String
  , captureSample :: String
  } deriving (Eq, Ord, Show)

-- * Document AST


data Document = Document
  { _docName   :: P.Inline
  , _docDesc   :: P.Block
  , _endpoints :: [Endpoint]
  } deriving (Eq, Show)

data Endpoint = Endpoint
  { _endpointDesc :: P.Block
  , _leaves       :: [Leaf]
  } deriving (Eq, Show)

data Leaf = Leaf
  { _methodType :: HTTP.Method
  , _leafDesc   :: P.Block
  , _leafResps  :: [ReqResp]
  } deriving (Eq, Show)

data ReqResp = ReqResp
  { _rrStatusCode   :: HTTP.Status
  , _rrHeaders      :: [HTTP.Header]
  , _rrBody         :: String
  } deriving (Eq, Show)

makeLenses ''Document
makeLenses ''Endpoint
makeLenses ''Leaf
makeLenses ''ReqResp
