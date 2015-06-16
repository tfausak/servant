{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
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

type HeaderS = (String, String)

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
  , _leafResps  :: [Response]
  , _leafReq    :: Request
  } deriving (Eq, Show)

data Response = Response
  { _respStatusCode   :: HTTP.Status
  , _respHeaders      :: [HeaderS]
  , _respBody         :: String
  } deriving (Eq, Show)

data Request = Request
  { _reqHeaders :: [HeaderS]
  , _reqBody    :: String
  } deriving (Eq, Show)

makeLenses ''Document
makeLenses ''Endpoint
makeLenses ''Leaf
makeLenses ''Response
makeLenses ''Request
