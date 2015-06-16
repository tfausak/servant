{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Servant.DocsSpec where

import           Data.Aeson
import           Data.String.Conversions (cs)
import           GHC.Generics
import           Test.Hspec

import           Servant.API
import           Servant.Docs.Internal.Sample

spec :: Spec
spec = describe "Servant.Docs" $ do
  hasDocsSpec

hasDocsSpec :: Spec
hasDocsSpec = describe "hasDocs" $ do
  it "generates a RouteTree with all endpoints" $ pending

{-leafRenderSpec :: Spec-}
{-leafRenderSpec = describe "leafRender" $ do-}
  {-it "generates a leaf with the correct information" $ do-}
    {-let resp = ReqResp { _rrStatusCode = status-}
    {-leafRender methodGet (Proxy :: Proxy '[JSON]) (Proxy :: Proxy Datatype1) -}
      {-`shouldBe` Leaf { _methodType = methodGet-}
                      {-, _leafDesc   = P.Null-}
                      {-, _leafResps  = [("Accept", "-}
                      {-}-}

-- * APIs

data Datatype1 = Datatype1 { dt1field1 :: String
                           , dt1field2 :: Int
                           } deriving (Eq, Show, Generic)

instance ToJSON Datatype1

instance ToSample Datatype1 where
  toSample _ = Datatype1 "field 1" 13

instance ToSample String where
  toSample _ = "a string"

instance ToSample Int where
  toSample _ = 17

instance MimeRender PlainText Int where
  mimeRender _ = cs . show


type TestApi1 = Get '[JSON, PlainText] Int
           :<|> ReqBody '[JSON] String :> Post '[JSON] Datatype1

