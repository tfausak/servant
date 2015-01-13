{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Utils.LinksSpec where

import Test.Hspec ( Spec, it, describe )

import Servant.API
    ( type (:<|>), ReqBody, QueryParam, MatrixParam, MatrixParams
    , MatrixFlag, Get, Post, Capture, type (:>) , HTML , JSON, XML )
import Servant.QQSpec ( (~>) )
import Servant.Utils.Links ( IsElem, IsLink )


type TestApi =
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Get '[JSON] Bool
  :<|> "greet" :> ReqBody '[JSON] 'True :> Post '[JSON, XML] Bool
  :<|> "parent" :> MatrixParams "name" String :> "child" :> MatrixParam "gender" String :> Get '[JSON] String

type TestLink = "hello" :> "hi" :> Get '[JSON] Bool
type TestLink2 = "greet" :> Post '[XML] Bool
type TestLink3 = "parent" :> "child" :> Get '[JSON] String

type BadTestLink = "hallo" :> "hi" :> Get '[JSON] Bool
type BadTestLink2 = "greet" :> Get '[XML] Bool
type BadTestLink3 = "parent" :> "child" :> MatrixFlag "male" :> Get '[JSON] String

type BadTestLink' = "hello" :> "hi" :> Get '[HTML] Bool
type BadTestLink'2 = "greet" :> Get '[HTML] Bool

type NotALink = "hello" :> Capture "x" Bool :> Get '[JSON] Bool
type NotALink2 = "hello" :> ReqBody '[JSON] 'True :> Get '[JSON] Bool

data Proxy x = Proxy
class ReflectT (x::Bool) where { reflected :: Proxy x -> Bool }
instance ReflectT 'True where { reflected _ = True }
instance ReflectT 'False where { reflected _ = False }

spec :: Spec
spec = describe "Servant.API.Elem" $ do
    isElem
    isLink

isElem :: Spec
isElem = describe "IsElem" $ do
    it "is True when the first argument is an url within the second" $ do
       reflected (Proxy::Proxy (IsElem TestLink TestApi)) ~> True
       reflected (Proxy::Proxy (IsElem TestLink2 TestApi)) ~> True
       reflected (Proxy::Proxy (IsElem TestLink3 TestApi)) ~> True
    it "is False when the first argument is not an url within the second" $ do
       reflected (Proxy::Proxy (IsElem BadTestLink TestApi)) ~> False
       reflected (Proxy::Proxy (IsElem BadTestLink2 TestApi)) ~> False
       reflected (Proxy::Proxy (IsElem BadTestLink3 TestApi)) ~> False

isLink :: Spec
isLink = describe "IsLink" $ do
    it "is True when all Subs are paths and the last is a method" $ do
        reflected (Proxy::Proxy (IsLink TestLink)) ~> True
        reflected (Proxy::Proxy (IsLink TestLink2)) ~> True
        reflected (Proxy::Proxy (IsLink TestLink3)) ~> True
    it "is False of anything with captures" $ do
        reflected (Proxy::Proxy (IsLink NotALink)) ~> False
        reflected (Proxy::Proxy (IsLink NotALink2)) ~> False
