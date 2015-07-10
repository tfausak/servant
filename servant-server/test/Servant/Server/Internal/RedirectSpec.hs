{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Server.Internal.RedirectSpec where

import           Data.Proxy
import           Servant.API
import           Test.Hspec
import           Test.Hspec.Wai

import           Servant.Server


spec :: Spec
spec = describe "module Servant.Server.Internal.Redirect" $ do
  redirectSpec

redirectSpec :: Spec
redirectSpec = describe "redirects" $ do

  with (return $ serve fullAPI server) $ do

    it "redirects to the mentioned link" $ do
      get "redirs" `shouldRespondWith` ""{ matchStatus = 301
                                         , matchHeaders = [ "Location" <:> "int" ]
                                         }


-- * API definiion

type First = "int" :> Get '[JSON] Int
type Second = Capture "x" Int :> Post '[JSON] Int

type MovedPermanentlyAPI = MovedPermanently Get First OriginalAPI
type FoundAPI = Capture "x" Int :> Found Post Second OriginalAPI

type OriginalAPI
  =   First
 :<|> Second

type FullAPI = OriginalAPI :<|> "redirs" :> (MovedPermanentlyAPI :<|> FoundAPI)

fullAPI :: Proxy FullAPI
fullAPI = Proxy

server :: Server FullAPI
server = (firstH :<|> secondH) :<|> movedPermanentlyH :<|> foundH

firstH :: Server First
firstH = return 5

secondH :: Server Second
secondH = return

movedPermanentlyH :: Server MovedPermanentlyAPI
movedPermanentlyH = return $ id

foundH :: Server FoundAPI
foundH x = return $ \fn -> fn x
