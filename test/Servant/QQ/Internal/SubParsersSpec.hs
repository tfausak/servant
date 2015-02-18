{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Servant.QQ.Internal.SubParsersSpec where

import           Data.List                      (isPrefixOf)
import           Language.Haskell.TH            (Type, runQ)
import           Test.Hspec                     (Expectation, Spec, describe,
                                                 it, pendingWith, shouldBe,
                                                 shouldContain)

import           Servant.API                    (Capture, Delete, Get, Header,
                                                 Post, Put, ReqBody)
import           Servant.API.ContentTypes       (HTML, JSON, XML)
import           Servant.QQ.Internal.SubParsers (MethodSubParser (..),
                                                 OptionSubParser (..),
                                                 PathSubParser (..), capture,
                                                 deleteMethodParser,
                                                 getMethodParser, header,
                                                 postMethodParser,
                                                 putMethodParser, queryParams,
                                                 request)

spec :: Spec
spec = describe "Servant.QQ.Internal.Subparsers" $ do
    deleteMethodParserSpec
    getMethodParserSpec
    postMethodParserSpec
    putMethodParserSpec
    captureSpec
    headerSpec
    matrixSpec
    queryParamsSpec
    requestSpec

-- Methods

deleteMethodParserSpec :: Spec
deleteMethodParserSpec = describe "deleteMethodParser" $
    it "always returns the 'Delete' type" $ do
        r <- runQ [t| Delete |]
        methodSubParser deleteMethodParser [] `shouldBe` Right r

getMethodParserSpec :: Spec
getMethodParserSpec = describe "getMethodParser" $ do

    it "requires a 'Response' option" $
        methodExpects getMethodParser `shouldContain` ["Response"]

    it "returns a 'Get' type applied to each comma-separated type in 'Response'" $ do
        r <- runQ [t| Get '[XML, HTML] Bool |]
        methodSubParser getMethodParser ["Bool | XML, HTML"]
            `qualifiedTypeShouldBe` r

postMethodParserSpec :: Spec
postMethodParserSpec = describe "postMethodParser" $ do

    it "requires a 'Response' option" $
        methodExpects postMethodParser `shouldContain` ["Response"]

    it "returns a 'Post' type applied to each comma-separated type in 'Response'" $ do
        r <- runQ [t| Post '[XML, HTML] Bool |]
        methodSubParser postMethodParser ["Bool | XML, HTML"]
            `qualifiedTypeShouldBe` r

putMethodParserSpec :: Spec
putMethodParserSpec = describe "putMethodParser" $ do

    it "requires a 'Response' option" $
        methodExpects putMethodParser `shouldContain` ["Response"]

    it "returns a 'Put' type applied to each comma-separated type in 'Response'" $ do
        r <- runQ [t| Put '[XML, HTML, JSON] Bool |]
        methodSubParser putMethodParser ["Bool | XML, HTML, JSON"]
            `qualifiedTypeShouldBe` r

-- Paths

captureSpec :: Spec
captureSpec = describe "capture" $

    it "parses captures into name and type" $ do
        r <- runQ [t| Capture "name" Bool |]
        pathSubParser capture "name::Bool" [] `qualifiedTypeShouldBe` r

matrixSpec :: Spec
matrixSpec = describe "matrix" $

    it "parses matrix into path, names and types" $
        pendingWith "needs type reordering logic"

-- Options

headerSpec :: Spec
headerSpec = describe "header" $

    it "parses header into name and type" $ do
        r <- runQ [t| Header "header" Bool |]
        optionSubParser header "header | Bool" `qualifiedTypeShouldBe` r

queryParamsSpec :: Spec
queryParamsSpec = describe "queryParams" $

    it "parses query params into appropriate types" $
        pendingWith "needs type reordering logic"

requestSpec :: Spec
requestSpec = describe "request" $

    it "parses request into ReqBody" $ do
        r <- runQ [t| ReqBody '[JSON, XML] Bool |]
        optionSubParser request "Bool | JSON, XML" `qualifiedTypeShouldBe` r

-- Utils

-- This is a hack to test that a dynamically-qualified type is the same as
-- a fully-qualified one in the current context.
qualifiedTypeShouldBe :: (Show a) => Either a Type -> Type -> Expectation
qualifiedTypeShouldBe a b = rem (show a) `shouldBe` rem (show (Right b :: Either String Type))
  where
    rem = remove "Servant.API.Sub." . remove "GHC.Types." . remove "Servant.API.ContentTypes."

    remove :: String -> String -> String
    remove r str@(s:ss) = if r `isPrefixOf` str
                              then remove r $ drop (length r) str
                              else s : remove r ss
    remove _ [] = []
