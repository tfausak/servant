{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Servant.QQSpec where

import           Test.Hspec  (Expectation, Spec, describe, it, pendingWith,
                              shouldBe)

import           Servant.API ((:<|>), (:>), Capture, Delete, Get, HTML, JSON,
                              MatrixParam, Post, Put, QueryFlag, QueryParam,
                              QueryParams, ReqBody, XML, sitemap)

--------------------------------------------------------------------------
-- Types for testing
--------------------------------------------------------------------------

-- Methods ---------------------------------------------------------------

type SimpleGet = [sitemap|
GET  hello
    Response: () | JSON
|]
type SimpleGet' = "hello" :> Get '[JSON] ()
type SimpleGet'' = "hello" :> Get '[JSON] Bool

type SimpleGet2 = [sitemap|
GET  hello
    Response: Bool | JSON
|]
type SimpleGet2' = "hello" :> Get '[JSON] Bool
type SimpleGet2'' = "hello" :> Get '[JSON] Int

type SimplePost = [sitemap|
POST  hello
    Response: () | JSON
|]
type SimplePost' = "hello" :> Post '[JSON] ()
type SimplePost'' = "hello" :> Post '[JSON] Bool

type SimplePost2 = [sitemap|
POST  hello
    Response: Bool | JSON
|]
type SimplePost2' = "hello" :> Post '[JSON] Bool
type SimplePost2'' = "hello" :> Post '[JSON] ()

type SimplePut = [sitemap|
PUT  hello
    Response: () | JSON
|]
type SimplePut' = "hello" :> Put '[JSON] ()
type SimplePut'' = "hello" :> Put '[JSON] Bool

type SimplePut2 = [sitemap|
PUT  hello
    Response: Bool | JSON
|]
type SimplePut2' = "hello" :> Put '[JSON] Bool
type SimplePut2'' = "hello" :> Put '[JSON] ()

-- Path ------------------------------------------------------------------
--
type Home = [sitemap|
GET /
    Response: Bool | JSON
|]

type Home' = Get '[JSON] Bool

type LeadingSlash = [sitemap|
GET /path
    Response: () | JSON
|]

type LeadingSlash' = [sitemap|
GET path
    Response: () | JSON
|]

-- Parameters ------------------------------------------------------------

type SimpleReqBody = [sitemap|
POST  hello
    Request: () | JSON
    Response: Bool | JSON
|]

type SimpleReqBody' = ReqBody '[JSON] () :> "hello" :> Post '[JSON] Bool
type SimpleReqBody'' = ReqBody '[JSON] Bool :> "hello" :> Post '[JSON] ()

type SimpleCapture = [sitemap|
POST  hello/capture:p::Int
    Response: Bool | JSON
|]
type SimpleCapture' = "hello" :> Capture "p" Int :> Post '[JSON] Bool
type SimpleCapture'' = "hello" :> Capture "r" Int :> Post '[JSON] Bool
type SimpleCapture''' = "hello" :> Capture "p" Bool :> Post '[JSON] Bool


type SimpleQueryParam = [sitemap|
POST  hello
    QueryParams: p :: Int
    Response: Bool | JSON
|]
type SimpleQueryParam' = QueryParam "p" Int :> "hello" :> Post '[JSON] Bool
type SimpleQueryParam'' = QueryParam "r" Int :> "hello" :> Post '[JSON] Bool
type SimpleQueryParam''' = QueryParam "p" Bool :> "hello" :> Post '[JSON] Bool


type SimpleMatrixParam = [sitemap|
POST  matrix:hello|p::Int
    Response: Bool | JSON
|]
type SimpleMatrixParam' = ("hello" :> MatrixParam "p" Int) :> Post '[JSON] Bool
type SimpleMatrixParam'' = ("hello" :> MatrixParam "r" Int) :> Post '[JSON] Bool
type SimpleMatrixParam''' = ("hello" :> MatrixParam "p" Bool) :> Post '[JSON] Bool

type ComplexMatrixParam = [sitemap|
POST  matrix:hello|p::Int,q::String/matrix:world|r::Int
    Response: Bool | JSON
|]
type ComplexMatrixParam' = ("hello" :> (MatrixParam "p" Int :> MatrixParam "q" String)) :> (("world" :> MatrixParam "r" Int) :> Post '[JSON] Bool)
type ComplexMatrixParam'' = ("hello" :> (MatrixParam "p" Int :> MatrixParam "q" String)) :> (("world" :> MatrixParam "s" Int) :> Post '[JSON] Bool)
type ComplexMatrixParam''' = ("hello" :> (MatrixParam "p" Int :> MatrixParam "q" String)) :> (("world" :> MatrixParam "r" Bool) :> Post '[JSON] Bool)

-- Combinations ----------------------------------------------------------

type TwoPaths = [sitemap|
POST hello
    Response: Bool | JSON
GET  hello
    Response: Bool | JSON
|]
type TwoPaths' = ("hello" :> Post '[JSON] Bool) :<|> ("hello" :> Get '[JSON] Bool)

-- Large examples --------------------------------------------------------

type LargeApi = [sitemap|

GET      an-int
    Response: Int | JSON, XML, HTML
    QueryParams: verbose :: Bool, age :: Int, names :: [String]

POST     post-int
    Request: Int | JSON
    Response: Bool | JSON

DELETE   capture:name::String

PUT      capture:name::String/age
    Response: Int | JSON

GET      a/longer/path
    Response: () | JSON
|]

x :: LargeApi
x = "s"

type LargeApi' = QueryFlag "verbose" :> QueryParam "age" Int :> QueryParams "names" String :> "an-int" :> Get '[JSON, XML, HTML] Int
            :<|> ReqBody '[JSON] Int :> "post-int" :> Post '[JSON] Bool
            :<|> Capture "name" String :> Delete
            :<|> Capture "name" String :> "age" :> Put '[JSON] Int
            :<|> "a" :> "longer" :> "path" :> Get '[JSON] ()
{-

[sitemap|
type: LargeApi2
server: serverForLargeApi2

GET      /an-int               getAnInt
    Response: Int | JSON, XML, HTML
    QueryParams: verbose :: Bool, age :: Int, names :: [String]

POST     /post-int             postInt
    Request:  Int  | JSON, PlainText
    Response: Bool | JSON, PlainText

DELETE   /capture:name         deleteByName
    name: String

PUT      /capture:name         putByName
    name: String
    Request: Int | JSON, PlainText
|]
-}

--------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "'sitemap' QuasiQuoter" $ do

        it "Handles simple GET types" $ do
            (u::SimpleGet)  ~= (u::SimpleGet'  ) ~> True
            (u::SimpleGet)  ~= (u::SimpleGet'' ) ~> False
            (u::SimpleGet2) ~= (u::SimpleGet2' ) ~> True
            (u::SimpleGet2) ~= (u::SimpleGet2'') ~> False

        it "Handles simple POST types" $ do
            (u::SimplePost)  ~= (u::SimplePost'  ) ~> True
            (u::SimplePost)  ~= (u::SimplePost'' ) ~> False
            (u::SimplePost2) ~= (u::SimplePost2' ) ~> True
            (u::SimplePost2) ~= (u::SimplePost2'') ~> False

        it "Handles simple PUT types" $ do
            (u::SimplePut)  ~= (u::SimplePut'  ) ~> True
            (u::SimplePut)  ~= (u::SimplePut'' ) ~> False
            (u::SimplePut2) ~= (u::SimplePut2' ) ~> True
            (u::SimplePut2) ~= (u::SimplePut2'') ~> False

        it "Handles top-level paths" $ do
            (u::Home) ~= (u::Home') ~> True

        it "Ignores leading slashes" $ do
            (u::LeadingSlash) ~= (u::LeadingSlash') ~> True

        it "Handles simple request body types" $ do
            (u::SimpleReqBody) ~= (u::SimpleReqBody' ) ~> True
            (u::SimpleReqBody) ~= (u::SimpleReqBody'') ~> False

        it "Handles simple captures" $ do
            (u::SimpleCapture) ~= (u::SimpleCapture' ) ~> True
            (u::SimpleCapture) ~= (u::SimpleCapture'') ~> False
            (u::SimpleCapture) ~= (u::SimpleCapture''') ~> False

        it "Handles simple querystring parameters" $ do
            (u::SimpleQueryParam) ~= (u::SimpleQueryParam' ) ~> True
            (u::SimpleQueryParam) ~= (u::SimpleQueryParam'') ~> False
            (u::SimpleQueryParam) ~= (u::SimpleQueryParam''') ~> False

        it "Handles simple matrix parameters" $ do
            (u::SimpleMatrixParam) ~= (u::SimpleMatrixParam' ) ~> True
            (u::SimpleMatrixParam) ~= (u::SimpleMatrixParam'') ~> False
            (u::SimpleMatrixParam) ~= (u::SimpleMatrixParam''') ~> False

        it "Handles more complex matrix parameters" $ do
            (u::ComplexMatrixParam) ~= (u::ComplexMatrixParam' ) ~> True
            (u::ComplexMatrixParam) ~= (u::ComplexMatrixParam'') ~> False
            (u::ComplexMatrixParam) ~= (u::ComplexMatrixParam''') ~> False

        it "Handles multiples paths" $ do
            (u::TwoPaths) ~= (u::TwoPaths') ~> True
            (u::LargeApi) ~= (u::LargeApi') ~> True

--------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------
data HTrue
data HFalse

-- Kiselyov's Type Equality predicate
class  TypeEq x y b | x y -> b where { areEq :: x -> y -> Bool }
instance               TypeEq x x HTrue where { areEq _ _ = True }
instance b ~ HFalse => TypeEq x y b where     { areEq _ _ = False}

infix 4 ~=
(~=) :: TypeEq x y b => x -> y -> Bool
(~=) = areEq

u :: a
u = undefined

infix 3 ~>
(~>) :: (Show a, Eq a) => a -> a -> Expectation
(~>) = shouldBe
