-- | QuasiQuoting utilities for API types.
--
-- 'sitemap' allows you to write your type in a very natural way:
--
-- @
-- [sitemap|
-- PUT        hello
--      Request: String | JSON, XML
--      Response: () | JSON
--
-- POST       hello/capture:age::Int
--      Request: String | JSON
--      Response: Int | JSON
--
-- GET        hello
--      QueryParams: name :: String
--      Response: Int | HTML
-- |]
-- @
--
-- Will generate:
--
-- @
--         "hello" :> ReqBody '[JSON, XML] String :> Put '[JSON] ()
--   :\<|> "hello" :> Capture "age" Int :> ReqBody String :> Post '[JSON] ()
--   :\<|> "hello" :> QueryParam "name" String :> Get '[HTML] Int
-- @
--
-- Note the @/@ before a @QueryParam@!
module Servant.QQ where

import           Language.Haskell.TH.Quote     (QuasiQuoter (..))
import Servant.QQ.Internal.Parser
import Servant.QQ.Internal.SubParsers
import           Text.ParserCombinators.Parsec (parse)

sitemap :: QuasiQuoter
sitemap = QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = \x -> case parse (expP defSubParser) "" x of
        Left err -> error $ show err
        Right st -> case st of
            Left err -> error err
            Right s -> return s
    }
