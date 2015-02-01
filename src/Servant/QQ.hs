{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | QuasiQuoting utilities for API types.
--
-- 'sitemap' allows you to write your type in a very natural way:
--
-- @
-- [sitemap|
-- PUT        hello                 String -> ()
-- POST       hello/p:Int           String -> ()
-- GET        hello/?name:String    Int
-- |]
-- @
--
-- Will generate:
--
-- @
--        "hello" :> ReqBody String :> Put ()
--   :\<|> "hello" :> Capture "p" Int :> ReqBody String :> Post ()
--   :\<|> "hello" :> QueryParam "name" String :> Get Int
-- @
--
-- Note the @/@ before a @QueryParam@!
module Servant.QQ  where

import           Control.Applicative           ((<$>))
import           Control.Monad                 (void)
import           Data.Either                   (lefts, rights)
import qualified Data.Map                      as Map
import           Data.Maybe                    (mapMaybe)
import           Data.Monoid                   (Monoid (..), (<>))
import           Language.Haskell.TH           (TyLit (StrTyLit),
                                                Type (AppT, ConT, LitT), mkName)
import           Language.Haskell.TH.Quote     (QuasiQuoter (..))
import           Servant.API.Alternative       ((:<|>))
import           Servant.API.Capture           (Capture)
import           Servant.API.Delete            (Delete)
import           Servant.API.Get               (Get)
import           Servant.API.MatrixParam       (MatrixParam)
import           Servant.API.Post              (Post)
import           Servant.API.Put               (Put)
import           Servant.API.QueryParam        (QueryParam)
import           Servant.API.ReqBody           (ReqBody)
import           Servant.API.Sub               ((:>))
import           Text.Parsec.Token
import           Text.ParserCombinators.Parsec (Parser, anyChar, char, endBy,
                                                lookAhead, many, many1,
                                                manyTill, noneOf, notFollowedBy,
                                                optionMaybe, optional, parse,
                                                sepBy, sepBy1, skipMany,
                                                skipMany1, space, spaces,
                                                string, try, (<?>), (<|>))


sitemap = undefined
qqLanguageDef :: LanguageDef st
qqLanguageDef =
        LanguageDef { commentStart = "{-"
                    , commentEnd   = "}-"
                    , commentLine  = "--"
                    , nestedComments = False
                    , identStart = noneOf " \t\n:/-"
                    , identLetter = noneOf " \t\n:/-"
                    , opStart = noneOf " \t\n:/-"
                    , opLetter = noneOf " \t\n:/-"
                    , reservedNames = []
                    , reservedOpNames = ["/:"]
                    , caseSensitive = True
                    }


expP :: Parser (MethodName, [Path], Handler, [Option])
expP = expGen $ makeTokenParser qqLanguageDef
    where
          splitPath x = case span (/= ':') x of
                            (xs, []) -> (xs, Nothing)
                            (xs, ':':ys) -> (xs, Just ys)
          expGen t@TokenParser{..} = do
            methodName <- identifier
            segments <- identifier `sepBy1` symbol "/"
            handler <- many $ noneOf "\n"
            string "\n"
            options <- optGen t `sepBy` string "\n"
            return (methodName, splitPath <$> segments, handler, options)

          optGen TokenParser{..} = do
            skipMany1 space
            optionName <- many1 $ noneOf ":"
            maybeOptParam <- optionMaybe $ try $ char ':' >> many (noneOf "\n")
            return (optionName, maybeOptParam)

parseAll :: MainParser -> (MethodName, [Path], Handler, [Option]) -> ParseResult
parseAll MainParser{..} (met, ps, h, opts) = case Map.lookup met methodParsers of
    Nothing -> Left $ "Unknown method: " ++ met
    Just topMet -> undefined
      where
        inOpts :: String -> Maybe Option
        inOpts = (\x -> case lookup x opts of
                      Nothing -> Nothing
                      Just p -> Just (x, p))

        metReq :: [Maybe Option]
        metReq = inOpts <$> topOpts topMet

        metResult :: ParseResult
        metResult = parseTopOpts topMet metReq

        lk :: Path -> ParseResult
        lk path = case Map.lookup (fst path) pathParsers of
                     Nothing -> Left $ "Unknown path arg: " ++ fst path
                     Just x -> parseTopOpts x $ undefined

        pathResults :: [ParseResult]
        pathResults = lk <$> ps


type Handler = String
type MethodName = String
type Option = (String, Maybe String)
type Path = (String, Maybe String)
type PathName = String
type ParseResult = Either String Type

data TopParser = TopParser
                  { topOpts      :: [String]
                  -- ^ List of opts to lookup
                  , parseTopOpts :: [Maybe (String, Maybe String)] -> ParseResult
                  -- ^ Function from those opts to a 'ParserResult'. The
                  -- function is guaranteed to be applied only to a list of
                  -- the same length as 'metOpts'.
                  }


data MainParser = MainParser
                { methodParsers :: Map.Map MethodName TopParser
                , pathParsers   :: Map.Map PathName FinalP
                , optParsers    :: Map.Map String FinalP
                }





joinResults :: (Type -> Type -> Type) -> [Either String Type] -> Either String Type
joinResults unionF r = case lefts r of
                        [] -> Right $ foldr1 unionF $ rights r
                        xs -> Left $ unlines xs

pathUnion :: Type -> Type -> Type
pathUnion a = AppT (AppT (ConT ''(:>)) a)

optsUnion :: Type -> Type -> Type
optsUnion a = AppT (AppT (ConT ''(:<|>)) a)


{-
addMethodParser :: String -> (String -> String -> Either String Type) -> SitemapParser -> SitemapParser
addPathParser, addOptsParser :: String -> (String -> Either String Type) -> SitemapParser -> SitemapParser
addMethodParser k a x@SitemapParser{..} = x{ methodParsers = Map.insert k a methodParsers }
addOptsParser k a x@SitemapParser{..} = x{ optsParsers = Map.insert k a optsParsers}
addPathParser k a x@SitemapParser{..} = x{ pathParsers = Map.insert k a pathParsers}

-- | Finally-tagless encoding for our DSL.
-- Keeping 'repr'' and 'repr' distinct when writing functions with an
-- @ExpSYM@ context ensures certain invariants (for instance, that there is
-- only one of 'get', 'post', 'put', and 'delete' in a value), but
-- sometimes requires a little more work.
class ExpSYM repr' repr | repr -> repr', repr' -> repr where
    lit         :: String -> repr' -> repr
    capture     :: String -> String -> repr -> repr
    reqBody     :: String -> repr -> repr
    queryParam  :: String -> String -> repr -> repr
    matrixParam :: String -> String -> repr -> repr
    conj        :: repr' -> repr -> repr
    get         :: String -> repr
    post        :: String -> repr
    put         :: String -> repr
    delete      :: String -> repr


infixr 6 >:

(>:) :: Type -> Type -> Type
(>:) = conj


instance ExpSYM Type Type where
    lit name r         = LitT (StrTyLit name) >: r
    capture name typ r = AppT (AppT (ConT ''Capture) (LitT (StrTyLit name)))
                               (ConT $ mkName typ) >: r
    reqBody typ r      = AppT (ConT ''ReqBody) (ConT $ mkName typ) >: r
    queryParam name typ r  = AppT (AppT (ConT ''QueryParam) (LitT (StrTyLit name)))
                               (ConT $ mkName typ) >: r
    matrixParam name typ r = AppT (AppT (ConT ''MatrixParam) (LitT (StrTyLit name)))
                               (ConT $ mkName typ) >: r
    conj x             = AppT (AppT (ConT ''(:>)) x)
    get  typ           = AppT (ConT ''Get) (ConT $ mkName typ)
    post typ           = AppT (ConT ''Post) (ConT $ mkName typ)
    put typ            = AppT (ConT ''Put) (ConT $ mkName typ)
    delete "()"        = ConT ''Delete
    delete _           = error "Delete does not return a request body"

parseMethod :: ExpSYM repr' repr => Parser (String -> repr)
parseMethod = try (string "GET"    >> return get)
          <|> try (string "POST"   >> return post)
          <|> try (string "PUT"    >> return put)
          <|> try (string "DELETE" >> return delete)

parseUrlSegment :: ExpSYM repr repr => Parser (repr -> repr)
parseUrlSegment = try parseCapture
              <|> try parseQueryParam
              <|> try parseLit
  where
      parseCapture = do
         cname <- many (noneOf " ?/:;")
         char ':'
         ctyp  <- many (noneOf " ?/:;")
         mx <- many parseMatrixParam
         return $ capture cname ctyp . foldr (.) id mx
      parseQueryParam = do
         char '?'
         cname <- many (noneOf " ?/:;")
         char ':'
         ctyp  <- many (noneOf " ?/:;")
         return $ queryParam cname ctyp
      parseLit = do
         lt <- many (noneOf " ?/:;")
         mx <- many parseMatrixParam
         return $ lit lt . foldr (.) id mx
      parseMatrixParam = do
         char ';'
         cname <- many (noneOf " ?/:;")
         char ':'
         ctyp  <- many (noneOf " ?/:;")
         return $ matrixParam cname ctyp

parseUrl :: ExpSYM repr repr => Parser (repr -> repr)
parseUrl = do
    optional $ char '/'
    url <- parseUrlSegment `sepBy1` char '/'
    return $ foldr1 (.) url

data Typ = Val String
         | ReqArgVal String String

parseTyp :: Parser Typ
parseTyp = do
    f <- many (noneOf "-{\n\r")
    spaces
    s <- optionMaybe (try parseRet)
    try $ optional inlineComment
    try $ optional blockComment
    case s of
        Nothing -> return $ Val (stripTr f)
        Just s' -> return $ ReqArgVal (stripTr f) (stripTr s')
  where
    parseRet :: Parser String
    parseRet = do
        string "->"
        spaces
        many (noneOf "-{\n\r")
    stripTr = reverse . dropWhile (== ' ') . reverse


parseEntry :: ExpSYM repr repr => Parser repr
parseEntry = do
    met <- parseMethod
    spaces
    url <- parseUrl
    spaces
    typ <- parseTyp
    case typ of
        Val s -> return $ url (met s)
        ReqArgVal i o -> return $ url $ reqBody i (met o)

blockComment :: Parser ()
blockComment = do
    string "{-"
    manyTill anyChar (try $ string "-}")
    return ()

inlineComment :: Parser ()
inlineComment = do
    string "--"
    manyTill anyChar (try $ lookAhead eol)
    return ()

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

eols :: Parser ()
eols = skipMany $ void eol <|> blockComment <|> inlineComment

parseAll :: Parser Type
parseAll = do
    eols
    entries <- parseEntry `endBy` eols
    return $ foldr1 union entries
  where union :: Type -> Type -> Type
        union a = AppT (AppT (ConT ''(:<|>)) a)

-- | The sitemap QuasiQuoter.
--
--     * @.../<var>:<type>/...@ becomes a capture
--     * @.../?<var>:<type>@ becomes a query parameter
--     * @<method>   ...  <typ>@ becomes a method returning @<typ>@
--     * @<method>   ...  <typ1> -> <typ2>@ becomes a method with request
--       body of @<typ1>@ and returning @<typ2>@
--
-- Comments are allowed, and have the standard Haskell format
--
--     * @--@ for inline
--     * @{- ... -}@ for block
--
sitemap :: QuasiQuoter
-}
