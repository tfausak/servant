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
import           Control.Monad                 (join, void)
import           Data.Char                     (isSpace)
import           Data.Either                   (lefts, rights)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust, isJust, isNothing,
                                                mapMaybe)
import           Data.Monoid                   (Monoid (..), (<>))
import           Language.Haskell.TH           (TyLit (StrTyLit), Type (AppT, ConT, LitT, PromotedNilT, PromotedConsT),
                                                mkName)
import           Language.Haskell.TH.Quote     (QuasiQuoter (..))
import           Servant.API.Alternative       ((:<|>))
import           Servant.API.Capture           (Capture)
import           Servant.API.Delete            (Delete)
import           Servant.API.Get               (Get)
import           Servant.API.Header            (Header)
import           Servant.API.MatrixParam       (MatrixParam)
import           Servant.API.Post              (Post)
import           Servant.API.Put               (Put)
import           Servant.API.QueryParam        (QueryFlag, QueryParam,
                                                QueryParams)
import           Servant.API.ReqBody           (ReqBody)
import           Servant.API.Sub               ((:>))
import           Text.Parsec.Token
import           Text.ParserCombinators.Parsec (Parser, anyChar, char, endBy,
                                                eof, lookAhead, many, many1,
                                                manyTill, noneOf, notFollowedBy,
                                                endBy1,
                                                oneOf, optionMaybe, optional,
                                                parse, sepBy, sepBy1, skipMany,
                                                skipMany1, space, spaces,
                                                string, try, (<?>), (<|>))

import           Debug.Trace

sitemap :: QuasiQuoter
sitemap = QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = \x -> case parse (expP defMainParser) "" x of
        Left err -> error $ show err
        Right st -> case st of
            Left err -> error err
            Right s -> return s
    }



expP :: MainParser -> Parser ParseResult
expP mp = do
        spaces
        urls <- expGen `sepBy1` spaces
        spaces >> eof
        case lefts urls of
            [] -> return $ Right $ foldr1 optsUnion $ rights urls
            xs -> return $ Left $ mconcat xs
    where
          splitPath x = case span (/= ':') x of
                            (xs, []) -> (xs, Nothing)
                            (xs, ':':ys) -> (xs, Just ys)
          expGen = do
            methodName <- many1 $ noneOf "\t \n/"
            many $ oneOf "\t "
            segments <- optional (char '/')
                     >> many1 (noneOf "\t\n/ ") `sepBy` char '/'
            {-handler <- many $ noneOf "\n"-}
            char '\n'
            options <- optGen `sepBy` char '\n'
            return $ parseAll mp (methodName, splitPath <$> segments, undefined, options)

          optGen = do
            skipMany1 space
            optionName <- many1 $ noneOf ":"
            maybeOptParam <- optionMaybe $ try $ char ':' >> many (noneOf "\n")

            return (optionName, maybeOptParam)

parseAll :: MainParser -> (MethodName, [Path], Handler, [Option]) -> ParseResult
parseAll MainParser{..} (met, ps, h, opts) = do
    method <- methodP
    path <- sequence pathP
    option <- sequence optP
    return $ foldr1 pathUnion (option ++ path ++ [method])

  where
    mLookupWithErr :: String -> String -> Map.Map String v -> Either String v
    mLookupWithErr s k m = case Map.lookup k m of
                               Nothing -> Left $ s ++ k
                               Just y  -> Right y

    optP :: [ParseResult]
    optP = [ fromJust (j k) entry | (k, entry) <- opts, isJust $ j k ]
      where j k = Map.lookup k optParsers

    pathP :: [ParseResult]
    pathP = join . (\(k,entry) ->
        if isNothing entry
            then return $ Right $ LitT $ StrTyLit k
            else do
                (reqs, f) <- mLookupWithErr "Path type not found: " k pathParsers
                let reqs' = (\x -> case lookup x opts of
                        Nothing -> Left $ "Required (by path '" ++ k ++ "') option '" ++ x ++ "' not found."
                        Just y  -> Right y) <$> reqs
                case lefts reqs' of
                    [] -> return $ f (entry, rights reqs')
                    xs -> Left $ unlines xs) <$> ps

    methodP :: ParseResult
    methodP = join $ (\k -> do
        (reqs, f) <- mLookupWithErr "Method not found: " k methodParsers
        let reqs' = (\x -> case lookup x opts of
                Nothing -> Left $ "Required (by method '" ++ k ++ "') option '" ++ x ++ "' not found."
                Just y  -> Right y) <$> reqs
        case lefts reqs' of
            [] -> return $ f $ rights reqs'
            xs -> Left $ unlines xs) met





type Handler = String
type MethodName = String
type Option = (String, Maybe String)
type Path = (String, Maybe String)
type PathName = String
type ParseResult = Either String Type


data MainParser = MainParser
        { methodParsers :: Map.Map String ([String], [Maybe String] -> ParseResult)
        , pathParsers   :: Map.Map String ([String], (Maybe String, [Maybe String]) -> ParseResult)
        , optParsers    :: Map.Map String (Maybe String -> ParseResult)
        }

defMainParser :: MainParser
defMainParser = MainParser
    { methodParsers = Map.fromList
        [ ("GET", (["Response"], withResponse "GET" ''Get))
        , ("POST", (["Response"], withResponse "POST" ''Post))
        , ("PUT", (["Response"], withResponse "PUT" ''Put))
        , ("DELETE", ([], const . Right $ ConT ''Delete))
        ]
    , pathParsers = Map.fromList
        [ ("capture", ([], capture))
        , ("matrix", ([], matrix))
        ]
    , optParsers = Map.fromList
        [ ("Request", request)
        , ("Header", header)
        , ("QueryParams", queryParams) -- QueryParam, QueryParams, QueryFlag
        ]
    }
  where
    removeWspace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    mkCTList str = foldr (\x y -> AppT (AppT PromotedConsT x) y) PromotedNilT
                         (ConT . mkName . removeWspace <$> wordsBy (== ',') str)

    -- Methods
    withResponse m typ [Just x] = case span (/= '|') x of
        ([], xs) -> Left $ "Method " ++ m ++ " expects 'Response' option"
                       ++ " to have a type to the left of '|'."
        (xs, '|':ys) -> Right $ AppT (AppT (ConT typ) (mkCTList ys))
                                     (ConT . mkName . removeWspace $ xs)
    withResponse m _ _
        = Left $ "Method " ++ m ++ "could not parse 'Response' option."

    -- Paths
    capture (Just x,_) = case span (/= ':') x of
        ([], _) -> Left "'capture' path must have name to the right of '::'"
        (xs, ':':':':ys) -> Right $ AppT (AppT (ConT ''Capture) (LitT $ StrTyLit xs))
                             (ConT $ mkName ys)
        _ -> Left "'capture' path expects name and type separated by '::'"
    capture _ = Left "'capture' path expects an argument!"

    matrix (Just x,_) = case wordsBy (== '|') x of
        [path, mps] -> case lefts (mkTyp <$> wordsBy (== ',') mps) of
               [] -> Right $ AppT (AppT (ConT ''(:>)) (LitT $ StrTyLit path))
                                  (foldr1 pathUnion $ rights (mkTyp <$> wordsBy (== ',') mps))
               xs -> Left $ mconcat xs

          where mkTyp xs = case span (/= ':') xs of
                    ([], _) -> Left "Each part of matrix should be of form '<name>::<type>'"
                    (xs, ':':':':ys) -> Right $ AppT (AppT (ConT ''MatrixParam) (LitT $ StrTyLit xs))
                                                       (ConT $ mkName ys)
                    _ -> Left $ "'matrix' path could not parse: " ++ xs
        _ -> Left "'matrix' path expects format 'matrix:<path>|<name>::<type>'"

    -- Options
    request Nothing  = Left "'Request' option must have an argument"
    request (Just x) = case span (/= '|') x of
        ([], xs) -> Left $ "'Request' option must have a type to the left "
                        ++ "of '|'"
        (xs, '|':ys) -> Right $ AppT (AppT (ConT ''ReqBody) (mkCTList ys))
                                     (ConT . mkName . removeWspace $ xs)

    header Nothing   = Left "'Header' option must have an argument"
    header (Just x)  = case span (/= '|') x of
        ([], xs) -> Left $ "'Header' option must have a string to the left "
                        ++ "of '|'"
        (xs, '|':ys) -> Right $ AppT (AppT (ConT ''Header)
                                           (LitT . StrTyLit $ removeWspace xs))
                                     (ConT $ mkName xs)

    queryParams Nothing  = Left "'QueryParams' option must have an argument"
    queryParams (Just x) = case lefts parts of
        [] -> Right $ foldr1 pathUnion (toType <$> rights parts)
        xs -> Left $ mconcat xs
      where
        splitType s = case span (/= ':') s of
            (n, ':':':':t) -> Right ( LitT . StrTyLit $ removeWspace n
                                    , removeWspace t)
            _          -> Left $ "'QueryParams' option expects a list of "
                              ++ "comma-separated '<name> :: <type>' strings"
        parts = splitType <$> wordsBy (== ',') x
        toType (name, "Bool") = AppT (ConT ''QueryFlag) name
        toType (name, '[':xs) = AppT (AppT (ConT ''QueryParams) name)
                                     (ConT . mkName $ init xs)
        toType (name, xs)     = AppT (AppT (ConT ''QueryParam) name)
                                     (ConT $ mkName xs)




joinResults :: (Type -> Type -> Type) -> [Either String Type] -> Either String Type
joinResults unionF r = case lefts r of
                        [] -> Right $ foldr1 unionF $ rights r
                        xs -> Left $ unlines xs

pathUnion :: Type -> Type -> Type
pathUnion a = AppT (AppT (ConT ''(:>)) a)

optsUnion :: Type -> Type -> Type
optsUnion a = AppT (AppT (ConT ''(:<|>)) a)

wordsBy     :: (Char -> Bool) -> String -> [String]
wordsBy p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsBy p s''
                            where (w, s'') = break p s'

{-

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
