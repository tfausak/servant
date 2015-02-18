{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Servant.QQ.Internal.Parser  where

import           Control.Applicative            ((<$>))
import           Control.Monad                  (join)
import           Data.Either                    (isLeft, lefts, rights)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust, isJust, isNothing)
import           Data.Monoid                    (Monoid (..))
import           Language.Haskell.TH            (Dec, Type (AppT, ConT))
import           Servant.API.Alternative        ((:<|>))
import           Servant.QQ.Internal.SubParsers
import           Text.ParserCombinators.Parsec  (Parser, char, eof, lookAhead,
                                                 many, many1, noneOf, oneOf, sepBy1,
                                                 optionMaybe, optional, sepBy,
                                                 space, spaces, try, (<?>))

data FullResult = ForDecs ParseResult ([String] -> IO [Dec])
                | ForType ParseResult

expP :: SubParsers -> Parser ParseResult
expP mp = do
        spaces
        urls <- expGen mp `sepBy` spaces
        spaces >> eof
        return $ bimapEither mconcat (foldr1 optsUnion) $ sequenceEithers urls

expGen :: SubParsers -> Parser ParseResult
expGen mp = do
   methodName <- many1 (noneOf "\t \n/") <?> "method name"
   many1 $ oneOf "\t "
   segments <- optional (char '/')
            >> ((many1 (noneOf "\t\n/ ") <?> "path segment" )`sepBy` char '/')
            <?> "path"
   {-handler <- many $ noneOf "\n"-}
   char '\n'
   options <- many $ do
       lookAhead $ oneOf "\t "
       r <- optGen <?> "option"
       char '\n'
       return r
   return $ parseAll mp (methodName, splitPath <$> segments, undefined, options)
  where
    splitPath x = case span (/= ':') x of
      (xs, [])       -> (xs, Nothing)
      (x:xs, ':':ys) -> (x:xs, Just ys)
      _              -> error "Empty option"

parsePreambles :: SubParsers -> Parser [Preamble]
parsePreambles SubParsers{..} = preamble `sepBy1` char '\n' <?> "preamble"
  where
    preamble = do
        pName <- many1 $ noneOf " \t\n:"
        char ':'
        pContent <- many $ noneOf "\n"
        case Map.lookup pName preambleParsers of
            Nothing -> error $ "Unknown preamble: " ++ pName
            Just f  -> return . f $ removeWspace pContent


optGen :: Parser (String, String)
optGen = do
    space >> spaces <?> "indentation"
    optionName <- many1 $ noneOf ":"
    maybeOptParam <- char ':' >> many (noneOf "\n")

    return (optionName, maybeOptParam)

parseAll :: SubParsers -> (MethodName, [Path], Handler, [Option]) -> ParseResult
parseAll SubParsers{..} (met, ps, h, opts) = do
    method <- methodP
    path <- sequence pathP
    option <- sequence optP
    return $ foldr1 pathUnion (option ++ path ++ [method])

  where
    mLookupWithErr :: String -> String -> Map.Map String v -> Either String v
    mLookupWithErr s k m = note (s ++ k) $ Map.lookup k m

    optP :: [ParseResult]
    optP = [ fromJust (j k) entry | (k, entry) <- opts, isJust $ j k ]
      where j k = Map.lookup k optParsers

    pathP :: [ParseResult]
    pathP = join . (\(k,entry) -> case entry of
        Nothing -> return $ Right $ strLit k
        Just e  -> do
                (reqs, f) <- mLookupWithErr "Path type not found: " k pathParsers
                let reqs' = [ note ("Required option not found: " ++ x)
                                   (lookup x opts)
                            | x <- reqs]
                case lefts reqs' of
                    [] -> return $ f (e, rights reqs')
                    xs -> Left $ unlines xs) <$> ps

    methodP :: ParseResult
    methodP = join $ (\k -> do
        (reqs, f) <- mLookupWithErr "Method not found: " k methodParsers
        let reqs' = [ note ("Required option not found: " ++ x)
                           (lookup x opts)
                    | x <- reqs]
        bimapEither unlines f $ sequenceEithers reqs' ) met


type Handler = String
type MethodName = String
type Option = (String, String)
type Path = (String, Maybe String)
type PathName = String


-- * Utils

optsUnion :: Type -> Type -> Type
optsUnion a = AppT (AppT (ConT ''(:<|>)) a)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- Left if any input is left, Right otherwise
sequenceEithers :: [Either e a] -> Either [e] [a]
sequenceEithers es = if any isLeft es then Left $ lefts es else Right $ rights es

bimapEither :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
bimapEither f g = either (Left . f) (Right . g)
