{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- Subparsers that handle particular parts of the Servant QQ.
--
-- All code will be run exclusively at compile-time, so 'errors' are used
-- liberally.
module Servant.QQ.Internal.SubParsers where

import           Control.Applicative        ((<$>))
import           Data.Char                  (isSpace)
import           Data.Either                (lefts, rights)
import           Data.List.Split            (splitOn)
import qualified Data.Map                   as M
import           Data.Monoid                (Monoid (..))
import           Language.Haskell.TH.Syntax (Name, TyLit (StrTyLit), Type (ConT, AppT, PromotedNilT, PromotedConsT, LitT),
                                             mkName, Dec, Exp, Exp(AppE, VarE, ConE))
import           Servant.API.Capture        (Capture)
import           Servant.API.Delete         (Delete)
import           Servant.API.Get            (Get)
import           Servant.API.Header         (Header)
import           Servant.API.MatrixParam    (MatrixParam)
import           Servant.API.Post           (Post)
import           Servant.API.Put            (Put)
import           Servant.API.QueryParam     (QueryFlag, QueryParam, QueryParams)
import           Servant.API.ReqBody        (ReqBody)
import           Servant.API.Sub            ((:>))
import           Servant.API.Alternative    ((:<|>))

-- * Types

type ParseResult = Either String Type

data Preamble = TypeTransPreamble (Type -> IO Type)
              | DecTransPreamble ([Exp] -> IO [Dec])
              | DecTypePreamble (Type -> IO Dec)


-- | `PreambleSubParser` parses an entry in the sitemap preamble. I.e.,
-- 'Type' and 'Verbose' in
--
-- @
-- Type: MyApi
-- Verbose: True
-- *************
-- DELETE   home
-- ...
-- @
--
-- Preamble options will generally modify the behavior of *all* paths.
data PreambleSubParser = PreambleSubParser
    { preambleName   :: String
    -- ^ Use to match against the preamble name (e.g. @"Type"@)
    , preambleSubParser :: String -> Preamble
    -- ^ Function from the contents of the preamble to a `Preamble`
    -- modifier.
    -- Note that if the @Preamble@ is a `DecTrans`, it will be ignored in
    -- case the QuasiQuoter is used in type position.
    }

-- | `MethodSubParser` parses the beginning of a path line. I.e., @GET@
-- in:
--
-- @
-- GET     home
--    Response: Int | XML
-- @
--
-- The type returned in the @Right@ of `ParseResult` by `methodSubParser`
-- will be placed at the *end* of the final type.
data MethodSubParser = MethodSubParser
    { methodName      :: String
    -- ^ Used to match against the method (e.g. @"GET"@)
    , methodExpects   :: [String]
    -- ^ List of options that the method requires (e.g. @"Response"@)
    , methodSubParser :: [String] -> ParseResult
    -- ^ Function from the contents of each option in `methodExpects` to a
    -- `ParseResult`. This function is guaranteed to be called with a list
    -- of the same length as @methodExpects@, with arguments in the same
    -- order. In case @methodExpects = [ "Response" ]@, in the example
    -- above @methodSubParser@ would be called with @[Just "Int | XML"]@.
    }

-- | `PathSubParser` parses a path segment. I.e., @capture@ in
--
-- @
-- DELETE   home/capture:my::string
--      AnOption: True
-- @
--
-- Path options modify the behavior of a single path segment.
data PathSubParser = PathSubParser
    { pathName      :: String
    -- ^ Used to match against the path segment (e.g. @"capture"@)
    , pathExpects   :: [String]
    -- ^ List of options that the path segment requires.
    , pathSubParser :: String -> [String] -> ParseResult
    -- ^ Function from the content of the paths, plus any options
    -- specified in `pathExpects`, to a @ParseResult@. The first argument,
    -- in the example above, would be @"my::string"@. And the second
    -- argument (which is guaranteed to have the same length as
    -- `pathExpects`, with elements in the same order) would be @["True"]@.
    }

-- | `OptionSubParser` parses an option. I.e., @AnOption@ in
--
-- @
-- DELETE   home/capture:my::string
--      AnOption: another string
-- @
--
-- Options modify the behavior of an entire route.
data OptionSubParser = OptionSubParser
    { optionName      :: String
    -- ^ Used to match against the option (e.g. @"AnOption"@)
    , optionSubParser :: String -> ParseResult
    -- ^ Function from the option string to `ParseResult`. In
    -- the example, the argument would be @"another string"@.
    }

-- | A data structure that includes all subparsers that will be used in
-- intepreting a string.
-- You should not need to construct a @SubParser@ directly; instead, use
-- @mempty@ or `defSubParser` and `addMethodSP`, `addPathSP`, `addOptSP`.
data SubParsers = SubParsers
    { preambleParsers :: M.Map String (String -> Preamble)
    , methodParsers   :: M.Map String ([String], [String] -> ParseResult)
    , pathParsers     :: M.Map String ([String] , (String, [String]) -> ParseResult)
    , optParsers      :: M.Map String (String -> ParseResult)
    , handlerParser   :: [String] -> IO [Exp]
    }


instance Monoid SubParsers where
    mempty = SubParsers M.empty M.empty M.empty M.empty (const $ return mempty)
    mappend a b = SubParsers
      { preambleParsers = preambleParsers a `unionSP` preambleParsers b
      , methodParsers   = methodParsers a `unionSP` methodParsers b
      , pathParsers     = pathParsers a `unionSP` pathParsers b
      , optParsers      = optParsers a `unionSP` optParsers b
      , handlerParser   = \x -> do
                                   a' <- handlerParser a x
                                   b' <- handlerParser b x
                                   return $ a' ++ b'
      }
      where unionSP = M.unionWithKey (\k _ _ -> error
                                     $ "Key '" ++ k ++ "'already exists!")

-- * Manipulating SubParsers

addPreambleSP :: PreambleSubParser -> SubParsers -> SubParsers
addPreambleSP PreambleSubParser{..} sp = sp {
    preambleParsers = M.insertWith (error $ "Key '" ++ preambleName ++ "'already exists!")
                                   preambleName
                                   preambleSubParser
                                   (preambleParsers sp)
    }

addMethodSP :: MethodSubParser -> SubParsers -> SubParsers
addMethodSP MethodSubParser{..} sp = sp {
    methodParsers = M.insertWith (error $ "Key '" ++ methodName ++ "'already exists!")
                                 methodName
                                 (methodExpects, methodSubParser)
                                 (methodParsers sp)
    }

addPathSP :: PathSubParser -> SubParsers -> SubParsers
addPathSP PathSubParser{..} sp = sp {
    pathParsers = M.insertWith (error $ "Key '" ++ pathName ++ "'already exists!")
                               pathName
                               (pathExpects, uncurry pathSubParser)
                               (pathParsers sp)
    }

addOptSP :: OptionSubParser -> SubParsers -> SubParsers
addOptSP OptionSubParser{..} sp = sp {
    optParsers = M.insertWith (error $ "Key '" ++ optionName ++ "'already exists!")
                              optionName
                              optionSubParser
                              (optParsers sp)
    }

-- * Built-in subparsers


defSubParser :: SubParsers
defSubParser = ffoldr addOptSP    [ request
                                  , header
                                  , queryParams
                                  ]
             $ ffoldr addPathSP   [ capture
                                  , matrix
                                  ]
             $ ffoldr addMethodSP [ getMethodParser
                                  , postMethodParser
                                  , putMethodParser
                                  , deleteMethodParser
                                  ]
             mempty
  where
    ffoldr x y z = foldr x z y


-- * Built-in Subparsers
-- ** Preambles

-- TODO:
serverPreamble :: PreambleSubParser
serverPreamble = PreambleSubParser
    { preambleName = "Server"
    , preambleSubParser = \name -> DecTypePreamble $ foldl1 go map VarE handlers
    }
    where
      go a b = AppE (AppE (ConE ''(:<|>)) (VarE $ mkName a)) b



-- ** Methods

getMethodParser :: MethodSubParser
getMethodParser = MethodSubParser
    { methodName = "GET"
    , methodExpects = ["Response"]
    , methodSubParser = withResponse "GET" ''Get
    }

postMethodParser :: MethodSubParser
postMethodParser = MethodSubParser
    { methodName = "POST"
    , methodExpects = ["Response"]
    , methodSubParser = withResponse "POST" ''Post
    }

putMethodParser :: MethodSubParser
putMethodParser = MethodSubParser
    { methodName = "PUT"
    , methodExpects = ["Response"]
    , methodSubParser = withResponse "PUT" ''Put
    }

deleteMethodParser :: MethodSubParser
deleteMethodParser = MethodSubParser
    { methodName = "DELETE"
    , methodExpects = []
    , methodSubParser = const . Right $ ConT ''Delete
    }


-- ** Paths
-- | @.../capture:<documentation-name>::<type>/...@
capture :: PathSubParser
capture = PathSubParser
    { pathName = "capture"
    , pathExpects = []
    , pathSubParser = go
    }
  where
     go x _ = case span (/= ':') x of
        ([], _) -> Left "'capture' path must have name to the right of '::'"
        (xs, ':':':':ys) -> Right $ AppT (AppT (ConT ''Capture) (strLit xs))
                             (ConT $ mkName ys)
        _ -> Left "'capture' path expects name and type separated by '::'"

-- | @.../matrix:<path>|<name1>::<type1>,<name2>::<type2>,.../...@
matrix :: PathSubParser
matrix = PathSubParser
    { pathName = "matrix"
    , pathExpects = []
    , pathSubParser = go
    }
  where
    mkTyp xs = case span (/= ':') xs of
        ([], _) -> Left "Each part of matrix should be of form '<name>::<type>'"
        (xs, ':':':':ys) -> Right $ AppT (AppT (ConT ''MatrixParam) (strLit xs))
                                           (ConT $ mkName ys)
        _ -> Left $ "'matrix' path could not parse: " ++ xs

    go x _ = case splitOn "|" x of
        [path, mps] -> case lefts (mkTyp <$> splitOn "," mps) of
           [] -> Right $ appTs [ ConT ''(:>)
                               , strLit path
                               , foldr1 pathUnion $ rights (mkTyp <$> splitOn "," mps)]
           xs -> Left $ unlines xs
        _ -> Left "'matrix' path expects format 'matrix:<path>|<name>::<type>'"


-- ** Options
-- | @Request: <type> | <content-type1>, <content-type2>, ...
request :: OptionSubParser
request = OptionSubParser
    { optionName = "Request"
    , optionSubParser = go
    }
  where
    go x = case splitOn "|" x of
        [xs, ys] -> Right $ appTs [ ConT ''ReqBody
                                  , mkCTList ys
                                  , ConT . mkName . removeWspace $ xs
                                  ]
        _        -> Left $ "'Request' option must have a type to the left "
                        ++ "of '|'"

-- | @Header: <header> | <type>@
header :: OptionSubParser
header = OptionSubParser
    { optionName = "Header"
    , optionSubParser = go
    }
  where
    go x = case splitOn "|" x of
        [xs, ys] -> Right $ appTs [ConT ''Header, strLit xs, ConT . mkName $ removeWspace ys]
        _        -> Left $ "'Header' option must have a string to the left "
                        ++ "of '|'"

-- | @QueryParam: <name1> :: <type1>, <name2> :: <type2>, ...@
queryParams :: OptionSubParser
queryParams = OptionSubParser
    { optionName = "QueryParams"
    , optionSubParser = go
    }
  where
    go x = case lefts parts of
        [] -> Right $ foldr1 pathUnion (toType <$> rights parts)
        xs -> Left $ mconcat xs
      where
        splitType s = case splitOn "::" s of
            [n, t] -> Right (strLit n, removeWspace t)
            _      -> Left $ "'QueryParams' option expects a list of "
                          ++ "comma-separated '<name> :: <type>' strings"
        parts = splitType <$> splitOn "," x
        toType (name, "Bool") = appTs [ConT ''QueryFlag, name]
        toType (name, '[':xs) = foldl1 AppT [ ConT ''QueryParams
                                      , name
                                      , ConT . mkName $ init xs
                                      ]
        toType (name, xs)     = foldl1 AppT [ConT ''QueryParam
                                            , name
                                            , ConT $ mkName xs]

-- * Utils

appTs :: [Type] -> Type
appTs = foldl1 AppT

strLit :: String -> Type
strLit = LitT . StrTyLit . removeWspace

removeWspace :: String -> String
removeWspace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

pathUnion :: Type -> Type -> Type
pathUnion a = AppT (AppT (ConT ''(:>)) a)


-- Helper function for method parsers that use the 'Response' option.
withResponse :: String -> Name -> [String] -> ParseResult
withResponse m typ [x] = case splitOn "|" x of
    [xs, ys] -> Right $ foldl1 AppT [ ConT typ
                              , mkCTList ys
                              , ConT . mkName . removeWspace $ xs
                              ]
    _        -> Left $ "Method " ++ m ++ " expects 'Response' option"
                   ++ " to have a type to the left of '|'."

-- For example:
-- mkCTList "JSON, XML" --> '[JSON, XML]
mkCTList :: String -> Type
mkCTList str = foldr (\x y -> foldl1 AppT [PromotedConsT, x, y])
                     PromotedNilT
                     (ConT . mkName . removeWspace <$> splitOn "," str)
