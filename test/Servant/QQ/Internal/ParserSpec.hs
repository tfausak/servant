module Servant.QQ.Internal.ParserSpec where

import Data.Monoid (Monoid(..))
import Data.Either (isRight)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Text.Parsec (parse)

import Servant.QQ.Internal.Parser
import Servant.QQ.Internal.SubParsers

spec :: Spec
spec = describe "Servant.QQ.Internal.Parser" $ do
    parsePreamblesSpec

parsePreamblesSpec :: Spec
parsePreamblesSpec = describe "parsePreambles" $ do
    let psp1 = PreambleSubParser "Test" (const $ TypeTransPreamble $ return . id)
    let sp = addPreambleSP psp1 mempty

    it "should parse preamble entries" $ do
        parse (parsePreambles sp) "<test file>" "Test: True" `shouldSatisfy`
            isRight

instance Show Preamble where
    show _ = "<PREAMBLE>"
