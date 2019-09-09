{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.ParserSpec
  ( spec
  ) where

import           CSV.Parser
import qualified CSV.Parser as CSV
import           Data.Either
    ( Either (..)
    )
import           Data.Text
    ( Text
    )
import qualified Data.Text as T
import           Prelude
    ( flip
    , fmap
    , replicate
    , unlines
    , ($)
    , (.)
    , (<$>)
    , (<>)
    )
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec as P

parse :: Parser a -> Text -> Either ParseError a
parse = flip P.parse ""

spec :: Spec
spec =
  describe "Parser functions" $ do
    context "field" $ do
      let p = parse row
      it "parses an empty unquoted field" $ parse field "" `shouldParse` ""
      it "parses an empty quoted field" $
        parse CSV.field "\"\"" `shouldParse` ""
      it "parses a single quote" $ parse field "\"\"\"\"" `shouldParse` "\""
      it "fails on unterminated quote" $ p `shouldFailOn` "\""
      it "fails on unterminated quote" $ p `shouldFailOn` "\"a\"a\","
      it "fails on unterminated quote" $ p `shouldFailOn` "\" \" \","
      it "parses an unquoted field" $ p "aaa" `shouldParse` ["aaa"]
      it "parses a quoted field" $ p "\"a\"\"aa\"" `shouldParse` ["a\"aa"]
      it "parses a quoted field with LF" $
        p "\"a\"\"a\na\"" `shouldParse` ["a\"a\na"]
      it "parses a quoted field with CRLF" $
        p "\"a\"\"a\r\na\"" `shouldParse` ["a\"a\r\na"]
      it "stops parsing on separator" $
        parse field "aaa,bbb,ccc" `shouldParse` "aaa"
    context "row" $ do
      it "fails on empty line" $ parse CSV.row `shouldFailOn` ""
      it "parses a field with a single quote" $
        parse row "\"\"\"\"" `shouldParse` ["\""]
      it "parses a line with several empty fields" $
        parse CSV.row ",,," `shouldParse` ["", "", "", ""]
      it "parses a line with several unquoted fields" $
        parse CSV.row "aaa,bbb,ccc" `shouldParse` ["aaa", "bbb", "ccc"]
      it "parses row with TAB separator" $
        parse (CSV.rowS '\t') "aaa\tbbb\tccc" `shouldParse`
        ["aaa", "bbb", "ccc"]
      it "parses line with mixed (un-)quoted fields" $
        parse CSV.row "\"aa\"\"a\",bbb,\"ccc\"" `shouldParse`
        ["aa\"a", "bbb", "ccc"]
    describe "instance EncodeCsv" $ do
      context "encode field" $ do
        it "encodes empty string" $ encodeCsv empty `shouldBe` ("" :: Text)
        it "encloses value in quotes" $
          encodeCsv (Field "AB") `shouldBe` "\"AB\""
        it "encodes one quote" $
          encodeCsv (Field "\"") `shouldBe` ("\"\"\"\"" :: Text)
        it "escapes quotes" $
          encodeCsv (Field "A\"B") `shouldBe` ("\"A\"\"B\"" :: Text)
      context "Show record" $ do
        it "empty fields" $
          encodeCsv (replicate 5 empty) `shouldBe` (",,,,\n" :: Text)
        it "unquoted fields" $ encodeCsv (uqRow 1) `shouldBe` uqRowResult 1
        it "quoted fields" $ encodeCsv (qRow 1) `shouldBe` qRowResult 1
      context "Show records" $ do
        it "empty fields" $
          encodeCsv (replicate 5 $ emptyRow 5) `shouldBe`
          (unlines . replicate 5) ",,,,"
        it "unquoted fields" $
          encodeCsv (replicate 5 $ uqRow 5) `shouldBe` uqRowResult 5
    describe "API" . context "parseField" $ do
      it "parses empty" $ parseField' "," `shouldBe` Right empty
      it "parses unquoted" $ parseField' "A," `shouldBe` (Right . Field $ "A")
      it "parses quoted" $ parseField' "\"A\"," `shouldBe` (Right . Field $ "A")
      it "parses with alternative separator" $
        parseField ';' "\"A\";" `shouldBe` (Right . Field $ "A")
    describe "Helpers" . context "escape" $ do
      let p = parse escape
      it "parses one quote" $ p "\"\"" `shouldBe` Right "\""
      it "fails on unterminated quote" $ p `shouldFailOn` "\" \" \""
      it "fails on unterminated quote" $ p `shouldFailOn` "\"\\\"\\\"\""
      it "fails on unterminated quote" $ p `shouldFailOn` "\" \" \""
  where
    empty = Field ""
    emptyRow n = replicate n empty
    uqRow n = Field <$> replicate n "A"
    uqRowResult n =
      unlines . replicate n $ T.intercalate "," (replicate n "\"A\"")
    qRow n = Field <$> fmap quote (replicate n "A")
    qRowResult n = unlines (replicate n "\"\"\"A\"\"\"")
    parseField' :: Text -> Either ParseError Field
    parseField' = parseField ','

--    emptyRow :: (KnownNat n) => Int -> Record n
--    uqRow :: (KnownNat n) => Int -> Record n
--    qRow :: (KnownNat n) => Int -> Record n
quote :: Text -> Text
quote t = "\"" <> t <> "\""
