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
    , show
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
    context "csvFile" $ do
      it "parses two lines of unquoted records" $
        parse csvFile (unlines ["aaa,bbb,ccc", "ddd,eee,fff"]) `shouldParse`
        [["aaa", "bbb", "ccc"], ["ddd", "eee", "fff"]]
      it "parses two lines of quoted records" $
        parse
          csvFile
          (unlines ["\"aaa\",\"bbb\",\"ccc\"", "\"ddd\",\"eee\",\"fff\""]) `shouldParse`
        [["aaa", "bbb", "ccc"], ["ddd", "eee", "fff"]]
      it "parses two lines of mixed quoted an unquoted records" $
        parse csvFile (unlines ["aa\"a,bbb,ccc", "ddd,ee\"e,fff"]) `shouldParse`
        [["aa\"a", "bbb", "ccc"], ["ddd", "ee\"e", "fff"]]
    context "field" $ do
      it "parses an empty unquoted field" $ parse CSV.field "" `shouldParse` ""
      it "parses an empty quoted field" $
        parse CSV.field "\"\"" `shouldParse` ""
      it "parses an unquoted field" $ parse CSV.field "aaa" `shouldParse` "aaa"
      it "parses a quoted field" $
        parse CSV.field "\"a\"\"aa\"" `shouldParse` "a\"aa"
      it "parses a quoted field with LF" $
        parse CSV.field "\"a\"\"a\na\"" `shouldParse` "a\"a\na"
      it "parses a quoted field with CRLF" $
        parse CSV.field "\"a\"\"a\r\na\"" `shouldParse` "a\"a\r\na"
      it "stops parsing on separator" $
        parse CSV.field "aaa,bbb,ccc" `shouldParse` "aaa"
    context "row" $ do
      it "fails on empty line" $ parse CSV.row `shouldFailOn` ""
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
    describe "instance Show" $ do
      context "Show field" $ do
        it "is empty" $ show empty `shouldBe` ("" :: Text)
        it "encloses value in quotes" $
          show (Field "AB") `shouldBe` ("\"AB\"" :: Text)
        it "escapes quotes" $
          show (Field "A\"B") `shouldBe` ("\"A\"\"B\"" :: Text)
      context "Show record" $ do
        it "empty fields" $
          show (Record (replicate 5 empty)) `shouldBe` (",,,,\n" :: Text)
        it "unquoted fields" $ show (uqRow 1) `shouldBe` uqRowResult 1
        it "quoted fields" $ show (qRow 1) `shouldBe` qRowResult 1
      context "Show records" $ do
        it "empty fields" $
          show (replicate 5 $ emptyRow 5) `shouldBe`
          (unlines . replicate 5) ",,,,"
        it "unquoted fields" $
          show (replicate 5 $ uqRow 5) `shouldBe` uqRowResult 5
      -- it "unquoted fields" $ show uqRow `shouldBe` uqRowResult
      -- it "quoted fields" $ show qRow `shouldBe` qRowResult
    describe "API" . context "parseField" $ do
      it "parses empty" $ parseField' "," `shouldBe` Right empty
      it "parses unquoted" $ parseField' "A," `shouldBe` (Right . Field $ "A")
      it "parses quoted" $ parseField' "\"A\"," `shouldBe` (Right . Field $ "A")
      it "parses with alternative separator" $
        parseField ';' "\"A\";" `shouldBe` (Right . Field $ "A")
  where
    empty = Field ""
    emptyRow n = Record (replicate n empty)
    uqRow n = Record $ Field <$> replicate n "A"
    uqRowResult n =
      unlines . replicate n $ T.intercalate "," (replicate n "\"A\"")
    qRow n = Record $ Field <$> fmap quote (replicate n "A")
    qRowResult n = unlines (replicate n "\"\"\"A\"\"\"")
    parseField' = parseField ','

quote :: Text -> Text
quote t = "\"" <> t <> "\""
