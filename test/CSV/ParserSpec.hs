{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.ParserSpec
  ( spec
  ) where

import           Data.Either
    ( Either
    )
import           Data.Text
    ( Text
    )

import qualified Data.Text as T
import           Prelude
    ( flip
    , ($)
    )

import           CSV.Parser
import qualified CSV.Parser as CSV

import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec as P

parse :: Parser a -> Text -> Either ParseError a
parse = flip P.parse ""

spec :: Spec
spec = do
  context "csvFile" $ do
    it "parses two lines of unquoted records" $
      parse csvFile (T.unlines ["aaa,bbb,ccc", "ddd,eee,fff"]) `shouldParse`
      [["aaa", "bbb", "ccc"], ["ddd", "eee", "fff"]]
    it "parses two lines of quoted records" $
      parse
        csvFile
        (T.unlines ["\"aaa\",\"bbb\",\"ccc\"", "\"ddd\",\"eee\",\"fff\""]) `shouldParse`
      [["aaa", "bbb", "ccc"], ["ddd", "eee", "fff"]]
    it "parses two lines of mixed quoted an unquoted records" $
      parse csvFile (T.unlines ["aa\"a,bbb,ccc", "ddd,ee\"e,fff"]) `shouldParse`
      [["aa\"a", "bbb", "ccc"], ["ddd", "ee\"e", "fff"]]
  context "field" $ do
    it "parses an empty unquoted field" $ parse CSV.field "" `shouldParse` ""
    it "parses an empty quoted field" $ parse CSV.field "\"\"" `shouldParse` ""
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
      parse (CSV.rowS '\t') "aaa\tbbb\tccc" `shouldParse` ["aaa", "bbb", "ccc"]
    it "parses line with mixed (un-)quoted fields" $
      parse CSV.row "\"aa\"\"a\",bbb,\"ccc\"" `shouldParse`
      ["aa\"a", "bbb", "ccc"]
