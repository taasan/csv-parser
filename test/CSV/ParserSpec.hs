{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.ParserSpec
  ( spec
  ) where

import CSV.Parser
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Prelude
  ( Either (..)
  , Show
  , String
  , Text
  , fmap
  , replicate
  , ($)
  , (.)
  , (<$>)
  , (<>)
  )
import Test.Hspec
import Test.Hspec.Attoparsec hiding
  ( shouldFailOn
  )
import qualified Test.Hspec.Attoparsec as Spec

parse :: Parser a -> Text -> Either String a
parse = P.parseOnly

shouldFailOn :: (Show a) => Parser a -> Text -> Expectation
shouldFailOn = Spec.shouldFailOn

spec :: Spec
spec =
  describe "Parser functions" $ do
    context "field" $ do
      it "parses an empty unquoted field" $ parse field' "" `shouldParse` ""
      it "parses an empty quoted field" $ parse field' "\"\"" `shouldParse` ""
      it "parses a single quote" $ parse field' "\"\"\"\"" `shouldParse` "\""
      it "fails on unterminated quote" $ row' `shouldFailOn` "\""
      it "fails on unquoted field with quote character" $
        row' `shouldFailOn` "12\""
      it "fails on unterminated quote" $ row' `shouldFailOn` "\"a\"a\","
      it "fails on unterminated quote" $ row' `shouldFailOn` "\" \" \","
      it "parses an unquoted field" $ parse row' "aaa" `shouldParse` ["aaa"]
      it "parses a quoted field" $
        parse row' "\"a\"\"aa\"" `shouldParse` ["a\"aa"]
      it "parses a quoted field with LF" $
        parse row' "\"a\"\"a\na\"" `shouldParse` ["a\"a\na"]
      it "parses a quoted field with CRLF" $
        parse row' "\"a\"\"a\r\na\"" `shouldParse` ["a\"a\r\na"]
      it "stops parsing on separator" $
        parse field' "aaa,bbb,ccc" `shouldParse` "aaa"
      it "skips spaces before quoted field" $
        parse field' "   \"aaa\"" `shouldParse` "aaa"
      it "skips spaces after quoted field" $
        parse field' "\"aaa\"   " `shouldParse` "aaa"
    context "row" $ do
      it "fails on empty line" $ row' `shouldFailOn` ""
      testRow "parses a field with a single quote" "\"\"\"\"" ["\""]
      testRow "parses a line with several empty fields" ",,," ["", "", "", ""]
      testRow
        "parses a line with several unquoted fields"
        "aaa,bbb,ccc"
        ["aaa", "bbb", "ccc"]
      testRow
        "parses line with mixed (un-)quoted fields"
        "\"aa\"\"a\",bbb,\"ccc\""
        ["aa\"a", "bbb", "ccc"]
      it "parses row with TAB separator" $
        parse (rowS '\t') "aaa\tbbb\tccc" `shouldParse` ["aaa", "bbb", "ccc"]
    describe "instance EncodeCsv" $ do
      context "encode field" $ do
        it "encodes empty string" $ encodeCsv' empty `shouldBe` ("" :: Text)
        it "encloses value in quotes" $
          encodeCsv' (Field "AB") `shouldBe` "\"AB\""
        it "encodes one quote" $
          encodeCsv' (Field "\"") `shouldBe` ("\"\"\"\"" :: Text)
        it "escapes quotes" $
          encodeCsv' (Field "A\"B") `shouldBe` ("\"A\"\"B\"" :: Text)
      context "Show record" $ do
        it "empty fields" $
          encodeCsv' (replicate 5 empty) `shouldBe` (",,,,\r\n" :: Text)
        it "unquoted fields" $ encodeCsv' (uqRow 1) `shouldBe` uqRowResult 1
        it "quoted fields" $ encodeCsv' (qRow 1) `shouldBe` qRowResult 1
        it "TAB separator" $
          let opt = rfc4180 {fieldSeparator = Tabulator}
           in encodeCsv opt (replicate 5 empty) `shouldBe`
              ("\t\t\t\t\r\n" :: Text)
      context "Show records" $ do
        it "empty fields" $
          encodeCsv' (replicate 5 $ emptyRow 5) `shouldBe`
          (unlines . replicate 5) ",,,,"
        it "unquoted fields" $
          encodeCsv' (replicate 5 $ uqRow 5) `shouldBe` uqRowResult 5
    describe "API" . context "parseField" $ do
      it "parses empty" $ parseField' "," `shouldBe` Right empty
      it "parses unquoted" $ parseField' "A," `shouldBe` (Right . Field $ "A")
      it "parses quoted" $ parseField' "\"A\"," `shouldBe` (Right . Field $ "A")
      it "parses with alternative separator" $
        parseField ';' "\"A\";" `shouldBe` (Right . Field $ "A")
    describe "Helpers" . context "escapedQuotedText" $ do
      it "parses one quote" $
        parse escapedQuotedText "\"\"" `shouldBe` Right "\""
      it "fails on unterminated quote" $
        escapedQuotedText `shouldFailOn` "\" \" \""
      it "fails on unterminated quote" $
        escapedQuotedText `shouldFailOn` "\"\\\"\\\"\""
      it "fails on unterminated quote" $
        escapedQuotedText `shouldFailOn` "\" \" \""
    describe "Attoparsec" . context "endOfLine" $ do
      it "succeeds on \\n" $ parse P.endOfLine "\n" `shouldBe` Right ()
      it "succeeds on \\r\\n" $ parse P.endOfLine "\r\n" `shouldBe` Right ()
  where
    encodeCsv' :: EncodeCsv a => a -> Text
    encodeCsv' = encodeCsv rfc4180
    field' = fieldS ','
    row' = rowS ','
    empty = Field ""
    emptyRow n = replicate n empty
    uqRow n = Field <$> replicate n "A"
    unlines s = T.intercalate "\r\n" s <> "\r\n"
    uqRowResult n =
      unlines . replicate n $ T.intercalate "," (replicate n "\"A\"")
    qRow n = Field <$> fmap quote (replicate n "A")
    qRowResult n = unlines (replicate n "\"\"\"A\"\"\"")
    parseField' = parseField ','
    parseRowLF s = parse row' $ s <> "\n"
    parseRowCRLF s = parse row' $ s <> "\r\n"
    testRow desc input expected = do
      it (desc <> " (LF)") $ parseRowLF input `shouldParse` expected
      it (desc <> " (CRLF)") $ parseRowCRLF input `shouldParse` expected

quote :: Text -> Text
quote t = "\"" <> t <> "\""
