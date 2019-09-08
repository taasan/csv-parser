{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Parser
  ( Parser
  , EncodeCsv(..)
  , Field(..)
  , Record(..)
  , ParseError
  , row
  , rowS
  , field
  , parseField
  , parseRecord
  , textParser
  , emptyStringParser
  , stringParser
  , escape
  ) where

import           Control.Applicative
    ( (<$)
    , (<$>)
    )
import           Control.Monad
    ( fmap
    , liftM2
    , return
    )
import           Data.Char
    ( Char
    )
import           Data.String
    ( IsString (fromString)
    )
import           Data.Text
    ( Text
    )
import qualified Data.Text as T
import           Data.Void
    ( Void
    )
import           Prelude
    ( Either (..)
    , Eq
    , Maybe (..)
    , Ord
    , Show
    , ToText
    , one
    , void
    , ($)
    , (&&)
    , (.)
    , (/=)
    , (<>)
    )
import qualified Prelude
import           Text.Megaparsec
    ( (<|>)
    )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
    ( char
    , eol
    , string
    )

-- TYPES
type Parser = P.Parsec Void Text

type ParseError = P.ParseErrorBundle Text Void

newtype Field =
  Field Text
  deriving (Eq, Ord, Show)

newtype Record =
  Record [Field]
  deriving (Show)

class EncodeCsv a where
  encodeCsv :: a -> Text

instance IsString Field where
  fromString = Field . Prelude.fromString

instance EncodeCsv Field where
  encodeCsv = encodeField

instance EncodeCsv [Field] where
  encodeCsv fs = encodeList (Just ',') $ fmap Right fs

instance EncodeCsv Record where
  encodeCsv = encodeRecord

instance EncodeCsv [Record] where
  encodeCsv r = encodeList Nothing $ fmap Right r

-- Helpers
encodeList :: (EncodeCsv b) => Maybe Char -> [Either Text b] -> Text
encodeList _ [] = ""
encodeList sep (x:xs) = shows x (showl sep xs)
  where
    shows :: (ToText a, EncodeCsv b) => Either a b -> (Text -> Text)
    shows x' s' = f x' <> s'
    f :: (ToText a, EncodeCsv b) => Either a b -> Text
    f x' =
      case x' of
        Left y  -> Prelude.toText y
        Right z -> encodeCsv z
    showl _ [] = ""
    showl sep' (y:ys) =
      case sep' of
        Just a  -> a `T.cons` showl Nothing ys
        Nothing -> shows y (showl Nothing ys)

-- API
parseField :: Char -> Text -> Either ParseError Field
parseField sep t = Field <$> parsed
  where
    parsed :: Either ParseError Text
    parsed = P.parse (fieldS sep) "" t

parseRecord :: Char -> Text -> Either ParseError Record
parseRecord sep t = mapFields <$> parsed
  where
    parsed :: Either ParseError [Text]
    parsed = P.parse (rowS sep) "" t

mapFields :: [Text] -> Record
mapFields = Record . (Field <$>)

{-# INLINE quote #-}
quote :: Parser Char
quote = P.char '"'

{-# INLINE escape #-}
escape :: Parser Text
escape = Prelude.toText <$> P.some (q <|> c)
  where
    q = P.label "escaped double quote" $ '"' <$ P.string "\"\""
    c = P.label "unescaped character" $ P.anySingleBut '"'

{-# INLINE fieldS #-}
fieldS :: Char -> Parser Text
fieldS = (<|> stringParser) . (<|> P.try emptyStringParser) . textParser

emptyStringParser :: Parser Text
emptyStringParser = do
  void quote
  void quote
  P.notFollowedBy quote
  return ""

{-# INLINE field #-}
field :: Parser Text
field = fieldS ','

{-# INLINE textParser #-}
textParser :: Char -> Parser Text
textParser c = do
  P.notFollowedBy quote
  P.takeWhileP Nothing p
  where
    p x = x /= c && x /= '\r' && x /= '\n' && x /= '"'

{-# INLINE stringParser #-}
stringParser :: Parser Text
stringParser = P.between quote quote escape

{-# INLINE row #-}
row :: Parser [Text]
row = rowS ','

{-# INLINE rowS #-}
rowS :: Char -> Parser [Text]
rowS c = do
  P.notFollowedBy P.eof -- to prevent reading empty line at the end of file
  res <- liftM2 P.sepBy1 fieldS P.char c
  void P.eol <|> void P.eof
  return res

{- Encoding -}
{-# INLINE encodeRecord #-}
encodeRecord :: Record -> Text
encodeRecord r = f r <> "\n"
  where
    f = T.intercalate "," . fmap encodeField . toFields
    toFields (Record fields) = fields

{-# INLINE encodeField #-}
encodeField :: Field -> Text
encodeField (Field "") = ""
encodeField (Field s) = T.concat ["\"", T.concatMap esc s, "\""]
  where
    esc :: Char -> Text
    esc '"' = "\"\""
    esc c   = one c
