{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , fromFields
  , textParser
  , emptyStringParser
  , stringParser
  , escape
  , mkRecord
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
import           Data.Vector.Sized
    ( Vector
    )
import qualified Data.Vector.Sized as V
import           Data.Void
    ( Void
    )

import           Prelude
    ( Either (..)
    , Eq
    , Int
    , KnownNat
    , Maybe (..)
    , Ord
    , Show
    , bifoldMap
    , one
    , void
    , ($)
    , (&&)
    , (.)
    , (/=)
    , (<>)
    , (==)
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

newtype Record n =
  Record (Vector n Field)
  deriving (Eq, Ord, Show)

mkRecord :: (KnownNat n) => Int -> [Field] -> Maybe (Record n)
mkRecord len fs =
  if len == Prelude.length fs
    then record
    else Nothing
  where
    record =
      case V.fromList fs of
        Nothing -> Nothing
        Just a  -> Just (Record a)

class EncodeCsv a where
  encodeCsv :: a -> Text

-- INSTANCES
instance IsString Field where
  fromString = Field . Prelude.fromString

instance EncodeCsv Field where
  encodeCsv = encodeField

instance EncodeCsv [Field] where
  encodeCsv = encodeRecord

instance EncodeCsv [[Field]] where
  encodeCsv = encodeRecords

instance EncodeCsv (Record n) where
  encodeCsv (Record fields) = encodeRecord $ V.toList fields

instance EncodeCsv [Record n] where
  encodeCsv r = encodeList Nothing $ fmap Right r

-- Helpers
encodeList :: (EncodeCsv a) => Maybe Char -> [Either Text a] -> Text
encodeList _ [] = ""
encodeList sep (x:xs) = shows x (showl xs)
  where
    shows :: (EncodeCsv a) => Either Text a -> (Text -> Text)
    shows x' s' = bifoldMap Prelude.toText encodeCsv x' <> s'
    showl :: (EncodeCsv a) => [Either Text a] -> Text
    showl [] = ""
    showl ys =
      case sep of
        Just a  -> a `T.cons` showl' ys
        Nothing -> showl' ys
    showl' (y:ys) = shows y (showl ys)
    showl' []     = ""

-- API
parseField :: Char -> Text -> Either ParseError Field
parseField sep t = Field <$> parsed fieldS sep t

parseRecord :: Char -> Text -> Either ParseError [Field]
parseRecord sep t = (Field <$>) <$> parsed rowS sep t

fromFields :: (KnownNat n) => [Field] -> Maybe (Record n)
fromFields fs =
  case V.fromList fs of
    Nothing -> Nothing
    Just v  -> Just (Record v)

parsed :: (Char -> Parser a) -> Char -> Text -> Either ParseError a
parsed f sep = P.parse (f sep) ""

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
{-# INLINE encodeRecords #-}
encodeRecords :: [[Field]] -> Text
encodeRecords r = encodeList Nothing $ fmap Right r

encodeRecord :: [Field] -> Text
encodeRecord fs = encodeList (Just ',') (fmap Right fs) <> "\n"

-- encodeCsv $ V.toList v
{-# INLINE encodeField #-}
encodeField :: Field -> Text
encodeField (Field "") = ""
encodeField (Field s) = T.concat ["\"", T.concatMap esc s, "\""]
  where
    esc :: Char -> Text
    esc '"' = "\"\""
    esc c   = one c
