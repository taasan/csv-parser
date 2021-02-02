{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Parser
  ( Parser
  , EncodeCsv(..)
  , EncoderOptions(..)
  , RecordSeparator(..)
  , FieldSeparator(..)
  , Field(..)
  , Record(..)
  , row
  , rowS
  , field
  , fieldS
  , parseField
  , parseRecord
  , fromFields
  , textParser
  , emptyQuotedStringParser
  , quotedStringParser
  , escapedQuotedText
  , rfc4180
  ) where

import Text.Parser.Combinators
  ( between
  , notFollowedBy
  )

import Control.Applicative
  ( (<$)
  , (<$>)
  )
import Control.Monad
  ( liftM2
  )
import Data.Char
  ( Char
  )
import Data.Text
  ( Text
  )
import qualified Data.Vector.Sized as V
import Prelude
  ( Either (..)
  , KnownNat
  , Maybe (..)
  , first
  , mconcat
  , pure
  , some
  , toText
  , void
  , ($)
  , (&&)
  , (.)
  , (/=)
  , (<<$>>)
  , (<|>)
  )

import qualified Data.Attoparsec.Combinator as P
import Data.Attoparsec.Text
  ( Parser
  , char
  , endOfInput
  , endOfLine
  , parseOnly
  , skipMany
  , string
  , takeWhile
  , takeWhile1
  , (<?>)
  )

import CSV.Encoder
import CSV.Types

-- API
parseField :: Char -> Text -> Either Text Field
parseField = ((<$>) Field .) . parsed fieldS

parseRecord :: Char -> Text -> Either Text [Field]
parseRecord = ((<<$>>) Field .) . parsed rowS

fromFields :: (KnownNat n) => [Field] -> Maybe (Record n)
fromFields = Record <<$>> V.fromList

parsed :: (Char -> Parser a) -> Char -> Text -> Either Text a
parsed f sep = first toText <$> parseOnly (f sep)

field :: Char -> Parser Field
field sep = Field <$> fieldS sep

row :: Char -> Parser [Field]
row sep = Field <<$>> rowS sep

{-# INLINE quote #-}
quote :: Parser Char
quote = char '"'

{-# INLINE escapedQuotedText #-}
escapedQuotedText :: Parser Text
escapedQuotedText = mconcat <$> some (q <|> c)
  where
    q = "\"" <$ string "\"\"" <?> "escaped double quote"
    c = takeWhile1 (/= '"') <?> "unescaped character"

{-# INLINE fieldS #-}
fieldS :: Char -> Parser Text
fieldS sep = emptyQuotedStringParser <|> quotedStringParser <|> textParser sep

emptyQuotedStringParser :: Parser Text
emptyQuotedStringParser = do
  skipSpace
  void quote
  void quote
  notFollowedBy quote
  skipSpace
  pure ""

{-# INLINE textParser #-}
textParser :: Char -> Parser Text
textParser c = takeWhile (\x -> x /= c && x /= '\r' && x /= '\n' && x /= '"')

{-# INLINE skipSpace #-}
skipSpace :: Parser ()
skipSpace = skipMany $ char ' '

{-# INLINE quotedStringParser #-}
quotedStringParser :: Parser Text
quotedStringParser = wrappedIn skipSpace parser
  where
    parser = wrappedIn quote escapedQuotedText
    wrappedIn p = between p p

{-# INLINE rowS #-}
rowS :: Char -> Parser [Text]
rowS c = do
  notFollowedBy endOfInput -- to prevent reading empty line at the end of file
  res <- liftM2 P.sepBy1 fieldS char c
  endOfLine <|> endOfInput
  pure res
