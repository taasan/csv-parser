{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Parser
  ( Parser
  , EncodeCsv(..)
  , CsvOptions(..)
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
  , mkRecord
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
  ( fmap
  , liftM2
  )
import Data.Char
  ( Char
  )
import Data.String
  ( IsString (fromString)
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
import Data.Vector.Sized
  ( Vector
  )
import qualified Data.Vector.Sized as V
import Prelude
  ( Either (..)
  , Eq
  , Int
  , KnownNat
  , Maybe (..)
  , Ord
  , Show
  , ToText
  , first
  , mconcat
  , otherwise
  , pure
  , some
  , toText
  , void
  , ($)
  , (&&)
  , (.)
  , (/=)
  , (<<$>>)
  , (<>)
  , (<|>)
  , (==)
  )
import qualified Prelude
import Relude.Extra.Newtype
  ( un
  )

-- import Relude.Functor.Reexport (first)
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

-- TYPES
data FieldSeparator
  = Comma
  | Tabulator

data RecordSeparator
  = CRLF
  | LF

instance ToText FieldSeparator where
  toText Comma = ","
  toText Tabulator = "\t"

instance ToText RecordSeparator where
  toText CRLF = "\r\n"
  toText LF = "\n"

data CsvOptions =
  CsvOptions
    { fieldSeparator :: FieldSeparator
    , recordSeparator :: RecordSeparator
    }

rfc4180 :: CsvOptions
rfc4180 = CsvOptions {fieldSeparator = Comma, recordSeparator = CRLF}

newtype Field =
  Field Text
  deriving (Eq, Ord, Show)

newtype Record n =
  Record (Vector n Field)
  deriving (Eq, Ord, Show)

mkRecord :: (KnownNat n) => Int -> [Field] -> Maybe (Record n)
mkRecord len fs
  | len == Prelude.length fs = Record <$> V.fromList fs
  | otherwise = Nothing

class EncodeCsv a where
  encodeCsv :: CsvOptions -> a -> Text

-- INSTANCES
instance IsString Field where
  fromString = Field . Prelude.fromString

instance EncodeCsv Text where
  encodeCsv options = encodeField options . Field

instance EncodeCsv Field where
  encodeCsv = encodeField

instance EncodeCsv [Field] where
  encodeCsv options =
    (<> rsep) . (T.intercalate fsep . fmap (encodeField options))
    where
      fsep = toText . fieldSeparator $ options
      rsep = toText . recordSeparator $ options

instance EncodeCsv [[Field]] where
  encodeCsv options = mconcat . fmap (encodeCsv options)

instance EncodeCsv (Record n) where
  encodeCsv options = encodeCsv options . V.toList . un

instance EncodeCsv [Record n] where
  encodeCsv options = mconcat . fmap (encodeCsv options)

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

{-# INLINE encodeField #-}
encodeField :: CsvOptions -> Field -> Text
encodeField _ "" = ""
encodeField _ (Field s) = "\"" <> T.replace "\"" "\"\"" s <> "\""
