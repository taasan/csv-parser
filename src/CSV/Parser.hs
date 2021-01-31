{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Parser
  ( Parser
  , EncodeCsv(..)
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
  , emptyStringParser
  , stringParser
  , escape
  , mkRecord
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
  , first
  , mconcat
  , otherwise
  , pure
  , some
  , toText
  , void
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
  , string
  , takeWhile
  , takeWhile1
  , (<?>)
  )

-- TYPES
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
  encodeCsv :: a -> Text

-- INSTANCES
instance IsString Field where
  fromString = Field . Prelude.fromString

instance EncodeCsv Text where
  encodeCsv = encodeField . Field

instance EncodeCsv Field where
  encodeCsv = encodeField

instance EncodeCsv [Field] where
  encodeCsv = (<> "\r\n") . (T.intercalate "," . fmap encodeField)

instance EncodeCsv [[Field]] where
  encodeCsv = mconcat . fmap encodeCsv

instance EncodeCsv (Record n) where
  encodeCsv = encodeCsv . V.toList . un

instance EncodeCsv [Record n] where
  encodeCsv = mconcat . fmap encodeCsv

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

{-# INLINE escape #-}
escape :: Parser Text
escape = mconcat <$> some (q <|> c)
  where
    q = "\"" <$ string "\"\"" <?> "escaped double quote"
    c = takeWhile1 (/= '"') <?> "unescaped character"

{-# INLINE fieldS #-}
fieldS :: Char -> Parser Text
fieldS = (<|> stringParser) . (<|> P.try emptyStringParser) . textParser

emptyStringParser :: Parser Text
emptyStringParser = do
  void quote
  void quote
  notFollowedBy quote
  pure ""

{-# INLINE textParser #-}
textParser :: Char -> Parser Text
textParser c = do
  notFollowedBy quote
  takeWhile p
  where
    p x = x /= c && x /= '\r' && x /= '\n' && x /= '"'

{-# INLINE stringParser #-}
stringParser :: Parser Text
stringParser = between quote quote escape

{-# INLINE rowS #-}
rowS :: Char -> Parser [Text]
rowS c = do
  notFollowedBy endOfInput -- to prevent reading empty line at the end of file
  res <- liftM2 P.sepBy1 fieldS char c
  void endOfLine <|> void endOfInput
  pure res

{-# INLINE encodeField #-}
encodeField :: Field -> Text
encodeField "" = ""
encodeField (Field s) = "\"" <> T.replace "\"" "\"\"" s <> "\""
