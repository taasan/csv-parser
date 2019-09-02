{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Parser
  ( Parser
  , Field
  , Record
  , csvFile
  , csvFileS
  , row
  , rowS
  , field
  , parseCsv
  , parseField
  , parseRow
  , toText
  , encodeField
  , encodeRecord
  , encode
  ) where

import           Control.Applicative
    ( (<$)
    , (<$>)
    )
import           Control.Monad
    ( fmap
    , liftM2
    )
import           Data.Bifunctor
    ( second
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
import           GHC.Show
    ( Show (show, showList, showsPrec)
    , ShowS
    )
import           Prelude
    ( Either
    , Eq
    , Maybe (Nothing)
    , Ord
    , ($)
    , (&&)
    , (.)
    , (/=)
    )
import           Text.Megaparsec
    ( (<|>)
    )
import qualified Text.Megaparsec as P
    ( Parsec
    , anySingleBut
    , between
    , eof
    , label
    , many
    , notFollowedBy
    , parse
    , sepBy1
    , sepEndBy1
    , takeWhileP
    )
import qualified Text.Megaparsec.Char as P
    ( char
    , eol
    , string
    )
import qualified Text.Megaparsec.Error as P

-- TYPES
type Parser = P.Parsec Void Text

type ParseError = P.ParseErrorBundle Text Void

newtype Field =
  Field Text
  deriving (Eq, Ord)

newtype Record =
  Record [Field]

-- ToText
class ToText a where
  toText :: a -> T.Text

-- Instances
-- TODO Add Read instances
instance ToText Field where
  toText (Field a) = a

instance ToText Record where
  toText = encodeRecord

toFields :: Record -> [Field]
toFields (Record fields) = fields

instance IsString Field where
  fromString = Field . T.pack

instance Show Field where
  show = T.unpack . encodeField
  showList = showList' ','

instance Show Record where
  show = T.unpack . encodeRecord
  showList = showList' '\n'

showList' :: (Show a) => Char -> [a] -> ShowS
showList' _ [] s = s
showList' sep (x:xs) s = shows x (showl xs)
  where
    shows = showsPrec 0
    showl []     = s
    showl (y:ys) = sep : shows y (showl ys)

-- API
parseField :: Char -> Text -> Either ParseError Field
parseField sep t = Field <$> P.parse (fieldS sep) "" t

parseRow :: Char -> Text -> Either ParseError Record
parseRow sep t = second map $ P.parse (rowS sep) "" t
  where
    map :: [Text] -> Record
    map = Record . (Field <$>)

parseCsv :: Char -> Text -> Either ParseError [Record]
parseCsv sep t = second map $ P.parse (csvFileS sep) "" t
  where
    map :: [[Text]] -> [Record]
    map = fmap (Record . fmap Field)

{-# INLINE quote #-}
quote :: Parser Char
quote = P.char '"'

{-# INLINE escape #-}
escape :: Parser Text
escape = T.pack <$> P.many (q <|> c)
  where
    q = P.label "escaped double quote" $ '"' <$ P.string "\"\""
    c = P.label "unescaped character" $ P.anySingleBut '"'

{-# INLINE fieldS #-}
fieldS :: Char -> Parser Text
fieldS = (stringParser <|>) . textParser

{-# INLINE field #-}
field :: Parser Text
field = fieldS ','

{-# INLINE textParser #-}
textParser :: Char -> Parser Text
textParser c = P.takeWhileP Nothing p
  where
    p x = x /= c && x /= '\r' && x /= '\n'

{-# INLINE stringParser #-}
stringParser :: Parser Text
stringParser = stringParserEscapeF escape

{-# INLINE stringParserEscapeF #-}
stringParserEscapeF :: Parser Text -> Parser Text
stringParserEscapeF = P.between quote quote

{-# INLINE row #-}
row :: Parser [Text]
row = rowS ','

{-# INLINE rowS #-}
rowS :: Char -> Parser [Text]
rowS c = do
  P.notFollowedBy P.eof -- to prevent reading empty line at the end of file
  liftM2 P.sepBy1 fieldS P.char c

{-# INLINABLE csvFile #-}
csvFile :: Parser [[Text]]
csvFile = csvFileS ','

{-# INLINABLE csvFileS #-}
csvFileS :: Char -> Parser [[Text]]
csvFileS = (`P.sepEndBy1` P.eol) . rowS

{- Encoding -}
{-# INLINABLE encode #-}
encode :: [Record] -> Text
encode rs = T.unlines (encodeRecord <$> rs)

{-# INLINE encodeRecord #-}
encodeRecord :: Record -> Text
encodeRecord = T.intercalate "," . fmap encodeField . toFields

{-# INLINE encodeField #-}
encodeField :: Field -> Text
encodeField "" = ""
encodeField s = T.concat ["\"", T.concatMap esc (toText s), "\""]
  where
    esc '"' = "\"\""
    esc c   = T.singleton c
