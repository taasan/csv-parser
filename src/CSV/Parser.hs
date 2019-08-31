{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Parser
  ( Parser
  , csvFile
  , csvFileS
  , row
  , rowS
  , field
  ) where

import           Control.Monad
    ( liftM2
    )

import           Data.Char
    ( Char
    )
import           Data.Text
    ( Text
    )
import qualified Data.Text as T

import           Data.Void
    ( Void
    )

import           Text.Megaparsec
    ( Parsec
    , anySingleBut
    , between
    , eof
    , label
    , many
    , notFollowedBy
    , sepBy1
    , sepEndBy1
    , takeWhileP
    , (<|>)
    )
import           Text.Megaparsec.Char
    ( char
    , eol
    , string
    )

import           Prelude
    ( Maybe (..)
    , ($)
    , (&&)
    , (.)
    , (/=)
    , (<$)
    , (<$>)
    )

type Parser = Parsec Void Field

type Field = Text

type Record = [Field]

{-# INLINE quote #-}
quote :: Parser Char
quote = char '"'

{-# INLINE escape #-}
escape :: Parser Field
escape = T.pack <$> many (q <|> c)
  where
    q = label "escaped double quote" $ '"' <$ string "\"\""
    c = label "unescaped character" $ anySingleBut '"'

{-# INLINE fieldS #-}
fieldS :: Char -> Parser Field
fieldS = (stringParser <|>) . textParser

{-# INLINE field #-}
field :: Parser Field
field = fieldS ','

{-# INLINE textParser #-}
textParser :: Char -> Parser Field
textParser c = takeWhileP Nothing p
  where
    p x = x /= c && x /= '\r' && x /= '\n'

{-# INLINE stringParser #-}
stringParser :: Parser Field
stringParser = stringParserEscapeF escape

{-# INLINE stringParserEscapeF #-}
stringParserEscapeF :: Parser Field -> Parser Field
stringParserEscapeF = between quote quote

{-# INLINE row #-}
row :: Parser Record
row = rowS ','

{-# INLINE rowS #-}
rowS :: Char -> Parser Record
rowS c = do
  notFollowedBy eof -- to prevent reading empty line at the end of file
  liftM2 sepBy1 fieldS char c

{-# INLINABLE csvFile #-}
csvFile :: Parser [Record]
csvFile = csvFileS ','

{-# INLINABLE csvFileS #-}
csvFileS :: Char -> Parser [Record]
csvFileS = (`sepEndBy1` eol) . rowS
