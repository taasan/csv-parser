{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Types
  ( Field(..)
  , FieldSeparator(..)
  , Record(..)
  , RecordSeparator(..)
  ) where

import Relude
  ( Eq
  , IsString (fromString)
  , Ord
  , Show
  , Text
  , ToText (..)
  , (.)
  )

import Data.Vector.Sized
  ( Vector
  )

newtype Field =
  Field Text
  deriving (Eq, Ord, Show)

newtype Record n =
  Record (Vector n Field)
  deriving (Eq, Ord, Show)

data FieldSeparator
  = Comma
  | Tabulator

data RecordSeparator
  = CRLF
  | LF

instance IsString Field where
  fromString = Field . fromString

instance ToText FieldSeparator where
  toText Comma = ","
  toText Tabulator = "\t"

instance ToText RecordSeparator where
  toText CRLF = "\r\n"
  toText LF = "\n"
