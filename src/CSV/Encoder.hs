{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Encoder
  ( EncoderOptions(..)
  , EncodeCsv(..)
  , rfc4180
  ) where

import CSV.Types

import qualified Data.Text as T
import qualified Data.Vector.Sized as V
import Relude
  ( Text
  , fmap
  , mconcat
  , toText
  , ($)
  , (.)
  , (<>)
  )
import Relude.Extra.Newtype
  ( un
  )

data EncoderOptions =
  EncoderOptions
    { fieldSeparator :: FieldSeparator
    , recordSeparator :: RecordSeparator
    }

class EncodeCsv a where
  encodeCsv :: EncoderOptions -> a -> Text

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

rfc4180 :: EncoderOptions
rfc4180 = EncoderOptions {fieldSeparator = Comma, recordSeparator = CRLF}

{-# INLINE encodeField #-}
encodeField :: EncoderOptions -> Field -> Text
encodeField _ "" = ""
encodeField _ (Field s) = "\"" <> T.replace "\"" "\"\"" s <> "\""
