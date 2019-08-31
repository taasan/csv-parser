{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Encoder
  ( encodeField
  , encodeRecord
  , encode
  ) where

import           Data.List
    ( map
    )
import           Data.Text
    ( Text
    )
import qualified Data.Text as T

import           Prelude
    ( (.)
    )

{-# INLINABLE encode #-}
encode :: [[Text]] -> Text
encode rs = T.unlines (encodeRecord `map` rs)

{-# INLINE encodeRecord #-}
encodeRecord :: [Text] -> Text
encodeRecord = T.intercalate "," . map encodeField

{-# INLINE encodeField #-}
encodeField :: Text -> Text
encodeField "" = ""
encodeField s = T.concat ["\"", T.concatMap escape s, "\""]
  where
    escape '"' = "\"\""
    escape c   = T.singleton c
