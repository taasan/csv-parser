{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Criterion.Main
import CSV.Parser
  ( EncodeCsv (..)
  , Field (..)
  , Parser
  )
import qualified CSV.Parser as CSV
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Prelude
  ( Either
  , IO
  , Int
  , String
  , Text
  , replicate
  , ($)
  , (.)
  )
import Text.Printf

parse :: Parser a -> Text -> Either String a
parse = P.parseOnly

unquoted :: Int -> Text
unquoted n = T.intercalate "," $ replicate n "aaaaaaa"

quoted :: Int -> Text
quoted n = T.intercalate "," . replicate n $ encodeCsv $ Field "\"abc"

main :: IO ()
main = do
  let c = ','
      parseRecord = CSV.rowS c
      n = 100
      uq = unquoted n
      q = quoted n
  printf "Length of unquoted string: %d\n" (T.length uq)
  printf "Length of quoted   string: %d\n" (T.length q)
  defaultMain
    [ bgroup
        "row"
        [ bench "unquoted" $ whnf (parse parseRecord) uq
        , bench "quoted" $ whnf (parse parseRecord) q
        ]
    ]
