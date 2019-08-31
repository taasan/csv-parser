{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Either
    ( Either
    )
import           Data.Text
    ( Text
    , intercalate
    , unlines
    )
import           Data.Void
    ( Void
    )

import           Criterion.Main
import           CSV.Encoder
import           CSV.Parser
import qualified CSV.Parser as CSV
import           Prelude
    ( IO
    , flip
    , replicate
    , ($)
    , (.)
    )
import qualified Text.Megaparsec as P

type ParseError = P.ParseErrorBundle Text Void

parse :: Parser a -> Text -> Either ParseError a
parse = flip P.parse ""

unquoted :: Text
unquoted = intercalate "," $ replicate 100 "aaaaa"

quoted :: Text
quoted = intercalate "," . replicate 100 $ encodeField "aa\"a\""

main :: IO ()
main =
  defaultMain
    [ bgroup
        "row"
        [ bench "unquoted" $ whnf (parse CSV.row) uq
        , bench "quoted" $ whnf (parse CSV.row) q
        ]
    , bgroup
        "csvFiles"
        [ bench "unquoted" . whnf (parse CSV.csvFile) $ file uq
        , bench "quoted" . whnf (parse CSV.csvFile) $ file q
        ]
    ]
  where
    file = unlines . replicate 10000
    uq = unquoted
    q = quoted
