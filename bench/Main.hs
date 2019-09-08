{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Criterion.Main
import           CSV.Parser
import qualified CSV.Parser as CSV
import qualified Data.Text as T
import           Prelude
    ( Either
    , IO
    , Text
    , flip
    , replicate
    , unlines
    , ($)
    , (.)
    )
import qualified Text.Megaparsec as P

parse :: Parser a -> Text -> Either ParseError a
parse = flip P.parse ""

unquoted :: Text
unquoted = T.intercalate "," $ replicate 100 "aaaaa"

-- unquoted = " -- " -- intercalate "," $ replicate 100 "aaaaa"
quoted :: Text
quoted = T.intercalate "," . replicate 100 $ encodeCsv $ Field "aa\"a\""

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
