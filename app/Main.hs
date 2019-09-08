{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe            #-}

module Main
  ( main
  ) where

import           CSV.Parser
import qualified Data.Text.IO as T
import           Prelude
    ( Either (Left, Right)
    , IO
    , putStr
    , putText
    , ($)
    )
import           Text.Megaparsec
    ( errorBundlePretty
    )

main :: IO ()
main = do
  input <- T.getContents
  case parseCsv ',' input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right res   -> putText $ encodeCsv res
