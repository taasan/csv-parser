{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  ) where

import           CSV.Encoder
import           CSV.Parser
import qualified Data.Text.IO as T
import           Prelude
    ( Either (..)
    , IO
    , putStr
    , ($)
    )
import           Text.Megaparsec

main :: IO ()
main = do
  input <- T.getContents
  case parse csvFile "" input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right res   -> T.putStrLn $ encode res
