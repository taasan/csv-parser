{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe            #-}

module Main
  ( main
  ) where

import           CSV.Parser
    ( parseCsv
    )
import qualified Data.Text.IO as T
import           Prelude
    ( Either (Left, Right)
    , IO
    , print
    , putStr
    )
import           Text.Megaparsec
    ( errorBundlePretty
    )

main :: IO ()
main = do
  input <- T.getContents
  case parseCsv ',' input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right res   -> print res
