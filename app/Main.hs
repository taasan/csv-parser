{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  ) where

import CSV
import Data.Text
  ( Text
  )
import Relude

import Conduit
import Data.Conduit.Attoparsec

-- toCsv' :: (Functor f, Bifunctor g, EncodeCsv csv) => f (g a csv) -> f (g a Text)
-- toCsv' = fmap $ second encodeCsv
toCsv :: (Bifunctor g, EncodeCsv csv) => g a csv -> g a Text
toCsv = second encodeCsv

main :: IO ()
main = do
  let parser = conduitParser $ row ','
  runResourceT $
    runConduit $
    stdinC .| decodeUtf8C .| parser .| mapC (snd . toCsv) .| mapM_C putText
