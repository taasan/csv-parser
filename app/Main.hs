{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe #-}

module Main
  ( main
  ) where

import           CSV
import           Data.Text
    ( Text
    )
import           Data.Text.IO
import           Prelude
    ( Either (Left, Right)
    , IO
    , bimap
    , lines
    , mapM_
    , stderr
    , stdout
    , (.)
    , (<$>)
    )
import           System.IO
    ( Handle
    )
import           Text.Megaparsec
    ( errorBundlePretty
    )

parse :: Text -> Either Text Text
parse = bimap (toText . errorBundlePretty) encodeCsv . parseRecord ','

handle :: Either Text Text -> (Handle, Text)
handle x =
  case x of
    Left err  -> (stderr, err)
    Right res -> (stdout, res)

print' :: (Handle, Text) -> IO ()
print' (h, x) = hPutStr h x

main :: IO ()
main = do
  input <- getContents
  let l = lines input
  let records = handle . parse <$> l
  mapM_ print' records
  -- void $ fmap print' input
  -- void $ hFlush stdout
  -- void $ hFlush stderr
  -- return ()
  -- where
  --   print' :: Text -> IO ()
  --   print' line = do
  --     case parseRecord ',' line of
  --       Left bundle -> putStr (errorBundlePretty bundle)
  --       Right res   -> putText $ encodeCsv res
