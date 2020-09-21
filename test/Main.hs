{-# language PatternSignatures, LambdaCase #-}

import Data.Music.Lilypond.Parse
import Data.Music.Lilypond.CST
import Text.Parsec
import Text.Parsec.Text.Lazy
import System.IO
import System.FilePattern.Directory
import System.FilePath
import System.Environment
import Control.Exception
import Control.Monad (forM, forM_, void)

main = getArgs >>= \ case
  [] -> main_for [ "data" ]
  args -> main_for args

main_for roots = forM_ roots $ \ root ->  do
  fs <- getDirectoryFiles root [ "**/*.ly" ]
  frs <- forM fs $ \ f -> do
    -- FIXME: replace with proper test driver
    result <- handle (\ (e :: SomeException) -> do
                         print e; return False
                     ) $ do
      void $ pff cst $ root </> f
      return True
    return (f, result)
  putStrLn $ unwords
    [ "files below", root, ":"
    , show (length frs), "total"
    , show (length $ filter snd frs), "OK"
    , show (length $ filter (not . snd) frs), "failed"
    ]
