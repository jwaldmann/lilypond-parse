{-# language PatternSignatures #-}

import Data.Music.Lilypond.Parse
import Data.Music.Lilypond.CST
import Text.Parsec
import Text.Parsec.Text.Lazy
import System.IO
import System.Directory
import System.FilePath
import Control.Exception
import Control.Monad (forM_, void)

main = do
  -- print =<< parseFromFile (parseLily <* eof) "ly-examples/bach-bwv610.ly"
  -- print =<< parseFromFile (parseLily <* eof) "data/issue-1.ly"
  let d = "ly-examples"
  fs <- listDirectory d
  forM_ (filter ((== ".ly") . takeExtension) fs) $ \ f -> do
    putStr $ "********* " <> f
    -- FIXME: replace with proper test driver
    handle (\ (e :: SomeException) -> print e) $ do
      void $ pff cst ( d </> f)
      putStrLn " ... OK"
