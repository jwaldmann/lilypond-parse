{-# language PatternSignatures, LambdaCase #-}

import Data.Music.Lilypond.Parse
import Data.Music.Lilypond.CST
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy.IO as TL
import System.IO
import System.FilePattern.Directory
import System.FilePath
import System.Environment
import Control.Exception
import Control.Monad (forM, forM_, void)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, isJust)
import Data.List (sortOn, nubBy)
import Data.Function (on)

main = getArgs >>= \ case
  [] -> main_for [ "data" ]
  args -> main_for args

main_for roots = forM_ roots $ \ root ->  do
  fs <- getDirectoryFiles root [ "**/*.ly" ]
  errors <- forM fs $ \ f -> do
    -- FIXME: replace with proper test driver
    ( parseFromFileSource 0 30 cst $ root </> f ) >>= \ case
      Left (e, src) -> return $ Just (e, src)
      Right _ -> return Nothing
  putStrLn $ unwords
    [ "files below", root, ":"
    , show (length fs), "total"
    , show (length $ filter isNothing errors), "OK"
    , show (length $ filter isJust errors), "failed"
    ]
  let collect = M.fromListWith (<>) $ do
        Just (e, src) <- errors
        return (sems $ errorMessages e, [(e,src)])
  forM_ (sortOn (length . snd) $ M.toList collect) $ \ (s, srcs) -> do
    putStrLn $ replicate 50 '*'
    putStr $ show (length srcs) <> " occurrences of error"
    putStrLn s
    putStrLn "example inputs:"
    forM_ (take 3 $ nubOn snd srcs) $ \ (e,src) -> do
      print $ errorPos e
      mapM_ TL.putStrLn src

-- wat: parsec does not export this ?? and pre-pends "\n" ??
sems :: [Message]-> String
sems  =  showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"


nubOn f = nubBy ((==) `on` f)
