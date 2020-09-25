{-# language LambdaCase #-}


import qualified Data.Music.Lilypond.CST as DMLC
import qualified Data.Music.Lilypond.CST.Parser as DMLC
import Data.Music.Lilypond.Util (context0)
import System.Environment

main = getArgs >>= \ case
  [ f ] -> do
     DMLC.parseFromFileSource context0 DMLC.cst f >>= \ case
       Left (e, doc) -> do
         print e
         print doc
       Right m -> do
         print m


