{-# language PatternSignatures, LambdaCase #-}
{-# language OverloadedStrings #-}

import qualified Data.Music.Lilypond.Parse as DMLP
import qualified Data.Music.Lilypond.CST as DMLC
import qualified Data.Music.Lilypond.CST.Parser as DMLC
import Data.Music.Lilypond.Util

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
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.String (fromString)
import Control.Monad.State (lift)

main = getArgs >>= \ case
  
  [] -> main_for  [ "cst",  "data" ]
  args -> main_for args

main_for (variant : roots) = forM_ roots $ \ root ->  do
  fs <- getDirectoryFiles root [ "**/*.ly" ]
  errors <- forM fs $ \ f -> do
    -- FIXME: replace with proper test driver
    let run f = case variant of
          "parse" -> fmap (const ())
            <$> DMLP.parseFromFileSource context0 DMLP.parseLily f
          "cst" -> fmap (const ())
            <$> DMLC.parseFromFileSource context0 DMLC.cst f
    (run  $ root </> f ) >>= \ case
      Left (e, src) -> return $ Just (e, src)
      Right _ -> return Nothing
  let separator = fromString $ replicate 50 '*'
  let summary =     hsep
        [ "files below", fromString root, ":"
        , pretty (length fs), "total,"
        , pretty (length $ filter isNothing errors), "OK,"
        , pretty (length $ filter isJust errors), "failed."
        , line
        ]
  putDoc summary 
  let collect = M.fromListWith (<>) $ do
        Just (e, doc) <- errors
        return (sems $ errorMessages e, [(e,doc)])
  putDoc $ vcat $ do      
    (s, docs) <- sortOn (length . snd) $ M.toList collect
    return $ vcat
      [ separator
      , pretty (length docs) <+> "occurrences of error"
        <//> pretty s
      , ""
      , "example inputs:" <//>
       ( starred $ do
          (e,doc) <- take 3 docs
          return $ vcat [ pretty s
                        , "location:" <//> fromString ( show $ errorPos e)
                        , ""
                        , doc
                        , ""
                        ] )
      ]
  putDoc summary

-- wat: parsec does not export this ?? and pre-pends "\n" ??
sems :: [Message]-> String
sems  = drop 1
  . showErrorMessages "or" "unknown parse error"
  "expecting" "unexpected" "end of input"


nubOn f = nubBy ((==) `on` f)
