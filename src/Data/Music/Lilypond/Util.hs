{-# language OverloadedStrings, LambdaCase #-}

module Data.Music.Lilypond.Util where

import Text.Parsec hiding (State)
import qualified Data.Text.Prettyprint.Doc as P
import Data.String (fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Prelude hiding (lines) -- argh

data Context = Context
  { lines :: Int
  , chars :: Int
  }
  deriving Show

context0 = Context { lines = 1, chars = 30 }

source_view con s e =
  let pos = errorPos e
      row = sourceLine pos - 1
      col = sourceColumn pos - 1
      (lines_pre, lines_err_post) = splitAt row $ TL.lines s
      (line_err, lines_post) = case lines_err_post of
        [] -> ( "", [])
        l : ls -> (l, ls)
      focus s =
        let (pre, post) = splitAt col s
            cc = chars con
            pre' = if length pre > cc
                   then "... " <> ekat cc pre
                   else pre
            post' = if length post > cc
                    then take cc post <> " ..."
                    else post
        in    fromString $ pre' <> post'
      location = replicate col '-' <> "^"
  in  P.vcat
            $ map P.pretty (ekat (lines con) lines_pre)
              <> [ focus $ TL.unpack line_err, focus location ]
             <> map P.pretty (take (lines con) lines_post)
  
ekat k = reverse . take k . reverse

type Doc = P.Doc ()

numbered :: [Doc] -> Doc
numbered xs = P.vcat $ do
  (i,x) <- zip [1::Int .. ] xs
  return $ P.pretty i <> ". " <> P.align x

starred :: [Doc] -> Doc
starred xs = P.vcat $ do
  (i,x) <- zip [1::Int .. ] xs
  return $ "* " <> P.align x

p <//> q = P.vcat [p, P.indent 2 $ P.align q ]
