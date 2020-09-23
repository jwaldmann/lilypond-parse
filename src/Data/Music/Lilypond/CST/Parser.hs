{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}

module Data.Music.Lilypond.CST.Parser where

import Data.Music.Lilypond.CST.Data
import Data.Music.Lilypond.Util

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Text.Parsec hiding (State)
import Text.Parsec.Pos
import qualified Control.Monad.State.Strict as CMS
import Control.Monad (void)

import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import qualified Data.Foldable as F

import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.String (fromString)

-- | http://lilypond.org/doc/v2.20/Documentation/notation/input-modes
data Mode = Chord | Drum | Figure | Lyrics | Markup | Note
  deriving (Eq, Ord, Read, Show)


-- see https://github.com/ejlilley/lilypond-parse/issues/9
data State = State
  { _mode :: ! Mode
  -- ^ lexical analysis depends on current input mode
  , _next_mode :: ! (Maybe Mode)
  -- ^ the mode that was announced recently,
  -- but we didn't see the open { for
  , _current :: ! (Seq Item)
  -- ^ sequence of items after most recent open delimiter
  , _open :: ! [(String, SourcePos)]
  -- ^ opening delimiters (parens) that are not closed
  }

$(makeLenses ''State)

state0 = State { _mode = Note
               , _next_mode = Nothing
               , _current = mempty
               , _open = mempty
               }

present :: State -> Doc
present s = P.vcat
  [ "mode:" <//> fromString (show $ s ^. mode)
  , "next_mode:" <//> fromString (show $ s ^. next_mode)
  , "most recent items in current group:"
   <//> numbered (  map (fromString . show . hide_group_content)
        $ ekat 5 $ F.toList $ s ^. current )
  , ""
  , "opening delimiters for most recent unclosed groups:"
    <//>  numbered ( do
      (op, pos) <- take 5 $ s ^. open
      return $ fromString op
        P.<+> fromString ( show $ hide_name  pos)
     )
  ]

hide_name :: SourcePos -> SourcePos
hide_name = flip setSourceName ""

type Parser = ParsecT TL.Text () (CMS.State State)


-- | @notarize p@ runs @p@ and pushes the result
-- onto the current state
notarize_item :: Parser Item -> Parser Item
notarize_item p = do
  i <- p
  current %= (Q.|> i)
  return i

notarize_open :: (String, SourcePos) -> Parser ()
notarize_open (op, here) = do
  current .= mempty
  open %= ((op,here):)

-- | like @between (expects open) (expects close) p@
-- with extra logging in the state
group :: String -> String -> Parser a -> Parser a
group op cl p = do
  here <- getPosition
  s <- CMS.get
  between (expects op) (expects cl)
     ( notarize_open (op,here) *> p )
    <* CMS.put s

white :: Parser ()
white = void $ many $
       void space
  <|> void ( (try $ string "%{") *> manyTill anyChar (try $ string "%}" ))
  <|> void ( char '%' *> manyTill anyChar (void endOfLine <|> eof) )

expect :: Char -> Parser ()
expect c = char c *> white

expects :: String -> Parser ()
expects s = try (string s) *> white


-- | parse from file, if it fails, add nicely formatted source snippet
-- FIXME: move this to separate module
parseFromFileSource
  :: Context
  -> Parser a
  -> FilePath
  -> IO (Either (ParseError, Doc) a)
parseFromFileSource con p f = do
  s <- TL.readFile f
  -- note on seq: in case of parse error,
  -- file is not closed otherwise
  seq (TL.length s) $ return ()

  let (a,st) = flip CMS.runState state0 $ runParserT p () f s
  return $ case a of
    Left e -> 
      let src = source_view con s e
          msg = P.vcat [src, "", present st ]
      in  Left (e, msg)
    Right x ->
      Right x
