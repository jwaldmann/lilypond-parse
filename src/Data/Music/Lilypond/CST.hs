{- | concrete syntax tree for lilypond files.
This produces a tree of tokens
It just represents structure (nesting).
It does not represent semantics of tokens.

cf. https://github.com/ejlilley/lilypond-parse/issues/3
-}

{-# language OverloadedStrings, LambdaCase #-}
{-# language TemplateHaskell #-}

module Data.Music.Lilypond.CST where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec hiding (State, try)
import Text.Parsec.Char
import Data.Char (isPunctuation,isSymbol)
import qualified Text.Parsec.Text.Lazy as TPTL
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import Control.Monad (void, guard)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import Data.String (fromString)
import qualified Data.Map.Strict as M

import Data.Music.Lilypond.Util
import Data.Music.Lilypond.CST.Data as D
import Data.Music.Lilypond.CST.Parser as P

import Lens.Micro.Mtl

-- | top-level parser, for contents of lilypond file.
-- sub-parsers remove whitespace *after* themselves,
-- so the top-parser must eat whitespace at the start.
cst :: Parser CST
cst = CST <$> (optional bom *> white *> many item <* eof)

bom = char '\65279' -- wat?

item :: Parser Item
item = notarize_item $ notFollowedBy (char '}') *>
  (   (String . T.pack ) <$> ( string_literal <* white )
  <|> Number <$> number <* white
  <|> expect '=' *> return Equals
  <|> Command <$> notarize_mode_switch command_name  <* white
  <|> Braces <$> (groupManySwitch "{" "}" item)
  <|> DoubleAngles <$> (groupMany "<<" ">>" item)
  <|> Angles <$> (groupMany "<" ">" item) <*> attach <* white
  <|> Scm <$> (expect '#' *> lisp <* white)
  <|> multiplier
  <|> expressive 
  <|> special <* white
  <|> when_mode P.Note *> try note <* white
  <|> when_mode P.Chord *> chord <* white
  <|> Name <$> name <* white
  <?> "item"
  )


special
  =  when_mode Markup *> 
     (Special <$> (satisfy isSymbol
                   <|> satisfy isPunctuation
                  ))
  <|> when_mode P.Note *> (Special <$> oneOf "|[]()~")
  <|> when_mode Lyrics *> (Special <$> oneOf "|")
  <|> when_mode P.Chord *> (Special <$> oneOf "|")


-- http://lilypond.org/doc/v2.20/Documentation/notation/input-modes
when_mode :: Mode -> Parser ()
when_mode m = use mode >>= (guard  . (== m))

notarize_mode_switch :: Parser Text -> Parser Text
notarize_mode_switch p = do
  s <- p
  let modes = M.fromList
        [ ("chordmode", P.Chord), ("chords", P.Chord)
        , ("drummode" , Drum   ),  ("drums" , Drum)
        , ("figuremode" , Figure ), ("figures" , Figure)
        , ("lyricmode" , Lyrics ), ("lyrics" , Lyrics ), ("addlyrics" , Lyrics)
        , ("markup" , Markup)
        , ("notemode" , P.Note)
        ]
  next_mode .=  M.lookup s modes 
  return s  


command_name = T.pack <$>
  ( char '\\' *> (many1 (letter {- <|> digit -} )
                  <|> string "\\"
                 <|> string "<" <|> string ">"
                 <|> string "!"
                 <|> string "(" <|> string ")"
                 ))

number :: Parser Number
number = try $ do
  s <- optionMaybe $ oneOf "+-"
  top <- T.pack <$> many1 digit
  option (Integ s top) $ choice
    [ expect '/' *> ( (Fraction s top . T.pack) <$>  many1 digit)
    , char '.' *> ((Dotted s top . T.pack) <$>  many digit)
    ]

-- | base note, accidentals (no octave, etc.)
pitch :: Parser Pitch
pitch = do
  base <- oneOf "cdefgabhsrR" -- h: german b
  -- r: rest, R: whole bar rest, s: silent
  acc <- if elem base ("ae" :: String)
    then option [] (char 's' *> return [Es])
    else return []
  (Pitch base . (acc <>)) <$> accidentals 

-- | http://lilypond.org/doc/v2.20/Documentation/notation/writing-pitches#note-names-in-other-languages
accidentals = many $ choices
       [("is",Is),("es",Es)
       ,("flat",Flat),("sharp",Sharp)
       ,("eh",Eh),("ih",Ih)
       ]
         
note = D.Note
  <$> pitch
  <*> (many $ choices [(",",Down),("'",Up)])
  <*> attach

-- | http://lilypond.org/doc/v2.20/Documentation/notation/chord-mode
chord = do
  p <- pitch; a <- attach
  option (D.Chord p a None [] [] Nothing) $ do
    char ':'
    D.Chord p a 
      <$> option None (choices
                      [("m",Min),("dim",Dim),("aug",Aug),("maj",Maj),("sus", Sus)])
      <*> flip sepBy (char '.') step
      <*> option [] (char '^' *> flip sepBy (char '.') nat )
      <*> optionMaybe (char '/' *>
                     ( Inversion <$> (optionMaybe $ char '+') <*> pitch ))

nat = read <$> many1 digit 

step :: Parser Step
step = Step <$> nat <*> optionMaybe (oneOf "+-")

choices :: [(String,a)] -> Parser a
choices  =
  choice . map (\ (k,v) -> string k *> return v) 

-- | e.g., NoteColumn.ignore-collision
-- also strings inside lyricsmode. FIXME?

-- FIXME in a Ly group inside a Scheme group,
-- there can be identifiers starting with dollar sign.
-- example: MutopiaProject/ftp/Traditional/greensleeves_guitar/greensleeves_guitar.ly
-- probably WONTFIX as long as we don't execute the Scheme code?

name = T.pack
 <$>  ( when_mode Lyrics *>
          many1 (letter <|> digit <|> satisfy isPunctuation )
      <|> notFollowedBy (char '.') *>
          many1 (letter <|> oneOf ".-")
      )
 <* white      

attach :: Parser Attach
attach = Attach
  <$> optionMaybe (oneOf "!?") -- accidental
  <*> optionMaybe (T.pack <$> many1 digit)
  <*> many (char '.')
  <*> optionMaybe (char ':' *> optionMaybe nat)

expressive :: Parser Item
expressive = Expressive
  <$> choices [("-", Dash),("^",Super),("_",Sub)]
  <*> optionMaybe (oneOf "^+-!>._" )
  <* white

text :: String -> Parser Text
text s = fromString <$> string s

multiplier = Multiplier <$> ( expect '*' *>  number <* white)

cons_char :: Char -> Parser Text -> Parser Text
cons_char c p = T.cons <$> char c *>  p
   
lisp :: Parser Lisp
lisp = (Strng . T.pack) <$> string_literal <* white
  <|> try numbr  <* white
  <|> list
  <|> Ly <$> (expects "#{" *> manyTill item (expects "#}") )
  <|> Literal <$> (expect '#' *> lisp)
  <|> Quote <$> (expect '\''  *> lisp)
  <|> Backquote <$> (expect '`' *> lisp)
  <|> Unquote <$> (expect ',' *> lisp)
  <|> (Symbol . T.pack) <$> many1 (alphaNum <|> oneOf "-+:*=!?></$") <* white
  <?> "lisp"


-- https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_8.html#SEC51
-- FIXME:  we risk parsing "." as number.
-- one more risk: parsing "-" or "+" as number
numbr :: Parser Lisp
numbr = do
  n <- Numbr
    <$> optionMaybe ( oneOf "+-")
    <*> optionMaybe (string "inf" <|> many1 digit)
    <*> (optionMaybe $ (:) <$> char '.' <*> many1 digit)
    <*> (optionMaybe $ (:) <$> oneOf "esfdl" <*> many1 digit)
  if isJust (integral n) || isJust (fractional n)
    then return n
    else unexpected "invalid lisp number"

list :: Parser Lisp
list = parens $ do
  prefix <- many lisp
  option (List prefix)
    (DotList prefix <$> (char '.' *> notFollowedBy alphaNum *> white *> lisp))
           
braces :: Parser a -> Parser a
braces = between (expect '{') (expect '}')

parens :: Parser a -> Parser a
parens = between (expect '(') (expect ')')


string_literal :: Parser String
string_literal =
  -- string literals can span several lines !?
  let stringChar = stringLetter <|> stringEscape
      stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\'))
      stringEscape  = char '\\' *> anyChar
  in  char '"' *> manyTill stringChar (char '"')







