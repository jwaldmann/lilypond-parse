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

import Text.Parsec hiding (State)
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
import Data.Music.Lilypond.CST.Data
import Data.Music.Lilypond.CST.Parser
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
  <|> Braces <$> (group "{" "}" $ with_switched_mode $ many item)
  <|> DoubleAngles <$> (group "<<" ">>" $ many item)
  <|> Angles <$> (group "<" ">" $ many item) <*> attach
  <|> Scm <$> (expect '#' *> lisp <* white)
  <|> special <* white
{-  
  -- parens/brackets are not necessarily nested,
  -- hence parse them as single symbols
  <|> Special <$> (notFollowedBy (oneOf "<>{}")
                   *> ( oneOf "()[]|^_~"
                        <|> satisfy isPunctuation
                        <|> satisfy isSymbol)
                  ) <* white
-}
  <|> when_mode Note *> pitch 
  <|> name
  <?> "item"
  )

special
  =  when_mode Markup *> 
     (Special <$> (satisfy isSymbol
                   <|> satisfy isPunctuation
                  ))
  <|> when_mode Note *> (Special <$> oneOf "|[]~")


-- http://lilypond.org/doc/v2.20/Documentation/notation/input-modes
when_mode :: Mode -> Parser ()
when_mode m = use mode >>= (guard  . (== m))

notarize_mode_switch :: Parser Text -> Parser Text
notarize_mode_switch p = do
  s <- p
  let modes = M.fromList
        [ ("chordmode", Chord), ("chords", Chord)
        , ("drummode" , Drum   ),  ("drums" , Drum)
        , ("figuremode" , Figure ), ("figures" , Figure)
        , ("lyricmode" , Lyrics ), ("lyrics" , Lyrics ), ("addlyrics" , Lyrics)
        , ("markup" , Markup)
        , ("notemode" , Note)
        ]
  next_mode .=  M.lookup s modes 
  return s  

with_switched_mode :: Parser a -> Parser a
with_switched_mode p = (next_mode <<.= Nothing) >>= \ case
  Nothing -> p
  Just m -> do
    previous <- (mode <<.= m)
    p <* (mode .= previous)
    

command_name = T.pack <$>
  ( char '\\' *> (many1 letter))

number :: Parser Number
number = (<* white) $ try $ do
  s <- optionMaybe $ oneOf "+-"
  top <- T.pack <$> many1 digit
  option (Integ s top) $ choice
    [ expect '/' *> ( (Fraction s top . T.pack) <$>  many1 digit)
    , char '.' *> ((Dotted s top . T.pack) <$>  many digit)
    ]

         
-- | e.g.,  a'4*2/3
pitch = Pitch
  <$> oneOf "cdefgabhqsrR" -- h: german b
  -- q: chord repetition, r: rest, R: whole bar rest, s: silent
  <*> many (T.pack <$> choice [string "is", string "es"])
  <*>  optionMaybe (T.pack <$> (many1 $ oneOf ",'"))
  <*> attach

-- | e.g., NoteColumn.ignore-collision
-- also strings inside lyricsmode. FIXME?
name = (Name . T.pack)
 <$>  ( when_mode Lyrics *>
          many1 (letter <|> digit <|> satisfy isPunctuation )
      <|> notFollowedBy (char '.') *>
          many1 (letter <|> char '.')
      )
 <* white      

                
attach :: Parser Attach
attach = Attach
  <$> optionMaybe (T.pack <$> many1 digit)
  <*> many (char '.')
  <*> optionMaybe (oneOf "!?") -- accidental
  <*> many (     try (cons_char '\\' (T.pack <$> many1 digit) ) -- stringNumber
             <|> (notFollowedBy (string "\\tweak") *>   try command_name)
             <|> (cons_char ':' (T.pack <$> many digit)) -- drum roll
             <|> (cons_char '-' (try command_name
                         <|> (T.singleton <$> (digit <|> oneOf "!-.+>[]()"))))
           )
  <* white
   <*> many ( expect '*' *>  number) --     , multipliers :: [Number]
   <* white

cons_char :: Char -> Parser Text -> Parser Text
cons_char c p = T.cons <$> char c *>  p
   
lisp :: Parser Lisp
lisp = (Strng . T.pack) <$> string_literal <* lisp_white
  <|> try numbr  <* lisp_white
  <|> list
  <|> Ly <$> (lisp_expects "#{" *> manyTill item (lisp_expects "#}") )
  <|> Literal <$> (lisp_expect '#' *> lisp)
  <|> Quote <$> (lisp_expect '\''  *> lisp)
  <|> Backquote <$> (lisp_expect '`' *> lisp)
  <|> Unquote <$> (lisp_expect ',' *> lisp)
  <|> (Symbol . T.pack) <$> many1 (alphaNum <|> oneOf "-+:*=!?></$") <* lisp_white
  <?> "lisp"

-- FIXME: this is wrong - they use lilypond comments (%) ?
lisp_white = void $ many $
    void ( char ';' *> manyTill anyChar endOfLine )
    <|> void space

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
list = lisp_parens $ do
  prefix <- many lisp
  option (List prefix)
    (DotList prefix <$> (char '.' *> notFollowedBy alphaNum *> lisp_white *> lisp))
           
braces :: Parser a -> Parser a
braces = between (expect '{') (expect '}')

parens :: Parser a -> Parser a
parens = between (expect '(') (expect ')')


lisp_expect :: Char -> Parser ()
lisp_expect c = char c *> lisp_white

lisp_expects :: String -> Parser ()
lisp_expects s = try (string s) *> lisp_white

lisp_braces :: Parser a -> Parser a
lisp_braces = between (lisp_expect '{') (lisp_expect '}')

lisp_parens :: Parser a -> Parser a
lisp_parens = between (lisp_expect '(') (lisp_expect ')')


string_literal :: Parser String
string_literal =
  -- string literals can span several lines !?
  let stringChar = stringLetter <|> stringEscape
      stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\'))
      stringEscape  = char '\\' *> anyChar
  in  char '"' *> manyTill stringChar (char '"')







