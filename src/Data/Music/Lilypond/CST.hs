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
import Control.Monad (void)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import Data.String (fromString)

import Data.Music.Lilypond.Util
import Data.Music.Lilypond.CST.Data
import Data.Music.Lilypond.CST.Parser


-- sub-parsers remove whitespace *after* themselves,
-- so the top-parser must eat whitespace at the start.

cst :: Parser CST
cst = CST <$> (optional bom *> white *> many item <* eof)

bom = char '\65279' -- wat?

item :: Parser Item
item = notarize_item $
  (   Command <$> command_name <* white
  <|> (String . T.pack ) <$> ( string_literal <* white )
  <|> Number <$> number <* white
  <|> expect '=' *> return Equals
  <|> Braces <$> (group "{" "}" $ many item )
  <|> DoubleAngles <$> (group "<<" ">>" $ many item)
  <|> Angles <$> (group "<" ">" $ many item) <*> attach
  <|> Scm <$> (expect '#' *> lisp <* white)
  -- parens/brackets are not necessarily nested,
  -- hence parse them as single symbols
  <|> Special <$> (notFollowedBy (oneOf "<>{}")
                   *> ( oneOf "()[]|^_~"
                        <|> satisfy isPunctuation
                        <|> satisfy isSymbol)
                  ) <* white 
  <|> try pitch 
  <|> name
  <?> "item"
  )

command_name = T.pack <$>
  ( char '\\' *> (many1 (letter <|> oneOf "-")
                   <|> (return <$> oneOf "<>()!\\[]")))

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
  <$> oneOf "cdefgabqsrR"
  -- q: chord repetition, r: rest, R: whole bar rest, s: silent
  <*> many (T.pack <$> choice [string "is", string "es"])
  <*>  optionMaybe (T.pack <$> (many1 $ oneOf ",'"))
  <*> attach
  <* white

-- | e.g., NoteColumn.ignore-collision
-- FIXME: currently, also strings inside lyricsmode are parsed as names
name = (Name . T.pack) 
  <$> ( many1 (letter <|> oneOf ".-:,!" ))
  <* white
                
attach :: Parser Attach
attach = Attach
  <$> optionMaybe (T.pack <$> many1 digit)
  <*> many (char '.')
  <*> optionMaybe (oneOf "!?") -- accidental
  <*> many (     try (char '\\' *> (T.pack <$> many1 digit) ) -- stringNumber
             <|> (notFollowedBy (string "\\tweak") *>   try command_name)
             <|> (char ':' *> (T.pack <$> many digit)) -- drum roll
             <|> (char '-' *> (try command_name
                               <|> T.pack <$> many1 (digit <|> oneOf "!-.+>[]()") ))
           )
  <* white
   <*> many ( expect '*' *>  number) --     , multipliers :: [Number]
   <*> return Nothing
   -- <*> optionMaybe (char ':' *> (T.pack <$> many1 alphaNum)) -- , chord :: Maybe Text
   <* white

   
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







