{- | concrete syntax tree for lilypond files.
This produces a tree of tokens
It just represents structure (nesting).
It does not represent semantics of tokens.

cf. https://github.com/ejlilley/lilypond-parse/issues/3
-}

{-# language OverloadedStrings, LambdaCase #-}

module Data.Music.Lilypond.CST where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text.Lazy
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import Control.Monad (void)

data CST = CST [ Item ]
  deriving Show

data Item 
  = Command Text
  | String Text
  | Number Number
  | Special Char -- ^ for single ( ) [ ]
  | Articulation Text  -- ^ starts with "-"
  | Name Text
  | Pitch { note :: Char
          , accidentals :: [Text]
         , octave :: Maybe Text
         , attached :: Attach
         }
  | Equals
  | Braces [Item]
  | Angles { items:: [Item]
           , attached :: Attach
           }
  | DoubleAngles [Item]
  | Identifier Text
  | Scm Lisp
  deriving Show

data Attach = Attach { duration :: Maybe Text
                     , dots :: [Char]
           , accidental :: Maybe Char
           , dynamic  :: [Text] -- ^ also articulation, ornamentation
           , multipliers :: [Number]
           , chord :: Maybe Text
           }
    deriving Show

data Number = Integ (Maybe Char) Text
  | Fraction (Maybe Char) Text Text
  | Dotted (Maybe Char) Text Text
  deriving Show

white :: Parser ()
white = void $ many $
       void space
  <|> void ( (try $ string "%{") *> manyTill anyChar (try $ string "%}" ))
  <|> void ( char '%' *> manyTill anyChar endOfLine )

-- sub-parsers remove whitespace *after* themselves,
-- so the top-parser must eat whitespace at the start.

cst :: Parser CST
cst = CST <$> (white *> many item <* eof)

item :: Parser Item
item =
      Command <$> command_name <* white
  <|> (String . T.pack ) <$> ( string_literal <* white )
  <|> Number <$> number
  -- parens/brackets are not necessarily nested,
  -- hence parse them as single symbols
  <|> Special <$> (oneOf "()[]|^_~") <* white 
  <|> expect '=' *> return Equals
  <|> Braces <$> (expect '{' *> manyTill item (expect '}') )
  <|> DoubleAngles <$> (between (expects "<<") (expects ">>") $ many item)
  <|> Angles <$> (between (expect '<') (expect '>') $ many item)
            <*> attach
  <|> Scm <$> (char '#' *> lisp <* white)
  <|> try pitch 
  <|> name
  <?> "item"

command_name = T.pack <$>
  ( char '\\' *> (many1 (letter <|> oneOf "-")
                   <|> (return <$> oneOf "<!\\[]")))

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
  <$> oneOf "cdefgabsR"
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
  <*> many (     (notFollowedBy (string "\\tweak") *>   try command_name)
             <|> try (char '\\' *> (T.pack <$> many1 digit) ) -- stringNumber
             <|> (char ':' *> (T.pack <$> many digit)) -- drum roll
             <|> (char '-' *> (try command_name
                               <|> T.pack <$> many1 (digit <|> oneOf "!-.+>[") ))
           )
  <* white
   <*> many ( expect '*' *>  number) --     , multipliers :: [Number]
   <*> return Nothing
   -- <*> optionMaybe (char ':' *> (T.pack <$> many1 alphaNum)) -- , chord :: Maybe Text
   <* white

-- https://github.com/ejlilley/lilypond-parse/issues/5
-- https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_9.html#SEC68
data Lisp = Symbol Text
  | Strng Text
  | Literal Text -- hash
  | Numbr (Maybe Char) String (Maybe String ) (Maybe String)
  | List [ Lisp ]
  | DotList [Lisp] Lisp
  | Quote Lisp
  | Backquote Lisp
  | Unquote Lisp -- comma
  | UnquoteList Lisp -- comma at
  | Ly [Item] -- back to ly
  deriving Show

lisp :: Parser Lisp
lisp = (Strng . T.pack) <$> string_literal <* lisp_white
  <|> numbr  <* lisp_white
  <|> list
  <|> Ly <$> (lisp_expects "#{" *> manyTill item (lisp_expects "#}") )
  <|> (Literal . T.pack) <$> (char '#' *> many1 (letter <|> oneOf ":"))
      <* lisp_white
  <|> Quote <$> (lisp_expect '\''  *> lisp)
  <|> Backquote <$> (lisp_expect '`' *> lisp)
  <|> Unquote <$> (lisp_expect ',' *> lisp)
  <|> (Symbol . T.pack) <$> many1 (alphaNum <|> oneOf "-+:*=!?><") <* lisp_white

lisp_white = void $ many $
    void ( char ';' *> manyTill anyChar endOfLine )
    <|> void space

-- https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_8.html#SEC51
-- FIXME:  we risk parsing "." as number.
-- one more risk: parsing "-" or "+" as number (hence try)
numbr :: Parser Lisp
numbr = try $ Numbr
  <$> optionMaybe ( oneOf "+-")
  <*> (string "inf" <|> many1 digit)
  <*> (optionMaybe $ char '.' *> many1 digit)
  <*> (optionMaybe $ (:) <$> oneOf "esfdl" <*> many1 digit)

list :: Parser Lisp
list = lisp_parens $ do
  prefix <- many lisp
  option (List prefix)
    (DotList prefix <$> (char '.' *> notFollowedBy alphaNum *> lisp_white *> lisp))
           
expect :: Char -> Parser ()
expect c = char c *> white

expects :: String -> Parser ()
expects s = try (string s) *> white

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


-- | parse from file, if it fails, add nicely formatted source snippet
-- FIXME: move this to separate module
parseFromFileSource
  :: Int -- ^ lines of context (use 0 .. 2)
  -> Int -- ^ chars of context (in error line) (use 30)
  -> Parser a
  -> FilePath
  -> IO (Either (ParseError, [ TL.Text ]) a)
parseFromFileSource lines_context chars_context p f = do
  s <- TL.readFile f
  -- note on seq: in case of parse error, file is not closed otherwise
  seq (TL.length s) $ case parse p f s of
    Left e -> do
      let pos = errorPos e
          row = sourceLine pos - 1
          col = sourceColumn pos - 1
      let (lines_pre, line_err : lines_post) = splitAt row $ TL.lines s
          ekat k = reverse . take k . reverse
      let 
          focus s = let (pre, post) = TL.splitAt (fromIntegral col) s
                        cc = fromIntegral chars_context
                        tl_ekat n = TL.reverse . TL.take  n . TL.reverse
                        pre' = if TL.length pre > cc
                               then "... " <> tl_ekat cc pre
                               else pre
                        post' = if TL.length post > cc
                               then TL.take cc post <> " ..."
                                else post
                  in    pre' <> post'
          location = replicate col '-' <> "^"
          
          msg = ekat lines_context lines_pre
              <> [ focus line_err, focus $ TL.pack location ]
             <> take lines_context lines_post
      return $ Left (e, msg)
    Right x -> return $ Right x





