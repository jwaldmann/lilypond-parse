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

import Text.Parsec hiding (State)
import qualified Control.Monad.State as CMS
import Text.Parsec.Char
import Data.Char (isPunctuation,isSymbol)
import qualified Text.Parsec.Text.Lazy as TPTL
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import Control.Monad (void)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import qualified Data.Foldable as F
import Lens.Micro
import qualified Data.Text.Prettyprint.Doc as P
import Data.String (fromString)

import Data.Music.Lilypond.Util


data CST = CST [ Item ]
  deriving Show

-- see https://github.com/ejlilley/lilypond-parse/issues/9
data State = State
  { current :: ! (Seq Item)
  -- ^ sequence of items after most recent open delimiter
  , open :: ! [(String, SourcePos)]
  -- ^ opening delimiters (parens) that are not closed
  }

state0 = State { current = mempty, open = mempty }

present :: State -> Doc
present s = P.vcat
  [ "most recent items in current group:"
   <//> numbered (  map (fromString . show . hide_group_content)
        $ ekat 5 $ F.toList $ current s )
  , ""
  , "opening delimiters for most recent unclosed groups:"
    <//>  numbered ( do
      (op, pos) <- take 5 $ open s
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
  CMS.modify' $ \ s ->
    s { current = current s Q.|> i }
  return i

notarize_open :: (String, SourcePos) -> Parser ()
notarize_open (op, here) = CMS.modify' $ \ s ->
  s { current = mempty , open = (op,here) : open s }

-- | like @between (expects open) (expects close) p@
-- with extra logging in the state
group :: String -> String -> Parser a -> Parser a
group op cl p = do
  here <- getPosition
  s <- CMS.get
  between (expects op) (expects cl)
     ( notarize_open (op,here) *> p )
    <* CMS.put s

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
  | Hidden
  deriving Show

hide_group_content :: Item -> Item
hide_group_content i = case i of
  Braces _ -> Braces [Hidden]
  Angles _ a -> Angles [Hidden] a
  DoubleAngles _ -> DoubleAngles [Hidden]
  Scm _ -> Scm Lisp_Hidden
  _ -> i

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
  <|> void ( char '%' *> manyTill anyChar (void endOfLine <|> eof) )

-- sub-parsers remove whitespace *after* themselves,
-- so the top-parser must eat whitespace at the start.

cst :: Parser CST
cst = CST <$> (optional bom *> white *> many item <* eof)

bom = char '\65279' -- wat?

item :: Parser Item
item = notarize_item $
  (   Command <$> command_name <* white
  <|> (String . T.pack ) <$> ( string_literal <* white )
  <|> Number <$> number
  <|> expect '=' *> return Equals
  <|> Braces <$> (group "{" "}" $ many item )
  <|> DoubleAngles <$> (group "<<" ">>" $ many item)
  <|> Angles <$> (group "<" ">"$ many item) <*> attach
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

-- https://github.com/ejlilley/lilypond-parse/issues/5
-- https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_9.html#SEC68
data Lisp = Symbol Text
  | Strng Text
  | Literal Lisp
  | Numbr { sign :: Maybe Char
          , integral :: Maybe String
          , fractional :: Maybe String
          , exponent :: Maybe String
          }
  | List [ Lisp ]
  | DotList [Lisp] Lisp
  | Quote Lisp
  | Backquote Lisp
  | Unquote Lisp -- comma
  | UnquoteList Lisp -- comma at
  | Ly [Item] -- back to ly
  | Lisp_Hidden
  deriving Show

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




