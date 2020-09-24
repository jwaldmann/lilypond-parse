module Data.Music.Lilypond.CST.Data where

import Data.Text (Text)

data CST = CST [ Item ]
  deriving Show

data Item 
  = Command Text
  | String Text
  | Number Number
  | Special Char -- ^ for single ( ) [ ]
  | Expressive Height (Maybe Char) -- ^ starts with oneOf "^-_"
  | Multiplier Number  -- ^ starts with "*"
  | Name Text
  | Note { note_pitch :: Pitch
         , octave :: [Octave]
         , attached :: Attach
         }
  | Chord { chord_pitch :: Pitch
          , attached :: Attach
          , shape :: Shape
          , added :: [Step]
          , removed :: [Int]
          , inversion :: Maybe Inversion
          } -- ^ this is for the part after ':'
  | Equals
  | ModeBraces Text [Item]
  | Braces [Item]
  | Angles { items:: [Item]
           , attached :: Attach
           }
  | DoubleAngles [Item]
  | Identifier Text
  | Scm Lisp
  | Hidden
  deriving Show

data Pitch = Pitch Char [Accidental]
  deriving Show

data Accidental = Es | Is
  | Flat | Sharp -- ^ english
  | Eh | Ih -- ^ quarters
  deriving Show

data Octave = Down | Up
  deriving Show

-- | http://lilypond.org/doc/v2.20/Documentation/notation/expressive-marks
-- FIXME: docs say "expressive marks ... attached to notes".
-- how do we represent this attachment?
-- for now, they just appear next to a note,
-- but are not part of that note.
data Height = Sub | Dash | Super
  deriving Show


-- http://lilypond.org/doc/v2.20/Documentation/notation/chord-mode
data Shape = None | Min | Dim | Aug | Maj | Sus
  deriving Show

data Step = Step Int (Maybe Char)
  deriving Show

data Inversion = Inversion (Maybe Char) Pitch
  deriving Show

-- | after pitch or chord (in single < >)
data Attach = Attach { accidental :: Maybe Char

                     , duration :: Maybe Text
                     , dots :: [Char]
                     -- | http://lilypond.org/doc/v2.20/Documentation/notation/short-repeats#tremolo-repeats
                     , tremolo :: Maybe (Maybe Int)
           }
    deriving Show

data Number = Integ (Maybe Char) Text
  | Fraction (Maybe Char) Text Text
  | Dotted (Maybe Char) Text Text
  deriving Show


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


-- | only used for debugging output
hide_group_content :: Item -> Item
hide_group_content i = case i of
  Braces _ -> Braces [Hidden]
  Angles _ a -> Angles [Hidden] a
  DoubleAngles _ -> DoubleAngles [Hidden]
  Scm _ -> Scm Lisp_Hidden
  _ -> i

