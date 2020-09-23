module Data.Music.Lilypond.CST.Data where

import Data.Text (Text)

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
  | Hidden
  deriving Show

-- | only used for debugging output
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

