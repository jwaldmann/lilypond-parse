import Data.Music.Lilypond.Parse
import Text.Parsec.String
import System.IO

main = do
  l <- parseFromFile parseLily "ly-examples/bach-bwv610.ly"
  print l
