import Data.Music.Lilypond.Parse
import Text.Parsec
import Text.Parsec.String
import System.IO

main = do
  print =<< parseFromFile (parseLily <* eof) "ly-examples/bach-bwv610.ly"
  print =<< parseFromFile (parseLily <* eof) "data/issue-1.ly"
