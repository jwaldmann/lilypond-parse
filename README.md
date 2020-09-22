# lilypond-parse

This package contains a Parsec parser for the Lilypond music typesetting language.

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

```
cabal configure
cabal install
```

## Development


Clone https://github.com/MutopiaProject/MutopiaProject/
as this gives plenty of test cases.

Install https://hackage.haskell.org/package/ghcid

```
cabal build --enable-tests
```

For ongoing development, start the following command
and keep it running.
Each time you save a file in the editor,
it will re-load sources files, compile,
and run the parser on all files
below the given root directories
```
cabal exec -- ghcid -c ghci -W -T ":main ../MutopiaProject/ftp/BachJS" -- -freverse-errors -isrc test/Main.hs
```
