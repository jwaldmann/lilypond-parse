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

once:
```
cabal build --enable-tests
```
Clone https://github.com/MutopiaProject/MutopiaProject/
as this gives plenty of test cases.

For ongoing development, start the following command
(using https://hackage.haskell.org/package/ghcid)
and keep it running.
Each time you save a file in the editor,
it will re-load sources files, compile,
and run tests (on all files below the given root directories)
```
cabal exec -- ghcid -c ghci -W -T ":main ../MutopiaProject/ftp/BachJS" -- -freverse-errors -isrc test/Main.hs
```
