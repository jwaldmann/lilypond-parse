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

## Plan

Lilypond parsing is a "total mess" according to comments
by the authors in http://git.savannah.gnu.org/gitweb/?p=lilypond.git;a=blob;f=lily/parser.yy;hb=HEAD

For a sane language you'd expect
* lexical analysis (make stream of tokens)
* syntactic analysis (make abstract syntax tree)
* static semantics (type-check)
* dynamic emantics (evaluation)

Not so for lilypond. Even tokenisation seems to depend
on dynamic semantics, e.g., meaning of `<` `>` characters
(are they single tokens, or part of some longer token)
depends on context, and context depends on evaluating code.

We are aiming to parse some "reasonable subset",
and we are using Mutopia files as test cases.

# Status

5dcccbf0d03865b8c0f52413b16731f2e1ef6934 :

```
files below ../MutopiaProject/ftp/ : 5812 total 5240 OK 572 failed
```
most frequent first error:
```
338 occurrences of error
unexpected '>'
expecting item
example inputs:
"../MutopiaProject/ftp/WanhalJ/O42/wanhal-rondo/wanhal-rondo.ly" (line 73, column 10)
  g,8.[-> ( a16  g8)]
---------^
"../MutopiaProject/ftp/VerdiG/Traviata_Preludio/Traviata_Preludio.ly" (line 27, column 335)
... ations = #'(left) <fis-2>1} >> |
... ------------------------------^
"../MutopiaProject/ftp/VerdiG/Traviata_18/Traviata_18.ly" (line 46, column 122)
...  gis> d <e gis> d <e gis> } >> |
... ------------------------------^

```
but that's just first error, it could mask more errors later in the same file.
