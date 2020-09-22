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
and keep it running:
```
cabal exec -- ghcid -c ghci -W -T ":main ../MutopiaProject/ftp/BachJS" -- -freverse-errors -isrc test/Main.hs
```
Each time you save a file in the editor,
it will re-load sources files, compile,
and run the parser on all files
below the given root directories.

## Goals

This parser will be used by 
https://github.com/music-suite/music-suite,
a universal music description language and converter.

The natural goal for any conversion
is that round-trips should preserve semantics.

Specifically, we mean conversion between

* external textual representation (a lilypond file)
* and internal representation (Haskell data type
https://github.com/music-suite/music-suite/blob/master/src/Data/Music/Lilypond.hs#L184)

We do not consider other external representations (ABC)
or internal representations (Music.Score) here,
as that is outside the scope of this parser.
(It is inside the scope for music-suite as a whole, of course.)

One direction of the conversion
is already given by `instance Pretty Music`
(https://github.com/music-suite/music-suite/blob/master/src/Data/Music/Lilypond.hs#L267). The other direction is built
via this parser, details see below.

We may consider these semantics:

* internal representation
  (the equivalence is `parse . print == id`
  for all elements of `data Music`)
* external textual representation
  (the equivalence is `print . parse == id`
  for all files that lilypond accepts)

Exact external equivalence could be relaxed
by treating it modulo some other semantical mapping

* type-setting (compare PDF output)
  (the condition is `lilypond-to-pdf . print . parse = lilypond-to-pdf`)
* musical interpretation (compare MIDI output),
  the condition is
  * `lilypond-to-midi . print . parse = lilypond-to-midi` or,
  * perhaps more interesting,
  `music-suite-to-midi . parse = lilypond-to-midi`

## Lilypond Syntax

Lilypond parsing is a "total mess" according to comments
by the authors in http://git.savannah.gnu.org/gitweb/?p=lilypond.git;a=blob;f=lily/parser.yy;hb=HEAD

For a sane language you'd expect
* lexical analysis (make stream of tokens)
* syntactic analysis (make abstract syntax tree)
* static semantics (name resolution, type-check)
* dynamic emantics (evaluation)

Not so for lilypond. Even tokenisation seems to depend
on dynamic semantics, e.g., meaning of `<` `>` characters
(are they single tokens, or part of some longer token)
depends on context, and context depends on evaluating code.

We are aiming to parse some "reasonable subset",
and we are using Mutopia files as test cases.

## Lilypond Semantics

Assume we have manage to parse, and look at an AST.
In a sane language, its semantics would be *compositional*
(new-speak: algebraic, a `fold`, old-speak: syntax-directed).

Not so for lilypond. The language appears to consist
of atoms (denoting pitches) and operators
(for sequential and parallel composition).
Still, their semantics is fundamentally imperative:
there are commands to open (and close) *contexts*,
each context can have several *engravers*
(that do the actual type-setting)
and there are commands to send atoms to contexts.

On the other hand, music-suite has (I guess, and I hope)
a more algebraic semantics (this is their reason for being,
and my reason for using it). But this means that
conversion from Lilypond's imperative model is hard:
it seems that we have to *execute* lilypond score in some way,
to collect the atoms that appear at contexts/engravers.

This is the idea of putting a tree-walker
(actually, interpreter) behind the parser,
see https://github.com/ejlilley/lilypond-parse/issues/3.

## Status (Parser)

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

You might think that 5240 (OK) of 5812 (total) looks quite good
but "OK" just means that the parser did produce *something*.
These tests don't to any semantics!

We can only test semantics after we have the interpreter
that converts our CST (concrete syntax tree)
to music-suite's AST - so that we can use their semantical
functions (just print, or convert to other formats)


## Status (Interpreter)

not even started ...
