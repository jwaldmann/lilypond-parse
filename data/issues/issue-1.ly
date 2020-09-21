\version "2.18"

\layout {
  \accidentalStyle modern
  \compressFullBarRests
}

\book {
  \paper {
    #(set-paper-size "a5")
    print-all-headers = ##t
    page-breaking = #ly:one-page-breaking
  }

  \include "small.ly"
}
