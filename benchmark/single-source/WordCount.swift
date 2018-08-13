//===--- WordCount.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

//
// Mini benchmark counting words in a longer string.
// Measures performance of
//  - Iterating over the Characters in a String
//  - Extracting short substrings as Strings
//  - Set<Character> lookup performance
//  - Set<String> init from sequence of short Strings, with duplicates
//  - Uniquing initializer for Dictionary with short string keys
//

public let WordCount = [
  BenchmarkInfo(
    name: "WordSplitASCII",
    runFunction: run_WordSplitASCII,
    tags: [.validation, .api, .String, .algorithm, .unstable],
    setUpFunction: { buildWorkload() }
  ),
  BenchmarkInfo(
    name: "WordSplitUTF16",
    runFunction: run_WordSplitUTF16,
    tags: [.validation, .api, .String, .algorithm, .unstable],
    setUpFunction: { buildWorkload() }
  ),
  BenchmarkInfo(
    name: "WordCountUniqueASCII",
    runFunction: run_WordCountUniqueASCII,
    tags: [.validation, .api, .String, .Dictionary, .algorithm],
    setUpFunction: { buildWorkload() }
  ),
  BenchmarkInfo(
    name: "WordCountUniqueUTF16",
    runFunction: run_WordCountUniqueUTF16,
    tags: [.validation, .api, .String, .Dictionary, .algorithm],
    setUpFunction: { buildWorkload() }
  ),
  BenchmarkInfo(
    name: "WordCountHistogramASCII",
    runFunction: run_WordCountHistogramASCII,
    tags: [.validation, .api, .String, .Dictionary, .algorithm],
    setUpFunction: { buildWorkload() }
  ),
  BenchmarkInfo(
    name: "WordCountHistogramUTF16",
    runFunction: run_WordCountHistogramUTF16,
    tags: [.validation, .api, .String, .Dictionary, .algorithm],
    setUpFunction: { buildWorkload() }
  ),
]

let asciiText = """
**Welcome to Swift!**

Swift is a high-performance system programming language.  It has a clean and
modern syntax, offers seamless access to existing C and Objective-C code and
frameworks, and is memory safe by default.

Although inspired by Objective-C and many other languages, Swift is not itself a
C-derived language. As a complete and independent language, Swift packages core
features like flow control, data structures, and functions, with high-level
constructs like objects, protocols, closures, and generics. Swift embraces
modules, eliminating the need for headers and the code duplication they entail.

To learn more about the programming language, visit swift.org.

## Contributing to Swift

Contributions to Swift are welcomed and encouraged! Please see the
Contributing to Swift guide.

To be a truly great community, Swift.org needs to welcome developers from all
walks of life, with different backgrounds, and with a wide range of
experience. A diverse and friendly community will have more great ideas, more
unique perspectives, and produce more great code. We will work diligently to
make the Swift community welcoming to everyone.

To give clarity of what is expected of our members, Swift has adopted the code
of conduct defined by the Contributor Covenant. This document is used across
many open source communities, and we think it articulates our values well. For
more, see the Code of Conduct.

## Getting Started

These instructions give the most direct path to a working Swift development
environment. To build from source you will need 2 GB of disk space for the
source code and over 20 GB of disk space for the build artifacts. A clean build
can take multiple hours, but incremental builds will finish much faster.
"""

let utf16Text = """
✨🌟 Welcome tö Swift! ⭐️✨

Swift is a high-performance system programming language.  It has a clean and
modern syntax, offers seamless access tö existing C and Objective-C code and
frameworks, and is memory safe by default.

Although inspired by Objective-C and many othér languages, Swift is not itself a
C-derived language. As a complete and independent language, Swift packages core
features li\u{30A}ke flow control, data structures, and functions, with
high-level constructs li\u{30A}ke objects, protöcols, closures, and
generics. Swift embraces modules, eliminating thé need for headers and thé code
duplication théy entail.

Tö learn more about thé programming language, visit swift.org.

☞ Contributing tö Swift

Contributions tö Swift are welcomed and encouraged! Please see thé
Contributing tö Swift guide.

Tö be a truly great community, Swift.org needs tö welcome developers from all
walks of life, with different backgrounds, and with a wide range of
experience. A diverse and friendly community will have more great ideas, more
unique perspectives, and produce more great code. We will work diligently tö
make thé Swift community welcoming tö everyone.

Tö give clarity of what is expected of our members, Swift has adopted thé code
of conduct defined by thé Contributör Covenant. This document is used across
many open source communities, and we think it articulates our values well. For
more, see thé Code of Conduct.

☞ Getting Started

Thése instructions give thé most direct path tö a working Swift development
environment. Tö build from source you will need 2 GB of disk space for thé
source code and over 20 GB of disk space for thé build artifacts. A clean build
can take multiple hours, but incremental builds will finish much faster.
"""

@inline(never)
func buildWorkload() {
  blackHole(someAlphanumerics)
  blackHole(asciiWords)
  blackHole(utf16Words)
}

// A partial set of Unicode alphanumeric characters. (ASCII letters with at most
// one diacritic (of a limited selection), plus ASCII digits.)
let someAlphanumerics: Set<Character> = {
  let baseAlphabet = Set(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".unicodeScalars)
  let someCombiningDiacriticalMarks: Set<Unicode.Scalar> =
    Set((0x300..<0x310).map { Unicode.Scalar($0)! })

  var alphanumerics: Set<Character> = []
  for base in baseAlphabet {
    alphanumerics.insert(Character(base))
    for mark in someCombiningDiacriticalMarks {
      var v = String.UnicodeScalarView()
      v.append(base)
      v.append(mark)
      alphanumerics.insert(Character(String(v)))
    }
  }
  alphanumerics.formUnion("0123456789")
  return alphanumerics
}()

extension Character {
  var isAlphanumeric: Bool {
    return someAlphanumerics.contains(self)
  }
}

struct Words: IteratorProtocol, Sequence {
  public typealias Iterator = Words

  let text: String
  var nextIndex: String.Index

  init(_ text: String) {
    self.text = text
    self.nextIndex = text.startIndex
  }

  mutating func next() -> String? {
    while nextIndex != text.endIndex && !text[nextIndex].isAlphanumeric {
      text.formIndex(after: &nextIndex)
    }
    let start = nextIndex
    while nextIndex != text.endIndex && text[nextIndex].isAlphanumeric {
      text.formIndex(after: &nextIndex)
    }
    guard start < nextIndex else { return nil }
    return String(text[start..<nextIndex])
  }
}

@inline(never)
public func run_WordSplitASCII(_ N: Int) {
  for _ in 1...10*N {
    let words = Array(Words(identity(asciiText)))
    CheckResults(words.count == 280)
    blackHole(words)
  }
}

@inline(never)
public func run_WordSplitUTF16(_ N: Int) {
  for _ in 1...10*N {
    let words = Array(Words(identity(utf16Text)))
    CheckResults(words.count == 280)
    blackHole(words)
  }
}

let asciiWords = Array(Words(asciiText))
let utf16Words = Array(Words(utf16Text))

@inline(never)
public func run_WordCountUniqueASCII(_ N: Int) {
  for _ in 1...100*N {
    let words = Set(identity(asciiWords))
    CheckResults(words.count == 168)
    blackHole(words)
  }
}

@inline(never)
public func run_WordCountUniqueUTF16(_ N: Int) {
  for _ in 1...100*N {
    let words = Set(identity(utf16Words))
    CheckResults(words.count == 168)
    blackHole(words)
  }
}

/// Returns an array of all words in the supplied string, along with their
/// number of occurances. The array is sorted by decreasing frequency.
/// (Words are case-sensitive and only support a limited subset of Unicode.)
@inline(never)
func histogram<S: Sequence>(for words: S) -> [(String, Int)]
where S.Element == String {
  let histogram = Dictionary<String, Int>(
    words.lazy.map { ($0, 1) },
    uniquingKeysWith: +)
  return histogram.sorted { (-$0.1, $0.0) < (-$1.1, $1.0) }
}

@inline(never)
public func run_WordCountHistogramASCII(_ N: Int) {
  for _ in 1...100*N {
    let words = histogram(for: identity(asciiWords))
    CheckResults(words.count == 168)
    CheckResults(words[0] == ("and", 15))
    blackHole(words)
  }
}

@inline(never)
public func run_WordCountHistogramUTF16(_ N: Int) {
  for _ in 1...100*N {
    let words = histogram(for: identity(utf16Words))
    CheckResults(words.count == 168)
    CheckResults(words[0] == ("and", 15))
    blackHole(words)
  }
}
