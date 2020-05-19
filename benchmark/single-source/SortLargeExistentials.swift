//===--- SortLargeExistentials.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test is a variant of the SortLettersInPlace.

import TestsUtils

public let SortLargeExistentials = BenchmarkInfo(
  name: "SortLargeExistentials",
  runFunction: run_SortLargeExistentials,
  tags: [.validation, .api, .algorithm, .cpubench],
  legacyFactor: 100)

protocol LetterKind {
  var value: String { get }
  func lessthan(_ rhs: LetterKind) -> Bool
}

// A struct which exceeds the size of the existential inline buffer.
struct Letter : LetterKind {
  let value: String

  // Make this struct a large struct which does not fit into the 3-word
  // existential inline buffer. Also provide an answer to ...
  var a: Int = 42
  var b: Int = 42
  var c: Int = 42
  var d: Int = 42

  init(_ value: String) {
    self.value = value
  }

  func lessthan(_ rhs: LetterKind) -> Bool {
    return value < rhs.value
  }
}

let lettersTemplate : [LetterKind] = [
  Letter("k"), Letter("a"), Letter("x"), Letter("i"), Letter("f"), Letter("l"),
  Letter("o"), Letter("w"), Letter("h"), Letter("p"), Letter("b"), Letter("u"),
  Letter("n"), Letter("c"), Letter("j"), Letter("t"), Letter("y"), Letter("s"),
  Letter("d"), Letter("v"), Letter("r"), Letter("e"), Letter("q"), Letter("m"),
  Letter("z"), Letter("g"),
  Letter("k"), Letter("a"), Letter("x"), Letter("i"), Letter("f"), Letter("l"),
  Letter("o"), Letter("w"), Letter("h"), Letter("p"), Letter("b"), Letter("u"),
  Letter("n"), Letter("c"), Letter("j"), Letter("t"), Letter("y"), Letter("s"),
  Letter("d"), Letter("v"), Letter("r"), Letter("e"), Letter("q"), Letter("m"),
  Letter("z"), Letter("g"),
  Letter("k"), Letter("a"), Letter("x"), Letter("i"), Letter("f"), Letter("l"),
  Letter("o"), Letter("w"), Letter("h"), Letter("p"), Letter("b"), Letter("u"),
  Letter("n"), Letter("c"), Letter("j"), Letter("t"), Letter("y"), Letter("s"),
  Letter("d"), Letter("v"), Letter("r"), Letter("e"), Letter("q"), Letter("m"),
  Letter("z"), Letter("g"),
  Letter("k"), Letter("a"), Letter("x"), Letter("i"), Letter("f"), Letter("l"),
  Letter("o"), Letter("w"), Letter("h"), Letter("p"), Letter("b"), Letter("u"),
  Letter("n"), Letter("c"), Letter("j"), Letter("t"), Letter("y"), Letter("s"),
  Letter("d"), Letter("v"), Letter("r"), Letter("e"), Letter("q"), Letter("m"),
  Letter("z"), Letter("g"),
  Letter("k"), Letter("a"), Letter("x"), Letter("i"), Letter("f"), Letter("l"),
  Letter("o"), Letter("w"), Letter("h"), Letter("p"), Letter("b"), Letter("u"),
  Letter("n"), Letter("c"), Letter("j"), Letter("t"), Letter("y"), Letter("s"),
  Letter("d"), Letter("v"), Letter("r"), Letter("e"), Letter("q"), Letter("m"),
  Letter("z"), Letter("g")
]

@inline(never)
public func run_SortLargeExistentials(_ N: Int) {
  for _ in 1...N {
    var letters = lettersTemplate

    letters.sort {
      return $0.lessthan($1)
    }

    // Check whether letters are sorted.
    CheckResults(letters[0].value <= letters[letters.count/2].value)
  }
}
