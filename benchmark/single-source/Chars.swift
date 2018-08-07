//===--- Chars.swift ------------------------------------------------------===//
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

// This test tests the performance of ASCII Character comparison.
import TestsUtils

public let Chars = BenchmarkInfo(
  name: "Chars2",
  runFunction: run_Chars,
  tags: [.validation, .api, .String],
  setUpFunction: { blackHole(alphabetInput) })

var alphabetInput: [Character] = [
    "A", "B", "C", "D", "E", "F", "G",
    "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R",
    "S", "T", "U",
    "V", "W", "X", "Y", "Z", "/", "f", "Z", "z", "6", "7", "C", "j", "f", "9",
    "g", "g", "I", "J", "K", "c", "x", "i", ".",
    "2", "a", "t", "i", "o", "e", "q", "n", "X", "Y", "Z", "?", "m", "Z", ","
    ]

@inline(never)
public func run_Chars(_ N: Int) {
  // Permute some characters.
  let alphabet: [Character] = alphabetInput

  for _ in 0..<50*N {
    for firstChar in alphabet {
      for lastChar in alphabet {
        blackHole(firstChar < lastChar)
        blackHole(firstChar == lastChar)
        blackHole(firstChar > lastChar)
        blackHole(firstChar <= lastChar)
        blackHole(firstChar >= lastChar)
      }
    }
  }
}
