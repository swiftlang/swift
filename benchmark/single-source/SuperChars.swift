//===--- SuperChars.swift -------------------------------------------------===//
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

public let SuperChars = BenchmarkInfo(
  name: "SuperChars",
  runFunction: run_SuperChars,
  tags: [.validation, .api, .String],
  setUpFunction: { blackHole(alphabetInput) })

// Permute some characters.
let alphabetInput: [Character] = [
  "A", "B", "C", "D", "E", "F", "G",
   "«", // throw in some unicode to make it slower
  "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R",
  "á", "お",
  "S", "T", "U",
  "🇯🇵",
  "V", "W", "X", "Y", "Z", "/", "f", "Z", "z", "6", "7", "C", "j", "f", "9",
  "🇯🇵🇺🇸", "🇯🇵🇺🇸🇨🇳", "🇯🇵🇺🇸🇨🇳🇩🇪",
  "g", "g", "I", "J", "K", "c", "x", "i", ".",
  "🇯🇵🇺🇸🇨🇳🇩🇪", "🇯🇵🇺🇸", "🇯🇵🇺🇸🇨🇳",
  "2", "a", "t", "i", "o", "e", "q", "n", "X", "Y", "Z", "?", "m", "Z", ","
  ]

@inline(never)
public func run_SuperChars(_ N: Int) {
  // Permute some characters.
  let alphabet: [Character] = alphabetInput

  for _ in 0..<N {
    for firstChar in alphabet {
      for middleChar in alphabet {
        for lastChar in alphabet {
          _ = ((firstChar == middleChar) != (middleChar < lastChar))
        }
      }
    }
  }
}
