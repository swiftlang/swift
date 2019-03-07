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
  name: "SuperChars2",
  runFunction: run_SuperChars,
  tags: [.validation, .api, .String],
  setUpFunction: { blackHole(alphabetInput) })

// Permute some characters.
let alphabetInput: [Character] = [
  "A", "B", "C", "D", "E", "F", "«",
  "á", "お", "S", "T", "U", "🇯🇵",
  "🧟‍♀️", "👩‍👦‍👦", "🕴🏿", "2", "?",
  ]

@inline(never)
public func run_SuperChars(_ N: Int) {
  // Permute some characters.
  let alphabet: [Character] = alphabetInput

  for _ in 0..<N {
    for firstChar in alphabet {
      for middleChar in alphabet {
        for lastChar in alphabet {
          blackHole((firstChar == middleChar) != (middleChar < lastChar))
        }
      }
    }
  }
}
