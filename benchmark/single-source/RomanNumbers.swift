//===--- RomanNumbers.swift -----------------------------------------------===//
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

// Mini benchmark implementing roman numeral conversions to/from integers.
// Measures performance of Substring.starts(with:), dropFirst and String.append
// with very short string arguments.

let t: [BenchmarkCategory] = [.api, .String, .algorithm]
let N = 270

public let RomanNumbers = [
  BenchmarkInfo(
    name: "RomanNumbers2",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanSSsWdF:)) },
    tags: t),
]

@inline(__always)
func checkId(_ n: Int, upTo limit: Int, _ itor: (Int) -> String,
  _ rtoi: (String) -> Int?) {
  for _ in 1...n {
   CheckResults(
     zip(1...limit, (1...limit).map(itor).map(rtoi)).allSatisfy { $0 == $1 })
  }
}

let romanTable: KeyValuePairs<String, Int> = [
  "M": 1000, "CM": 900, "D": 500, "CD": 400,
  "C": 100_, "XC": 90_, "L": 50_, "XL": 40_,
  "X": 10__, "IX": 9__, "V": 5__, "IV": 4__,
  "I": 1,
]

extension BinaryInteger {
  // Imperative Style
  // See https://www.rosettacode.org/wiki/Roman_numerals/Encode#Swift
  // See https://www.rosettacode.org/wiki/Roman_numerals/Decode#Swift

  var romanNumeral: String {
    var result = ""
    var n = self
    for (numeral, value) in romanTable {
      while n >= value {
        result += numeral
        n -= Self(value)
      }
    }
    return result
  }

  init?(romanSSsWdF number: String) {
    self = 0
    var raw = Substring(number)
    for (numeral, value) in romanTable {
      while raw.starts(with: numeral) {
        self += Self(value)
        raw = raw.dropFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
  }
}
