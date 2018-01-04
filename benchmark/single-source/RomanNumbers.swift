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

//
// Mini benchmark implementing roman numeral conversions to/from integers.
// Measures performance of Substring.starts(with:) and String.append(),
// with very short string arguments.
//

public let RomanNumbers = [
  BenchmarkInfo(
    name: "RomanNumbers",
    runFunction: run_RomanNumbers,
    tags: [.api, .String, .algorithm])
]

let romanTable: [(Int, String)] = [
  (1000, "M"),
  (900, "CM"),
  (500, "D"),
  (400, "CD"),
  (100, "C"),
  (90, "XC"),
  (50, "L"),
  (40, "XL"),
  (10, "X"),
  (9, "IX"),
  (5, "V"),
  (4, "IV"),
  (1, "I"),
]

extension BinaryInteger {
  var romanNumeral: String {
    var result = ""
    var value = self
  outer:
    while value > 0 {
      var position = 0
      for i in position ..< romanTable.count {
        let (v, s) = romanTable[i]
        if value >= v {
          result += s
          value -= Self(v)
          position = i
          continue outer
        }
      }
      fatalError("Unreachable")
    }
    return result
  }

  init?(romanNumeral: String) {
    self = 0
    var input = Substring(romanNumeral)
  outer:
    while !input.isEmpty {
      var position = 0
      for i in position ..< romanTable.count {
        let (v, s) = romanTable[i]
        if input.starts(with: s) {
          self += Self(v)
          input = input.dropFirst(s.count)
          position = i
          continue outer
        }
      }
      return nil
    }
  }
}

@inline(never)
func checkRomanNumerals(upTo limit: Int) {
  for i in 0 ..< limit {
    guard Int(romanNumeral: identity(i.romanNumeral)) == i else {
      fatalError()
    }
  }
}

@inline(never)
public func run_RomanNumbers(_ N: Int) {
  for _ in 0 ..< 10 * N {
    checkRomanNumerals(upTo: 1100)
  }
}
