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
// Measures performance of String/Substring/UTF8View with very short string
// arguments and methods: hasPrefix/starts(with:), removeFirst/dropFirst and
// String.append().
//
// For comparison, there's one extra variant with character based parsing
// algorithm: `Roman.DictCharInt.map.reduce`.

let t: [BenchmarkCategory] = [.api, .String, .algorithm]
let N = 270 // 1100

public let RomanNumbers = [
  // Imperative style variants:
  // String permuatations
  BenchmarkInfo(
    name: "Roman.String.hasPrefix.removeFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanShPrF:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.String.hasPrefix.dropFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanShPdF:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.String.startsWith.dropFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanSsWdF:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.String.startsWith.removeFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanSsWrF:)) },
    tags: t),
  // Substring permutations
  BenchmarkInfo(
    name: "Roman.Substring.hasPrefix.removeFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanSShPrF:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.Substring.hasPrefix.dropFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanSShPdF:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.Substring.startsWith.dropFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanSSsWdF:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.Substring.startsWith.removeFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanSSsWrF:)) },
    tags: t),
  // UTF8View SubSequence
  BenchmarkInfo(
    name: "Roman.UTF8ViewSS.startsWith.dropFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanU8SSsWdF:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.UTF8ViewSS.startsWith.removeFirst",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanU8SSsWrF:)) },
    tags: t),
  // FP-style variants:
  BenchmarkInfo(
    name: "Roman.String.hasPrefix.dropFirst.RI",
    runFunction: {
      checkId($0, upTo: N, { $0.romanReduceInto }, Int.init(romanReduceInto:))},
    tags: t),
  BenchmarkInfo(
    name: "Roman.String.hasPrefix.dropFirst.R",
    runFunction: {
      checkId($0, upTo: N, { $0.romanReduce }, Int.init(romanReduce:)) },
    tags: t),
  BenchmarkInfo(
    name: "Roman.DictCharInt.map.reduce",
    runFunction: {
      checkId($0, upTo: N, { $0.romanNumeral }, Int.init(romanMapReduce:)) },
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

let romanTableUTF8 = romanTable.map { ($0.utf8, $1) }

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

  // String permutations (romanS)

  init?(romanShPrF number: String) {
    self = 0
    var raw = number
    for (numeral, value) in romanTable {
      while raw.hasPrefix(numeral) {
        self += Self(value)
        raw.removeFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
  }

  init?(romanShPdF number: String) {
    self = 0
    var raw = number
    for (numeral, value) in romanTable {
      while raw.hasPrefix(numeral) {
        self += Self(value)
        raw = String(raw.dropFirst(numeral.count))
      }
    }
    guard raw.isEmpty else { return nil }
  }

  init?(romanSsWdF number: String) {
    self = 0
    var raw = number
    for (numeral, value) in romanTable {
      while raw.starts(with: numeral) {
        self += Self(value)
        raw = String(raw.dropFirst(numeral.count))
      }
    }
    guard raw.isEmpty else { return nil }
  }

  init?(romanSsWrF number: String) {
    self = 0
    var raw = number
    for (numeral, value) in romanTable {
      while raw.starts(with: numeral) {
        self += Self(value)
        raw.removeFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
  }

  // Substring permutations (romanSS)

  init?(romanSShPrF number: String) {
    self = 0
    var raw = Substring(number)
    for (numeral, value) in romanTable {
      while raw.hasPrefix(numeral) {
        self += Self(value)
        raw.removeFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
  }

  init?(romanSShPdF number: String) {
    self = 0
    var raw = Substring(number)
    for (numeral, value) in romanTable {
      while raw.hasPrefix(numeral) {
        self += Self(value)
        raw = raw.dropFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
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

  init?(romanSSsWrF number: String) {
    self = 0
    var raw = Substring(number)
    for (numeral, value) in romanTable {
      while raw.starts(with: numeral) {
        self += Self(value)
        raw.removeFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
  }

  // UTF8View SubSequence

  init?(romanU8SSsWdF number: String) {
    self = 0
    var raw = number.utf8[...]
    for (numeral, value) in romanTableUTF8 {
      while raw.starts(with: numeral) {
        self += Self(value)
        raw = raw.dropFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
  }

  init?(romanU8SSsWrF number: String) {
    self = 0
    var raw = number.utf8[...]
    for (numeral, value) in romanTableUTF8 {
      while raw.starts(with: numeral) {
        self += Self(value)
        raw.removeFirst(numeral.count)
      }
    }
    guard raw.isEmpty else { return nil }
  }
}

extension BinaryInteger {
  // FP-style
  // Following is a translation of the imperative algorithm into functional
  // style: for-in loop is replaced with reduction and while loop with recusion.
  // XXX: These functions are not tail call optimized... 🤷‍♂️ (ARC on Strings?)

  typealias State = (number: String, value: Self)
  typealias Roman = (numeral: String, value: Int)

  // Classic functional style with reduce

  static func parseRomanNumeral(_ running: State, candidate r: Roman) -> State {
    guard running.number.hasPrefix(r.numeral) else { return running }
    return parseRomanNumeral((String(running.number.dropFirst(r.numeral.count)),
       running.value + Self(r.value)), candidate: r)
  }

  static func buildRomanNumeral(_ running: State, candidate r: Roman) -> State {
    guard running.value >= r.value else { return running }
    return buildRomanNumeral(
      (running.number + r.numeral, running.value - Self(r.value)), candidate: r)
  }

  var romanReduce: String {
    return romanTable.reduce(("", self), Self.buildRomanNumeral).number
  }

  init?(romanReduce number: String) {
    let (remainder, value) = romanTable.reduce(
      (number, Self(0)), Self.parseRomanNumeral)
    guard remainder.isEmpty else { return nil }
    self = value
  }

  // Swifty mutable hybrid functional style with reduce(into:)

  static func parseRomanNumeral(_ running: inout State, candidate r: Roman) {
    guard running.number.hasPrefix(r.numeral) else { return }
    running.number = String(running.number.dropFirst(r.numeral.count))
    running.value += Self(r.value)
    parseRomanNumeral(&running, candidate: r)
  }

  static func buildRomanNumeral(_ running: inout State, candidate r: Roman) {
    guard running.value >= r.value else { return }
    running.value -= Self(r.value)
    running.number += r.numeral
    buildRomanNumeral(&running, candidate: r)
  }

  var romanReduceInto: String {
    return romanTable.reduce(into: ("", self), Self.buildRomanNumeral).number
  }

  init?(romanReduceInto number: String) {
    let (remainder, value) = romanTable.reduce(into:
      (number, Self(0)), Self.parseRomanNumeral)
    guard remainder.isEmpty else { return nil }
    self = value
  }
}

// Parsing with Dictionary and map reduce.
// See `fromRoman2` https://www.rosettacode.org/wiki/Roman_numerals/Decode#Scala

let romanDigits: Dictionary<Character, Int> = [
  "I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000
]

extension BinaryInteger {
  typealias RunningSum = (sum: Self, last: Self)

  static func sumRomanDigits(r: RunningSum?, digitValue: Self?) -> RunningSum? {
    switch (r, digitValue) {
    case let (r?, value?):
      return (r.sum + value - (r.last < value ? 2 * r.last : 0), value)
    default:
      return nil
    }
  }

  init?(romanMapReduce number: String) {
    guard let r = (number
      .lazy // brings about 2x improvement over eager
      .map { romanDigits[$0].map { Self($0) } }
      .reduce((Self(0), Self(0)), Self.sumRomanDigits)
    ) else { return nil }
    self = r.sum
  }
}
