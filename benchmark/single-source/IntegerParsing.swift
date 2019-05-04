//===--- IntegerParsing.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let IntegerParsing = [
  BenchmarkInfo(name: "ParseInt.Small.Decimal",
    runFunction: run_ParseIntFromSmallDecimal,
    tags: [.validation, .api],
    setUpFunction: { blackHole(smallDecimalStrings) }),
  BenchmarkInfo(name: "ParseInt.Small.Binary",
    runFunction: run_ParseIntFromSmallBinary,
    tags: [.validation, .api],
    setUpFunction: { blackHole(smallBinaryStrings) }),
  BenchmarkInfo(name: "ParseInt.Small.Hex",
    runFunction: run_ParseIntFromSmallHex,
    tags: [.validation, .api],
    setUpFunction: { blackHole(smallHexStrings) }),
  BenchmarkInfo(name: "ParseInt.Small.UncommonRadix",
    runFunction: run_ParseIntFromSmallUncommonRadix,
    tags: [.validation, .api],
    setUpFunction: { blackHole(smallUncommonRadixStrings) }),
  BenchmarkInfo(name: "ParseInt.Large.Decimal",
    runFunction: run_ParseIntFromLargeDecimal,
    tags: [.validation, .api],
    setUpFunction: { blackHole(largeDecimalStrings) }),
  BenchmarkInfo(name: "ParseInt.Large.Binary",
    runFunction: run_ParseIntFromLargeBinary,
    tags: [.validation, .api],
    setUpFunction: { blackHole(largeBinaryStrings) }),
  BenchmarkInfo(name: "ParseInt.Large.Hex",
    runFunction: run_ParseIntFromLargeHex,
    tags: [.validation, .api],
    setUpFunction: { blackHole(largeHexStrings) }),
  BenchmarkInfo(name: "ParseInt.Large.UncommonRadix",
    runFunction: run_ParseIntFromLargeUncommonRadix,
    tags: [.validation, .api],
    setUpFunction: { blackHole(largeUncommonRadixStrings) }),
]

private let uncommonRadix: Int = 7
private let smallValuesSum: Int = 0
private let largeValuesSum: Int64 = 4790606764485943206
// Values
private let smallValues: [Int] = Array(-1000...1000)
private let largeValues: [Int64] = {
  var rng = SplitMix64(seed: 42)
  return (0..<2000).map { _ in Int64.random(in: .min ... .max, using: &rng) }
}()
// Strings
private let smallDecimalStrings: [String]
  = smallValues.map { String($0, radix: 10) }
private let smallBinaryStrings: [String]
  = smallValues.map { String($0, radix: 2) }
private let smallHexStrings: [String]
  = smallValues.map { String($0, radix: 16) }
private let smallUncommonRadixStrings: [String]
  = smallValues.map { String($0, radix: uncommonRadix) }
private let largeDecimalStrings: [String]
  = largeValues.map { String($0, radix: 10) }
private let largeBinaryStrings: [String]
  = largeValues.map { String($0, radix: 2) }
private let largeHexStrings: [String]
  = largeValues.map { String($0, radix: 16) }
private let largeUncommonRadixStrings: [String]
  = largeValues.map { String($0, radix: uncommonRadix) }

@inline(never)
public func run_ParseIntFromSmallDecimal(N: Int) {
  var result = 0
  let count = N*10
  for _ in 0..<count {
    for string in smallDecimalStrings {
      result &+= Int(string, radix: 10)!
    }
  }
  CheckResults(result == smallValuesSum &* count)
}

@inline(never)
public func run_ParseIntFromSmallBinary(N: Int) {
  var result = 0
  let count = N*10
  for _ in 0..<count {
    for string in smallBinaryStrings {
      result &+= Int(string, radix: 2)!
    }
  }
  CheckResults(result == smallValuesSum &* count)
}

@inline(never)
public func run_ParseIntFromSmallHex(N: Int) {
  var result = 0
  let count = N*10
  for _ in 0..<count {
    for string in smallHexStrings {
      result &+= Int(string, radix: 16)!
    }
  }
  CheckResults(result == smallValuesSum &* count)
}

@inline(never)
public func run_ParseIntFromSmallUncommonRadix(N: Int) {
  var result = 0
  let count = N*10
  for _ in 0..<count {
    for string in smallUncommonRadixStrings {
      result &+= Int(string, radix: uncommonRadix)!
    }
  }
  CheckResults(result == smallValuesSum &* count)
}

@inline(never)
public func run_ParseIntFromLargeDecimal(N: Int) {
  var result: Int64 = 0
  for _ in 0..<N {
    for string in largeDecimalStrings {
      result &+= Int64(string, radix: 10)!
    }
  }
  CheckResults(result == largeValuesSum &* Int64(N))
}

@inline(never)
public func run_ParseIntFromLargeBinary(N: Int) {
  var result: Int64 = 0
  for _ in 0..<N {
    for string in largeBinaryStrings {
      result &+= Int64(string, radix: 2)!
    }
  }
  CheckResults(result == largeValuesSum &* Int64(N))
}

@inline(never)
public func run_ParseIntFromLargeHex(N: Int) {
  var result: Int64 = 0
  for _ in 0..<N {
    for string in largeHexStrings {
      result &+= Int64(string, radix: 16)!
    }
  }
  CheckResults(result == largeValuesSum &* Int64(N))
}

@inline(never)
public func run_ParseIntFromLargeUncommonRadix(N: Int) {
  var result: Int64 = 0
  for _ in 0..<N {
    for string in largeUncommonRadixStrings {
      result &+= Int64(string, radix: uncommonRadix)!
    }
  }
  CheckResults(result == largeValuesSum &* Int64(N))
}
