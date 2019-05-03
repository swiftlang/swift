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
  BenchmarkInfo(name: "ParseIntFromDecimal",
    runFunction: run_ParseIntFromDecimal,
    tags: [.validation, .api],
    setUpFunction: { blackHole(decimalStrings) }),
  BenchmarkInfo(name: "ParseIntFromBinary",
    runFunction: run_ParseIntFromBinary,
    tags: [.validation, .api],
    setUpFunction: { blackHole(binaryStrings) }),
  BenchmarkInfo(name: "ParseIntFromHex",
    runFunction: run_ParseIntFromHex,
    tags: [.validation, .api],
    setUpFunction: { blackHole(hexStrings) }),
  BenchmarkInfo(name: "ParseIntFromUncommonRadix",
    runFunction: run_ParseIntFromUncommonRadix,
    tags: [.validation, .api],
    setUpFunction: { blackHole(uncommonRadixStrings) }),
]

private let values: [Int] = Array(-1000...1000) // Give extra weight to low ints
  + (0..<2000).map { _ in Int.random(in: .min ... .max) }
private let uncommonRadix: Int = 7
private let decimalStrings: [String] = values.map { String($0, radix: 10) }
private let binaryStrings: [String] = values.map { String($0, radix: 2) }
private let hexStrings: [String] = values.map { String($0, radix: 16) }
private let uncommonRadixStrings: [String]
  = values.map { String($0, radix: uncommonRadix) }

@inline(never)
public func run_ParseIntFromDecimal(N: Int) {
  var result = 0
  for _ in 0..<N {
    for string in decimalStrings {
      result &+= Int(string, radix: 10)!
    }
  }
  blackHole(result)
}

@inline(never)
public func run_ParseIntFromBinary(N: Int) {
  var result = 0
  for _ in 0..<N {
    for string in binaryStrings {
      result &+= Int(string, radix: 2)!
    }
  }
  blackHole(result)
}

@inline(never)
public func run_ParseIntFromHex(N: Int) {
  var result = 0
  for _ in 0..<N {
    for string in hexStrings {
      result &+= Int(string, radix: 16)!
    }
  }
  blackHole(result)
}

@inline(never)
public func run_ParseIntFromUncommonRadix(N: Int) {
  var result = 0
  for _ in 0..<N {
    for string in uncommonRadixStrings {
      result &+= Int(string, radix: uncommonRadix)!
    }
  }
  blackHole(result)
}
