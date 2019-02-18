//===--- DictionaryCompactMapValues.swift ---------------------------------===//
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

let size = 100
let oddNumbers = stride(from: 1, to: size, by: 2)
let smallOddNumMap: [Int: Int?] =
  Dictionary(uniqueKeysWithValues: zip(oddNumbers, oddNumbers))
let compactOddNums: [Int: Int] =
  Dictionary(uniqueKeysWithValues: zip(oddNumbers, oddNumbers))
let oddStringMap: [Int: String] = Dictionary(uniqueKeysWithValues:
  (1...size).lazy.map { ($0, $0 % 2 == 0 ? "dummy" : "\($0)") })

let t: [BenchmarkCategory] = [.validation, .api, .Dictionary]

public let DictionaryCompactMapValues = [
  BenchmarkInfo(name: "DictionaryCompactMapValuesOfNilValue",
    runFunction: { for _ in 1...$0*20 {
      CheckResults(smallOddNumMap.compactMapValues({$0}) == compactOddNums) }},
    tags: t, legacyFactor: 50),
  BenchmarkInfo(name: "DictionaryCompactMapValuesOfCastValue",
    runFunction: { for _ in 1...$0*20 {
      CheckResults(oddStringMap.compactMapValues(Int.init) == compactOddNums) }
    }, tags: t, legacyFactor: 54),
]
