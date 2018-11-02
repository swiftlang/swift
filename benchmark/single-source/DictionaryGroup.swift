//===--- DictionaryGroup.swift --------------------------------------------===//
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

import TestsUtils

public let DictionaryGroup = [
  BenchmarkInfo(name: "DictionaryGroup", runFunction: run_DictionaryGroup, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionaryGroupOfObjects", runFunction: run_DictionaryGroupOfObjects, tags: [.validation, .api, .Dictionary]),
]

let count = 10_000
let result = count / 10

@inline(never)
public func run_DictionaryGroup(_ N: Int) {
  for _ in 1...N {
    let dict = Dictionary(grouping: 0..<count, by: { $0 % 10 })
    CheckResults(dict.count == 10)
    CheckResults(dict[0]!.count == result)
  }
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  static func ==(lhs: Box, rhs: Box) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
public func run_DictionaryGroupOfObjects(_ N: Int) {
  let objects = (0..<count).map { Box($0) }
  for _ in 1...N {
    let dict = Dictionary(grouping: objects, by: { Box($0.value % 10) })
    CheckResults(dict.count == 10)
    CheckResults(dict[Box(0)]!.count == result)
  }
}
