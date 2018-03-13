//===--- DictionaryCopy.swift ---------------------------------------------===//
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

// This benchmark checks for quadratic behavior while copying elements in hash
// order between Dictionaries of decreasing capacity
//
// https://bugs.swift.org/browse/SR-3268

import TestsUtils

public let DictionaryCopy = [
  BenchmarkInfo(name: "DictionaryCopy", runFunction: run_DictionaryCopy,
    tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionaryFilter", runFunction: run_DictionaryFilter,
    tags: [.validation, .api, .Dictionary]),
]

@inline(never)
public func testCopy(_ size: Int) {
  var dict1 = [Int: Int](minimumCapacity: size)

  // Fill dictionary
  for i in 1...size {
    dict1[i] = 2 * i
  }
  CheckResults(dict1.count == size)

  var dict2 = [Int: Int]()
  for (key, value) in dict1 {
    dict2[key] = value
  }
  CheckResults(dict2.count == size)
}

@inline(never)
public func run_DictionaryCopy(_ N: Int) {
  for _ in 0 ..< N {
    // We run the test at a spread of sizes between 1*x and 2*x, because the
    // quadratic behavior only happens at certain load factors.
    testCopy(100_000)
    testCopy(125_000)
    testCopy(150_000)
    testCopy(175_000)
  }
}

@inline(never)
public func testFilter(_ size: Int) {
  var dict1 = [Int: Int](minimumCapacity: size)

  // Fill dictionary
  for i in 1...size {
    dict1[i] = 2 * i
  }
  CheckResults(dict1.count == size)

  // Filter with a predicate returning true is essentially the same loop as the
  // one in testCopy above.
  let dict2 = dict1.filter { _ in true }
  CheckResults(dict2.count == size)
}

@inline(never)
public func run_DictionaryFilter(_ N: Int) {
  for _ in 0 ..< N {
    // We run the test at a spread of sizes between 1*x and 2*x, because the
    // quadratic behavior only happens at certain load factors.
    testFilter(100_000)
    testFilter(125_000)
    testFilter(150_000)
    testFilter(175_000)
  }
}
