//===--- LazyFilter.swift -------------------------------------------------===//
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

// This test checks performance of creating an array from lazily filtered
// collections.
import TestsUtils

public let LazyFilter = [
  BenchmarkInfo(name: "LazilyFilteredArrays2", runFunction: run_LazilyFilteredArrays, tags: [.validation, .api, .Array],
      setUpFunction: { blackHole(filteredRange) }),
  BenchmarkInfo(name: "LazilyFilteredRange", runFunction: run_LazilyFilteredRange, tags: [.validation, .api, .Array]),
  BenchmarkInfo(
    name: "LazilyFilteredArrayContains",
    runFunction: run_LazilyFilteredArrayContains,
    tags: [.validation, .api, .Array],
    setUpFunction: setup_LazilyFilteredArrayContains,
    tearDownFunction: teardown_LazilyFilteredArrayContains),
]

@inline(never)
public func run_LazilyFilteredRange(_ N: Int) {
  var res = 123
  let c = (1..<1_000_000).lazy.filter { $0 % 7 == 0 }
  for _ in 1...N {
    res += Array(c).count
    res -= Array(c).count
  }
  CheckResults(res == 123)
}

let filteredRange = (1..<100_000).map({[$0]}).lazy.filter { $0.first! % 7 == 0 }

@inline(never)
public func run_LazilyFilteredArrays(_ N: Int) {
  var res = 123
  let c = filteredRange
  for _ in 1...N {
    res += Array(c).count
    res -= Array(c).count
  }
  CheckResults(res == 123)
}

fileprivate var multiplesOfThree: LazyFilterCollection<Array<Int>>?

fileprivate func setup_LazilyFilteredArrayContains() {
  multiplesOfThree = Array(1..<5_000).lazy.filter { $0 % 3 == 0 }
}

fileprivate func teardown_LazilyFilteredArrayContains() {
  multiplesOfThree = nil
}

@inline(never)
fileprivate func run_LazilyFilteredArrayContains(_ N: Int) {
  let xs = multiplesOfThree!
  for _ in 1...N {
    var filteredCount = 0
    for candidate in 1..<5_000 {
      filteredCount += xs.contains(candidate) ? 1 : 0
    }
    CheckResults(filteredCount == 1666)
  }
}
