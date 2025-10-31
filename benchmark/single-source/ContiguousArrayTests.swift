//===--- ContiguousArrayTests.swift ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "ContiguousArrayEqualUnique", runFunction: run_ContiguousArrayEqualUnique, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ContiguousArrayEqualShared", runFunction: run_ContiguousArrayEqualShared, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ContiguousArrayIdentical", runFunction: run_ContiguousArrayIdentical, tags: [.validation, .api, .Array]),
]

@inline(never)
public func run_ContiguousArrayEqualUnique(_ n: Int) {
  let a1 = ContiguousArray(0 ..< n)
  let a2 = ContiguousArray(0 ..< n)
  for _ in 0 ..< 100_000 {
    check(a1 == a2)
  }
}

@inline(never)
public func run_ContiguousArrayEqualShared(_ n: Int) {
  let a1 = ContiguousArray(0 ..< n)
  let a2 = a1
  for _ in 0 ..< 100_000 {
    check(a1 == a2)
  }
}

@inline(never)
public func run_ContiguousArrayIdentical(_ n: Int) {
  let a1 = ContiguousArray(0 ..< n)
  let a2 = a1
  for _ in 0 ..< 100_000 {
    check(a1.isTriviallyIdentical(to: a2))
  }
}
