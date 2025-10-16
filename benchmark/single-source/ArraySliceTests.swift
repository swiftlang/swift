//===--- ArraySliceTests.swift --------------------------------------------===//
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
  BenchmarkInfo(name: "ArraySliceEqualUnique", runFunction: run_ArraySliceEqualUnique, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArraySliceEqualShared", runFunction: run_ArraySliceEqualShared, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArraySliceIdentical", runFunction: run_ArraySliceIdentical, tags: [.validation, .api, .Array]),
]

@inline(never)
public func run_ArraySliceEqualUnique(_ n: Int) {
  let a1 = ArraySlice(0 ..< n)
  let a2 = ArraySlice(0 ..< n)
  for _ in 0 ..< 100_000 {
    check(a1 == a2)
  }
}

@inline(never)
public func run_ArraySliceEqualShared(_ n: Int) {
  let a1 = ArraySlice(0 ..< n)
  let a2 = a1
  for _ in 0 ..< 100_000 {
    check(a1 == a2)
  }
}

@inline(never)
public func run_ArraySliceIdentical(_ n: Int) {
  let a1 = ArraySlice(0 ..< n)
  let a2 = a1
  for _ in 0 ..< 100_000 {
    check(a1.isTriviallyIdentical(to: a2))
  }
}
