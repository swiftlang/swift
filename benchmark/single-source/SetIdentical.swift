//===--- SetIdentical.swift -----------------------------------------------===//
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
  BenchmarkInfo(name: "SetEqualUnique", runFunction: run_SetEqualUnique, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetEqualShared", runFunction: run_SetEqualShared, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetIdentical", runFunction: run_SetIdentical, tags: [.validation, .api, .Set]),
]

@inline(never)
public func run_SetEqualUnique(_ n: Int) {
  let s1 = Set(1...n)
  let s2 = Set(1...n)
  for _ in 0 ..< 100_000 {
    check(s1 == s2)
  }
}

@inline(never)
public func run_SetEqualShared(_ n: Int) {
  let s1 = Set(1...n)
  let s2 = s1
  for _ in 0 ..< 100_000 {
    check(s1 == s2)
  }
}

@inline(never)
public func run_SetIdentical(_ n: Int) {
  let s1 = Set(1...n)
  let s2 = s1
  for _ in 0 ..< 100_000 {
    check(s1.isTriviallyIdentical(to: s2))
  }
}
