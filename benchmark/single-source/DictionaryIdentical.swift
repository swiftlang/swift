//===--- DictionaryIdentical.swift ----------------------------------------===//
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
  BenchmarkInfo(name: "DictionaryEqualUnique", runFunction: run_DictionaryEqualUnique, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionaryEqualShared", runFunction: run_DictionaryEqualShared, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionaryIdentical", runFunction: run_DictionaryIdentical, tags: [.validation, .api, .Dictionary]),
]

@inline(never)
public func run_DictionaryEqualUnique(_ n: Int) {
  let d1 = Dictionary(uniqueKeysWithValues: zip(1...n, 1...n))
  let d2 = Dictionary(uniqueKeysWithValues: zip(1...n, 1...n))
  for _ in 0 ..< 100_000 {
    check(d1 == d2)
  }
}

@inline(never)
public func run_DictionaryEqualShared(_ n: Int) {
  let d1 = Dictionary(uniqueKeysWithValues: zip(1...n, 1...n))
  let d2 = d1
  for _ in 0 ..< 100_000 {
    check(d1 == d2)
  }
}

@inline(never)
public func run_DictionaryIdentical(_ n: Int) {
  let d1 = Dictionary(uniqueKeysWithValues: zip(1...n, 1...n))
  let d2 = d1
  for _ in 0 ..< 100_000 {
    check(d1.isTriviallyIdentical(to: d2))
  }
}
