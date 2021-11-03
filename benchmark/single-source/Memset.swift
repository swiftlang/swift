//===--- Memset.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "Memset",
    runFunction: run_Memset,
    tags: [.validation, .unstable])

@inline(never)
func memset(_ a: inout [Int], _ c: Int) {
  for i in 0..<a.count {
    a[i] = c
  }
}

@inline(never)
public func run_Memset(_ n: Int) {
  var a = [Int](repeating: 0, count: 10_000)
  for _ in 1...50*n {
    memset(&a, 1)
    memset(&a, 0)
  }
  check(a[87] == 0)
}
