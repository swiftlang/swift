//===--- Join.swift -------------------------------------------------------===//
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

// This test tests the performance of ASCII Character comparison.
import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "Join",
    runFunction: run_Join,
    tags: [.validation, .api, .String, .Array])

@inline(never)
public func run_Join(_ n: Int) {
  var array: [String] = []
  for x in 0..<1000 * n {
    array.append(String(x))
  }
  _ = array.joined(separator: "")
  _ = array.joined(separator: " ")
}
