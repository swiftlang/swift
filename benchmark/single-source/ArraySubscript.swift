//===--- ArraySubscript.swift ---------------------------------------------===//
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

// This test checks the performance of modifying an array element.
import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "ArraySubscript",
    runFunction: run_ArraySubscript,
    tags: [.validation, .api, .Array],
    legacyFactor: 4)

@inline(never)
public func run_ArraySubscript(_ n: Int) {
  var lfsr = LFSR()

  let numArrays = 50
  let numArrayElements = 100

  func bound(_ x: Int) -> Int { return min(x, numArrayElements-1) }

  for _ in 1...n {
    var arrays = [[Int]](repeating: [], count: numArrays)
    for i in 0..<numArrays {
      for _ in 0..<numArrayElements {
        arrays[i].append(Int(truncatingIfNeeded: lfsr.next()))
      }
    }

    // Do a max up the diagonal.
    for i in 1..<numArrays {
      arrays[i][bound(i)] =
        max(arrays[i-1][bound(i-1)], arrays[i][bound(i)])
    }
    check(arrays[0][0] <= arrays[numArrays-1][bound(numArrays-1)])
  }
}
