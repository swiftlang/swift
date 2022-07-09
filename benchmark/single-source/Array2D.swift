//===--- Array2D.swift ----------------------------------------------------===//
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
    name: "Array2D",
    runFunction: run_Array2D,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(inputArray) },
    tearDownFunction: { inputArray = nil },
    legacyFactor: 16)

let size = 256

var inputArray: [[Int]]! = {
  var a: [[Int]] = []
  a.reserveCapacity(size)
  for _ in 0 ..< size {
    a.append(Array(0 ..< size))
  }
  return a
}()

@inline(never)
func modifyArray(_ a: inout [[Int]], _ n: Int) {
  for _ in 0..<n {
    for i in 0 ..< size {
      for y in 0 ..< size {
        a[i][y] = a[i][y] + 1
        a[i][y] = a[i][y] - 1
      }
    }
  }
}

@inline(never)
public func run_Array2D(_ n: Int) {
  modifyArray(&inputArray, n)
}
