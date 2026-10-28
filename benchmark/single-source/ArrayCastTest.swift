//===--- ArrayCastTest.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Measures `x is [Int]` where `x` is an `[Any]` whose elements are all Int
// except the last, which is a String.  The test must examine every element
// before it can fail, and it should not need to build a result array to do so.

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "ArrayCastTest.AnyToInt.Fail",
    runFunction: run_ArrayCastTestAnyToIntFail,
    tags: [.validation, .api, .Array, .runtime],
    setUpFunction: { blackHole(anyArray) }),
]

let anyArray: [Any] = {
  var a: [Any] = (0..<10).map { $0 as Any }
  a.append("not an Int")
  return a
}()

@inline(never)
func isIntArray(_ x: Any) -> Bool {
  return x is [Int]
}

@inline(never)
public func run_ArrayCastTestAnyToIntFail(_ n: Int) {
  let x = anyArray
  for _ in 0..<n {
    check(!isIntArray(identity(x)))
  }
}
