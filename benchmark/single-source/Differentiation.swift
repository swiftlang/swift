//===--- Differentiation.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if canImport(_Differentiation)

import TestsUtils
import _Differentiation

public let benchmarks = [
  BenchmarkInfo(
    name: "DifferentiationIdentity",
    runFunction: run_DifferentiationIdentity,
    tags: [.regression, .differentiation]
  ),
  BenchmarkInfo(
    name: "DifferentiationSquare",
    runFunction: run_DifferentiationSquare,
    tags: [.regression, .differentiation]
  ),
  BenchmarkInfo(
    name: "DifferentiationArraySum",
    runFunction: run_DifferentiationArraySum,
    tags: [.regression, .differentiation],
    setUpFunction: { blackHole(onesArray) }
  ),
]

@inline(never)
public func run_DifferentiationIdentity(n: Int) {
  func f(_ x: Float) -> Float {
    x
  }
  for _ in 0..<1000*n {
    blackHole(valueWithGradient(at: 1, of: f))
  }
}

@inline(never)
public func run_DifferentiationSquare(n: Int) {
  func f(_ x: Float) -> Float {
    x * x
  }
  for _ in 0..<1000*n {
    blackHole(valueWithGradient(at: 1, of: f))
  }
}

let onesArray: [Float] = Array(repeating: 1, count: 50)

@inline(never)
public func run_DifferentiationArraySum(n: Int) {
  func sum(_ array: [Float]) -> Float {
    var result: Float = 0
    for i in withoutDerivative(at: 0..<array.count) {
      result += array[i]
    }
    return result
  }
  for _ in 0..<n {
    blackHole(valueWithGradient(at: onesArray, of: sum))
  }
}

#endif
