//===--- SIMDInt.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let SIMDInt = [
  BenchmarkInfo(name: "SIMDIntAdd", runFunction: run_SIMDIntAdd, tags: [.validation, .api]),
  BenchmarkInfo(name: "SIMDIntSub", runFunction: run_SIMDIntSub, tags: [.validation, .api]),
  BenchmarkInfo(name: "SIMDIntMul", runFunction: run_SIMDIntMul, tags: [.validation, .api]),
  BenchmarkInfo(name: "SIMDIntDiv", runFunction: run_SIMDIntDiv, tags: [.validation, .api]),
  BenchmarkInfo(name: "SIMDIntRem", runFunction: run_SIMDIntRem, tags: [.validation, .api]),
  BenchmarkInfo(name: "SIMDIntShl", runFunction: run_SIMDIntShl, tags: [.validation, .api]),
  BenchmarkInfo(name: "SIMDIntShr", runFunction: run_SIMDIntShr, tags: [.validation, .api]),
  BenchmarkInfo(name: "SIMDIntBulkSum", runFunction: run_SIMDBulkSum, tags: [.validation, .api]),
]

@inline(never)
public func run_SIMDIntAdd(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &+ b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMDIntSub(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &- b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMDIntMul(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &* b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMDIntDiv(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a / b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMDIntRem(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a % b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMDIntShl(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &<< b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMDIntShr(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &>> b
  }
  blackHole(a)
}

let bulkSumInts = Array(repeating: SIMD4<Int32>(1, 2, 3, 4), count: 100)

@inline(never)
public func run_SIMDIntBulkSum(N: Int) {
  for _ in 0..<N {
    let ints = identity(bulkSumInts)
    let sum = ints.reduce(SIMD4<Int32>(), &+).wrappedSum()
    blackHole(sum)
  }
}
