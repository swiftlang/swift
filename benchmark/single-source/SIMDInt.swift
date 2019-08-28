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

let t: [BenchmarkCategory] = [.validation, .api, .simd]

public let SIMDInt = [
  BenchmarkInfo(name: "SIMD4Int32.add",     runFunction: run_SIMD4Int32Add,     tags: t),
  BenchmarkInfo(name: "SIMD4Int32.sub",     runFunction: run_SIMD4Int32Sub,     tags: t),
  BenchmarkInfo(name: "SIMD4Int32.mul",     runFunction: run_SIMD4Int32Mul,     tags: t),
  BenchmarkInfo(name: "SIMD4Int32.div",     runFunction: run_SIMD4Int32Div,     tags: t),
  BenchmarkInfo(name: "SIMD4Int32.rem",     runFunction: run_SIMD4Int32Rem,     tags: t),
  BenchmarkInfo(name: "SIMD4Int32.shl",     runFunction: run_SIMD4Int32Shl,     tags: t),
  BenchmarkInfo(name: "SIMD4Int32.shr",     runFunction: run_SIMD4Int32Shr,     tags: t),
  BenchmarkInfo(name: "SIMD4Int32.bulkSum", runFunction: run_SIMD4Int32BulkSum, tags: t,
                setUpFunction: { blackHole(bulkSumInts) }),
]

@inline(never)
public func run_SIMD4Int32Add(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &+ b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMD4Int32Sub(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &- b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMD4Int32Mul(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &* b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMD4Int32Div(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a / b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMD4Int32Rem(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a % b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMD4Int32Shl(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &<< b
  }
  blackHole(a)
}

@inline(never)
public func run_SIMD4Int32Shr(N: Int) {
  var a = identity(SIMD4<Int32>(1, 2, 3, 4))
  let b = identity(SIMD4<Int32>(5, 6, 7, 8))
  for _ in 0..<N {
    a = a &>> b
  }
  blackHole(a)
}

// The wrapped sum of these integers is expected to be 1000
let bulkSumInts = Array(repeating: SIMD4<Int32>(1, 2, 3, 4), count: 100)

@inline(never)
public func run_SIMD4Int32BulkSum(N: Int) {
  for _ in 0..<N {
    let ints = identity(bulkSumInts)
    let sum = ints.reduce(SIMD4<Int32>(), &+).wrappedSum()
    CheckResults(sum == 1000)
  }
}
