//===--- SIMDRandomMask.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let SIMDRandomMask = [
  BenchmarkInfo(
    name: "SIMDRandomMask.Int8x16",
    runFunction: run_SIMDRandomMaskInt8x16,
    tags: [.validation, .SIMD]
  ),
  BenchmarkInfo(
    name: "SIMDRandomMask.Int8x64",
    runFunction: run_SIMDRandomMaskInt8x64,
    tags: [.validation, .SIMD]
  ),
  BenchmarkInfo(
    name: "SIMDRandomMask.Int64x2",
    runFunction: run_SIMDRandomMaskInt64x2,
    tags: [.validation, .SIMD]
  ),
  BenchmarkInfo(
    name: "SIMDRandomMask.Int64x8",
    runFunction: run_SIMDRandomMaskInt64x8,
    tags: [.validation, .SIMD]
  ),
  BenchmarkInfo(
    name: "SIMDRandomMask.Int64x64",
    runFunction: run_SIMDRandomMaskInt64x64,
    tags: [.validation, .SIMD]
  )
]

@inline(never)
public func run_SIMDRandomMaskInt8x16(_ N: Int) {
  var g = SplitMix64(seed: 0)
  var accum = SIMDMask<SIMD16<Int8>>()
  for _ in 0 ..< 10000*N {
    accum .^= SIMDMask.random(using: &g)
  }
  blackHole(accum)
}

@inline(never)
public func run_SIMDRandomMaskInt8x64(_ N: Int) {
  var g = SplitMix64(seed: 0)
  var accum = SIMDMask<SIMD64<Int8>>()
  for _ in 0 ..< 10000*N {
    accum .^= SIMDMask.random(using: &g)
  }
  blackHole(accum)
}

@inline(never)
public func run_SIMDRandomMaskInt64x2(_ N: Int) {
  var g = SplitMix64(seed: 0)
  var accum = SIMDMask<SIMD2<Int64>>()
  for _ in 0 ..< 10000*N {
    accum .^= SIMDMask.random(using: &g)
  }
  blackHole(accum)
}

@inline(never)
public func run_SIMDRandomMaskInt64x8(_ N: Int) {
  var g = SplitMix64(seed: 0)
  var accum = SIMDMask<SIMD8<Int64>>()
  for _ in 0 ..< 10000*N {
    accum .^= SIMDMask.random(using: &g)
  }
  blackHole(accum)
}

@inline(never)
public func run_SIMDRandomMaskInt64x64(_ N: Int) {
  var g = SplitMix64(seed: 0)
  var accum = SIMDMask<SIMD64<Int64>>()
  for _ in 0 ..< 10000*N {
    accum .^= SIMDMask.random(using: &g)
  }
  blackHole(accum)
}
 
