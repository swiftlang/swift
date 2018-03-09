//===--- FloatingPointPrinting.swift -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test verifies the performance of generating a text description
// from a binary floating-point value.

import TestsUtils

public let FloatingPointPrinting = [
  BenchmarkInfo(
    name: "FloatingPointPrinting_Float_description",
    runFunction: run_FloatingPointPrinting_Float_description,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Double_description",
    runFunction: run_FloatingPointPrinting_Double_description,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Float80_description",
    runFunction: run_FloatingPointPrinting_Float80_description,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Float_interpolated",
    runFunction: run_FloatingPointPrinting_Float_interpolated,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Double_interpolated",
    runFunction: run_FloatingPointPrinting_Double_interpolated,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Float80_interpolated",
    runFunction: run_FloatingPointPrinting_Float80_interpolated,
    tags: [.validation, .api, .runtime, .String])
]

// Description test just generates description for 100,000
// values across a range of bit patterns.

@inline(never)
public func run_FloatingPointPrinting_Float_description(_ N: Int) {
  let count = 100_000
  let step = UInt32.max / UInt32(count)
  var s = ""
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt32(i) * step
      let f = Float(bitPattern: raw)
      s = f.description
    }
  }
  CheckResults(s.count > 1)
}

@inline(never)
public func run_FloatingPointPrinting_Double_description(_ N: Int) {
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  var s = ""
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt64(i) * step
      let f = Double(bitPattern: raw)
      s = f.description
    }
  }
  CheckResults(s.count > 1)
}

@inline(never)
public func run_FloatingPointPrinting_Float80_description(_ N: Int) {
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  var s = ""
  for _ in 0..<N {
    for i in 0..<count {
      let fraction = UInt64(i) * step
      let exponent = UInt(i) % 32768
      let f = Float80(sign: .plus, exponentBitPattern: exponent, significandBitPattern: fraction)
      s = f.description
    }
  }
  CheckResults(s.count > 1)
}

// The "interpolated" tests verify that any storage optimizations used while
// producing the formatted numeric strings don't pessimize later use of the
// result.

@inline(never)
public func run_FloatingPointPrinting_Float_interpolated(_ N: Int) {
  let count = 100_000
  let step = UInt32.max / UInt32(count)
  var s = ""
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt32(i) * step
      let f = Float(bitPattern: raw)
      s = "result was \(f)"
    }
  }
  CheckResults(s.count > 1)
}

@inline(never)
public func run_FloatingPointPrinting_Double_interpolated(_ N: Int) {
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  var s = ""
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt64(i) * step
      let f = Double(bitPattern: raw)
      s = "result was \(f)"
    }
  }
  CheckResults(s.count > 1)
}

@inline(never)
public func run_FloatingPointPrinting_Float80_interpolated(_ N: Int) {
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  var s = ""
  for _ in 0..<N {
    for i in 0..<count {
      let fraction = UInt64(i) * step
      let exponent = UInt(i) % 32768
      let f = Float80(sign: .plus, exponentBitPattern: exponent, significandBitPattern: fraction)
      s = "result was \(f)"
    }
  }
  CheckResults(s.count > 1)
}

