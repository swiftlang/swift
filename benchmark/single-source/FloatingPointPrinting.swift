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
    name: "FloatingPointPrinting_Float_description_small",
    runFunction: run_FloatingPointPrinting_Float_description_small,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Double_description_small",
    runFunction: run_FloatingPointPrinting_Double_description_small,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Float80_description_small",
    runFunction: run_FloatingPointPrinting_Float80_description_small,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Float_description_uniform",
    runFunction: run_FloatingPointPrinting_Float_description_uniform,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Double_description_uniform",
    runFunction: run_FloatingPointPrinting_Double_description_uniform,
    tags: [.validation, .api, .runtime, .String]),

  BenchmarkInfo(
    name: "FloatingPointPrinting_Float80_description_uniform",
    runFunction: run_FloatingPointPrinting_Float80_description_uniform,
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

// Generate descriptions for 100,000 values around 1.0.
//
// Note that some formatting algorithms behave very
// differently for values around 1.0 than they do for
// less-common extreme values.  Having a "small" test
// and a "uniform" test exercises both cases.
//
// Dividing integers 1...100000 by 101 (a prime) yields floating-point
// values from about 1e-2 to about 1e3, each with plenty of digits after
// the decimal:

@inline(never)
public func run_FloatingPointPrinting_Float_description_small(_ N: Int) {
  let count = 100_000
  for _ in 0..<N {
    for i in 1...count {
      let f = Float(i) / 101.0
      blackHole(f.description)
    }
  }
}

@inline(never)
public func run_FloatingPointPrinting_Double_description_small(_ N: Int) {
  let count = 100_000
  for _ in 0..<N {
    for i in 1...count {
      let f = Double(i) / 101.0
      blackHole(f.description)
    }
  }
}

@inline(never)
public func run_FloatingPointPrinting_Float80_description_small(_ N: Int) {
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) || os(Linux)
// On Darwin, long double is Float80 on x86, and Double otherwise.
// On Linux, Float80 is at aleast available on x86.
#if arch(x86_64) || arch(i386)
  let count = 100_000
  for _ in 0..<N {
    for i in 1...count {
      let f = Float80(i) / 101.0
      blackHole(f.description)
    }
  }
#endif // x86
#endif // Darwin/Linux
}

// Generate descriptions for 100,000 values spread evenly across
// the full range of the type:

@inline(never)
public func run_FloatingPointPrinting_Float_description_uniform(_ N: Int) {
  let count = 100_000
  let step = UInt32.max / UInt32(count)
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt32(i) * step
      let f = Float(bitPattern: raw)
      blackHole(f.description)
    }
  }
}

@inline(never)
public func run_FloatingPointPrinting_Double_description_uniform(_ N: Int) {
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt64(i) * step
      let f = Double(bitPattern: raw)
      blackHole(f.description)
    }
  }
}

@inline(never)
public func run_FloatingPointPrinting_Float80_description_uniform(_ N: Int) {
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) || os(Linux)
// On Darwin, long double is Float80 on x86, and Double otherwise.
// On Linux, Float80 is at aleast available on x86.
#if arch(x86_64) || arch(i386)
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  for _ in 0..<N {
    for i in 0..<count {
      let fraction = UInt64(i) * step
      let exponent = UInt(i) % 32768
      let f = Float80(sign: .plus, exponentBitPattern: exponent, significandBitPattern: fraction)
      blackHole(f.description)
    }
  }
#endif // x86
#endif // Darwin/Linux
}

// The "interpolated" tests verify that any storage optimizations used while
// producing the formatted numeric strings don't pessimize later use of the
// result.

@inline(never)
public func run_FloatingPointPrinting_Float_interpolated(_ N: Int) {
  let count = 100_000
  let step = UInt32.max / UInt32(count)
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt32(i) * step
      let f = Float(bitPattern: raw)
      blackHole("and the actual result was \(f)")
    }
  }
}

@inline(never)
public func run_FloatingPointPrinting_Double_interpolated(_ N: Int) {
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  for _ in 0..<N {
    for i in 0..<count {
      let raw = UInt64(i) * step
      let f = Double(bitPattern: raw)
      blackHole("and the actual result was \(f)")
    }
  }
}

@inline(never)
public func run_FloatingPointPrinting_Float80_interpolated(_ N: Int) {
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) || os(Linux)
// On Darwin, long double is Float80 on x86, and Double otherwise.
// On Linux, Float80 is at aleast available on x86.
#if arch(x86_64) || arch(i386)
  let count = 100_000
  let step = UInt64.max / UInt64(count)
  for _ in 0..<N {
    for i in 0..<count {
      let fraction = UInt64(i) * step
      let exponent = UInt(i) % 32768
      let f = Float80(sign: .plus, exponentBitPattern: exponent, significandBitPattern: fraction)
      blackHole("and the actual result was \(f)")
    }
  }
#endif // x86
#endif // Darwin/Linux
}

