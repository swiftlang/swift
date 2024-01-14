//===--- IntegerDivision.swift.gyb ----------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: long_test
// UNSUPPORTED: use_os_stdlib

import StdlibUnittest

var suite = TestSuite("Integer Division Traps")

suite.test("Int8 division lower bound")
  .forEach(in: Array(-128 ... 127)) { b in
    if b == 0 { return }
    let boundary: Int = (b < 0 ?  128 : -129) * b
    let high = Int8(boundary >> 8)
    let low = UInt8(boundary & 0xff)
    expectCrashLater()
    let (q, _) = Int8(b).dividingFullWidth(
      (high: high, low: low)
    )
    _blackHole(q)
  }

suite.test("Int8 division upper bound")
  .forEach(in: Array(-128 ... 127)) { b in
    if b == 0 { return }
    let boundary: Int = (b < 0 ? -129 :  128) * b
    let high = Int8(boundary >> 8)
    let low = UInt8(boundary & 0xff)
    expectCrashLater()
    let (q, _) = Int8(b).dividingFullWidth(
      (high: high, low: low)
    )
    _blackHole(q)
  }

// Dead-simple deterministic random source to ensure that we always test
// the same "random" values.
struct WyRand: RandomNumberGenerator {
  var state: UInt64
  mutating func next() -> UInt64 {
    state &+= 0xa076_1d64_78bd_642f
    let p = state.multipliedFullWidth(by: state ^ 0xe703_7ed1_a0b4_28db)
    return p.high ^ p.low
  }
}

suite.test("Int32 division lower bound")
  .forEach(in: Array((-128 as Int32) ... 127)) { bhi in
    var g = WyRand(state: UInt64(truncatingIfNeeded: bhi))
    let b = bhi << 24 | Int32.random(in: 0 ..< 0x100_0000, using: &g)
    let boundary = (b < 0 ? 0x1_0000_0000 : -0x1_0000_0001) * Int64(b)
    let high = Int32(boundary >> 32)
    let low = UInt32(boundary & 0xffff_ffff)
    expectCrashLater()
    let (q, _) = b.dividingFullWidth(
      (high: high, low: low)
    )
    _blackHole(q)
  }

suite.test("Int32 division upper bound")
  .forEach(in: Array((-128 as Int32) ... 127)) { bhi in
    var g = WyRand(state: UInt64(truncatingIfNeeded: bhi))
    let b = bhi << 24 | Int32.random(in: 0 ..< 0x100_0000, using: &g)
    let boundary = (b < 0 ? -0x1_0000_0001 : -0x1_0000_0000) * Int64(b)
    let high = Int32(boundary >> 32)
    let low = UInt32(boundary & 0xffff_ffff)
    expectCrashLater()
    let (q, _) = b.dividingFullWidth(
      (high: high, low: low)
    )
    _blackHole(q)
  }

suite.test("UInt32 division upper bound")
  .forEach(in: Array(UInt32.zero ... 255)) { bhi in
    var g = WyRand(state: UInt64(truncatingIfNeeded: bhi))
    let b = bhi << 24 | UInt32.random(in: 0 ..< 0x100_0000, using: &g)
    expectCrashLater()
    let (q, _) = b.dividingFullWidth(
      (high: b, low: UInt32.random(in: 0 ... .max, using: &g))
    )
    _blackHole(q)
  }

suite.test("UInt64 division upper bound")
  .forEach(in: Array(UInt64.zero ... 255)) { bhi in
    var g = WyRand(state: UInt64(truncatingIfNeeded: bhi))
    let b = bhi << 56 | UInt64.random(in: 0 ..< 0x100_0000_0000_0000, using: &g)
    expectCrashLater()
    let (q, _) = b.dividingFullWidth(
      (high: b, low: UInt64.random(in: 0 ... .max, using: &g))
    )
    _blackHole(q)
  }

runAllTests()

