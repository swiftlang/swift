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

import StdlibUnittest

var suite = TestSuite("Integer Division")

suite.test("Int8 division inbounds") {
  for b in -128 ... 127 {
    if b == 0 { continue }
    let lb: Int = (b < 0 ?  128 : -129) * b + 1
    let ub: Int = (b < 0 ? -129 :  128) * b - 1
    for a in lb ... ub {
      let high = Int8(a >> 8)
      let low = UInt8(a & 0xff)
      let (q, r) = Int8(b).dividingFullWidth(
        (high: high, low: low)
      )
      expectTrue(r.magnitude < b.magnitude)
      if r != 0 { expectEqual(r < 0, a < 0) }
      expectTrue(r == a - b*Int(q))
    }
  }
}

suite.test("UInt8 division inbounds") {
  for b in 0 ... 255 {
    if b == 0 { continue }
    for a in 0 ..< (b << 8) {
      let high = UInt8(a >> 8)
      let low = UInt8(a & 0xff)
      let (q, r) = UInt8(b).dividingFullWidth(
        (high: high, low: low)
      )
      expectTrue(r < b)
      expectTrue(r == a - b*Int(q))
    }
  }
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

suite.test("Int32 division inbounds") {
  let step: Int32 = 0x100_0000
  var g = WyRand(state: 0)
  for bhi in (-128 as Int32) ... 127 {
    let b = bhi << 24 | Int32.random(in: 0 ..< step, using: &g)
    for qhi in (-128 as Int32) ... 127 {
      let q = qhi << 24 | Int32.random(in: 0 ..< step, using: &g)
      let rmag = UInt32.random(in: 0 ..< b.magnitude, using: &g)
      let p = Int64(b)*Int64(q)
      let r = p < 0 ? -Int32(rmag) : Int32(rmag)
      let a = p + Int64(r)
      let high = Int32(a >> 32)
      let low = UInt32(a & 0xffff_ffff)
      let observed = b.dividingFullWidth((high: high, low: low))
      expectEqual(observed.quotient, q)
      expectEqual(observed.remainder, r)
    }
  }
}

suite.test("UInt32 division inbounds") {
  let step: UInt32 = 0x100_0000
  var g = WyRand(state: 0)
  for bhi in UInt32.zero ... 255 {
    let b = bhi << 24 | UInt32.random(in: 0 ..< step, using: &g)
    for qhi in UInt32.zero ... 255 {
      let q = qhi << 24 | UInt32.random(in: 0 ..< step, using: &g)
      let r = UInt32.random(in: 0 ..< b, using: &g)
      let a = UInt64(b)*UInt64(q) + UInt64(r)
      let high = UInt32(a >> 32)
      let low = UInt32(a & 0xffff_ffff)
      let observed = b.dividingFullWidth((high: high, low: low))
      expectEqual(observed.quotient, q)
      expectEqual(observed.remainder, r)
    }
  }
}

suite.test("Int64 division inbounds") {
  let step: Int64 = 0x100_0000_0000_0000
  var g = WyRand(state: 0)
  for bhi in (-128 as Int64) ... 127 {
    let b = bhi << 56 | Int64.random(in: 0 ..< step, using: &g)
    for qhi in (-128 as Int64) ... 127 {
      let q = qhi << 56 | Int64.random(in: 0 ..< step, using: &g)
      let rmag = UInt64.random(in: 0 ..< b.magnitude, using: &g)
      let p = b.multipliedFullWidth(by: q)
      let r = p.high < 0 ? -Int64(rmag) : Int64(rmag)
      let (low, c) =
        p.low.addingReportingOverflow(UInt64(truncatingIfNeeded: r))
      let high = p.high &+ (r >> 63) &+ (c ? 1 : 0)
      let observed = b.dividingFullWidth((high: high, low: low))
      expectEqual(observed.quotient, q)
      expectEqual(observed.remainder, r)
    }
  }
}

suite.test("UInt64 division inbounds") {
  let step: UInt64 = 0x100_0000_0000_0000
  var g = WyRand(state: 0)
  for bhi in UInt64.zero ... 255 {
    let b = bhi << 56 | UInt64.random(in: 0 ..< step, using: &g)
    for qhi in UInt64.zero ... 255 {
      let q = qhi << 56 | UInt64.random(in: 0 ..< step, using: &g)
      let r = UInt64.random(in: 0 ..< b, using: &g)
      let p = b.multipliedFullWidth(by: q)
      let (low, c) = p.low.addingReportingOverflow(r)
      let high = p.high &+ (c ? 1 : 0)
      let observed = b.dividingFullWidth((high: high, low: low))
      expectEqual(observed.quotient, q)
      expectEqual(observed.remainder, r)
    }
  }
}

runAllTests()

