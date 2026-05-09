//===--- BitCastTests.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift(-strict-memory-safety)
// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("bitCast Tests")
defer { runAllTests() }

suite.test("bitCast integer to integer")
.require(.stdlib_6_4).code {
  let u = 123_456_789
  let s = bitCast(u, to: UInt.self)
  expectEqual(s, UInt(bitPattern: u))

  let back = bitCast(s, to: Int.self)
  expectEqual(back, u)
}

suite.test("bitCast integer to float")
.require(.stdlib_6_4).code {
  let bits = 0x4048_f5c3 as UInt32 // bit pattern of ~3.14 as float
  let f = bitCast(bits, to: Float32.self)
  expectTrue(f > 3.13 && f < 3.15)

  let roundTrip = bitCast(f, to: UInt32.self)
  expectEqual(roundTrip, bits)
}

suite.test("bitCast UInt8 to Int8")
.require(.stdlib_6_4).code {
  let m = UInt8.max
  let s = bitCast(m, to: Int8.self)
  expectEqual(s, -1)
}

suite.test("bitCast UInt64 to Int64")
.require(.stdlib_6_4).code {
  let m = UInt64.max
  let s = bitCast(m, to: Int64.self)
  expectEqual(s, -1)
}

suite.test("bitCast size mismatch precondition")
.require(.crashTesting)
.require(.stdlib_6_4).code {
  let u = Int32.zero
  expectCrashLater()
  _ = bitCast(u, to: UInt64.self)
}
