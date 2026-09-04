//===--- SIMDVectorIndexing.swift -----------------------------*- swift -*-===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let suite = TestSuite("SIMDVectorIndexing")
defer {
  runAllTests()
}

// A helper to build vectors where the lane value equals the lane index.
func laneVec<V: SIMD>(_: V.Type) -> V where V.Scalar: FixedWidthInteger {
  var result = V()
  for i in result.indices { result[i] = V.Scalar(truncatingIfNeeded: i) }
  return result
}

suite.test("indexing") {
  let v = laneVec(SIMD16<UInt8>.self)
  for i in 0 ..< 16 {
    expectEqual(
      SIMD4<UInt8>(repeating: UInt8(i)),
      v[SIMD4<Int>(repeating: i)]
    )
  }
}

suite.test("negative indices") {
  // A negative index -k where (0 < k <= scalarCount) means (scalarCount - k).
  let v = laneVec(SIMD16<UInt8>.self)
  for k in 1 ... 16 {
    expectEqual(
      SIMD2<UInt8>(repeating: UInt8(16 - k)),
      v[SIMD2<Int>(repeating: -k)]
    )
  }
  // Make sure that Int8.min also behaves correctly.
  expectEqual(
    SIMD2<UInt8>(repeating: 0),
    v[SIMD2<Int8>(repeating: Int8.min)]
  )
}

suite.test("indices over Int.max") {
  // Int(_:) traps on these; the subscript must not.
  let v = laneVec(SIMD16<UInt8>.self)
  expectEqual(
    SIMD2<UInt8>(repeating: 15),
    v[SIMD2<UInt64>(repeating: .max)]
  )
  expectEqual(
    SIMD2<UInt8>(repeating: 0),
    v[SIMD2<UInt64>(repeating: 1 << 63)]
  )
  expectEqual(
    SIMD2<UInt8>(repeating: 15),
    v[SIMD2<UInt>(repeating: .max)]
  )
}

suite.test("every Int8 index into 4 bits") {
  let v = laneVec(SIMD16<UInt8>.self)
  for raw in Int8.min ... Int8.max {
    let expected = UInt8(bitPattern: raw) & 15
    expectEqual(
      SIMD2<UInt8>(repeating: expected),
      v[SIMD2<Int8>(repeating: raw)],
      "index \(raw)"
    )
  }
}

suite.test("every Int8 index into 3 lanes") {
  // 3 is the case where masking would give the wrong answer.
  let v = laneVec(SIMD3<UInt8>.self)
  for raw in Int8.min ... Int8.max {
    let m = Int(raw) % 3
    let expected = UInt8(m < 0 ? m + 3 : m)
    expectEqual(
      SIMD2<UInt8>(repeating: expected),
      v[SIMD2<Int8>(repeating: raw)],
      "index \(raw)"
    )
  }
}

suite.test("result width follows the index width") {
  let v = laneVec(SIMD16<UInt8>.self)
  let i = SIMD8<Int>(0, 1, 2, 3, 4, 5, 6, 7)
  expectEqual(SIMD8<UInt8>(0, 1, 2, 3, 4, 5, 6, 7), v[i])
  expectEqual(2, v[SIMD2<Int>(repeating: 2)].scalarCount)
  expectEqual(3, v[SIMD3<Int>(repeating: 2)].scalarCount)
  expectEqual(4, v[SIMD4<Int>(repeating: 2)].scalarCount)
  expectEqual(8, v[SIMD8<Int>(repeating: 2)].scalarCount)
  expectEqual(16, v[SIMD16<Int>(repeating: 2)].scalarCount)
  expectEqual(32, v[SIMD32<Int>(repeating: 2)].scalarCount)
  expectEqual(64, v[SIMD64<Int>(repeating: 2)].scalarCount)
}

suite.test("no-trap spot check") {
  let v = laneVec(SIMD16<UInt8>.self)
  func check<Index: FixedWidthInteger & SIMDScalar>(_: Index.Type) {
    for raw in [Index.min, Index.max, 0, 1] {
      let lane = v[SIMD2<Index>(repeating: raw)][0]
      expectTrue(v.indices.contains(Int(lane)), "\(Index.self) \(raw)")
    }
  }
  check(Int8.self);  check(UInt8.self)
  check(Int16.self); check(UInt16.self)
  check(Int32.self); check(UInt32.self)
  check(Int64.self); check(UInt64.self)
  check(Int.self);   check(UInt.self)
}
