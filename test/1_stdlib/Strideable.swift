//===--- Strideable.swift - Tests for strided iteration -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// XFAIL: interpret

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

// Check that the generic parameter is called 'Element'.
protocol TestProtocol1 {}

extension StrideToGenerator where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension StrideTo where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension StrideThroughGenerator where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension StrideThrough where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

var StrideTestSuite = TestSuite("Strideable")

struct R : RandomAccessIndexType {
  typealias Distance = Int
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func successor() -> R {
    return R(x + 1)
  }
  func predecessor() -> R {
    return R(x - 1)
  }
  func distanceTo(rhs: R) -> Int {
    return rhs.x - x
  }
  func advancedBy(n: Int) -> R {
    return R(x + n)
  }
  func advancedBy(n: Int, limit: R) -> R {
    let d = distanceTo(limit)
    if d == 0 || (d > 0 ? d <= n : d >= n) {
      return limit
    }
    return self.advancedBy(n)
  }
}

StrideTestSuite.test("Double") {
  // Doubles are not yet ready for testing, since they still conform
  // to RandomAccessIndexType
}

StrideTestSuite.test("HalfOpen") {
  func check(from start: Int, to end: Int, by stepSize: Int, sum: Int) {
    // Work on Ints
    expectEqual(
      sum,
      start.stride(to: end, by: stepSize).reduce(0, combine: +))

    // Work on an arbitrary RandomAccessIndexType
    expectEqual(
      sum,
      R(start).stride(to: R(end), by: stepSize).reduce(0) { $0 + $1.x })
  }
  
  check(from: 1, to: 15, by: 3, sum: 35)  // 1 + 4 + 7 + 10 + 13
  check(from: 1, to: 16, by: 3, sum: 35)  // 1 + 4 + 7 + 10 + 13
  check(from: 1, to: 17, by: 3, sum: 51)  // 1 + 4 + 7 + 10 + 13 + 16
  
  check(from: 1, to: -13, by: -3, sum: -25)  // 1 + -2 + -5 + -8 + -11
  check(from: 1, to: -14, by: -3, sum: -25)  // 1 + -2 + -5 + -8 + -11
  check(from: 1, to: -15, by: -3, sum: -39)  // 1 + -2 + -5 + -8 + -11 + -14
  
  check(from: 4, to: 16, by: -3, sum: 0)
  check(from: 1, to: -16, by: 3, sum: 0)
}

StrideTestSuite.test("Closed") {
  func check(from start: Int, through end: Int, by stepSize: Int, sum: Int) {
    // Work on Ints
    expectEqual(
      sum,
      start.stride(through: end, by: stepSize).reduce(0, combine: +))

    // Work on an arbitrary RandomAccessIndexType
    expectEqual(
      sum,
      R(start).stride(through: R(end), by: stepSize).reduce(0) { $0 + $1.x })
  }
  
  check(from: 1, through: 15, by: 3, sum: 35)  // 1 + 4 + 7 + 10 + 13
  check(from: 1, through: 16, by: 3, sum: 51)  // 1 + 4 + 7 + 10 + 13 + 16
  check(from: 1, through: 17, by: 3, sum: 51)  // 1 + 4 + 7 + 10 + 13 + 16
  
  check(from: 1, through: -13, by: -3, sum: -25) // 1 + -2 + -5 + -8 + -11
  check(from: 1, through: -14, by: -3, sum: -39) // 1 + -2 + -5 + -8 + -11 + -14
  check(from: 1, through: -15, by: -3, sum: -39) // 1 + -2 + -5 + -8 + -11 + -14
  
  check(from: 4, through: 16, by: -3, sum: 0)
  check(from: 1, through: -16, by: 3, sum: 0)
}

StrideTestSuite.test("OperatorOverloads") {
  var r1 = R(50)
  var r2 = R(70)
  var stride: Int = 5

  if true {
    var result = r1 + stride
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  if true {
    var result = stride + r1
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  if true {
    var result = r1 - stride
    expectType(R.self, &result)
    expectEqual(45, result.x)
  }
  if true {
    var result = r1 - r2
    expectType(Int.self, &result)
    expectEqual(-20, result)
  }
  if true {
    var result = r1
    result += stride
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  if true {
    var result = r1
    result -= stride
    expectType(R.self, &result)
    expectEqual(45, result.x)
  }
}

runAllTests()

