//===--- Strideable.swift - Tests for strided iteration -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | FileCheck %s

import StdlibUnittest

var strideTestCase = TestCase("Strideable")

struct R : RandomAccessIndex {
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
}

func ==(a: R, b: R) -> Bool {
  return a.x == b.x
}

strideTestCase.test("Double") {
  // Doubles are not yet ready for testing, since they still conform
  // to RandomAccessIndex
}

strideTestCase.test("HalfOpen") {
  func check(from start: Int, to end: Int, by stepSize: Int, #sum: Int) {
    // Work on Ints
    expectEqual(
      sum,
      reduce(stride(from: start, to: end, by: stepSize), 0, +))

    // Work on an arbitrary RandomAccessIndex
    expectEqual(
      sum,
      reduce(stride(from: R(start), to: R(end), by: stepSize), 0){ $0 + $1.x })
  }
  
  check(from: 0, to: 14, by: 3, sum: 30)  // 0 + 3 + 6 + 9 + 12
  check(from: 0, to: 15, by: 3, sum: 30)  // 0 + 3 + 6 + 9 + 12
  check(from: 0, to: 16, by: 3, sum: 45)  // 0 + 3 + 6 + 9 + 12 + 15
  
  check(from: 1, to: -13, by: -3, sum: -25)  // 1 + -2 + -5 + -8 + -11
  check(from: 1, to: -14, by: -3, sum: -25)  // 1 + -2 + -5 + -8 + -11
  check(from: 1, to: -15, by: -3, sum: -39)  // 1 + -2 + -5 + -8 + -11
  
  check(from: 4, to: 16, by: -3, sum: 0)
  check(from: 1, to: -16, by: 3, sum: 0)
}

strideTestCase.run()
// CHECK: {{^}}Strideable: All tests passed
