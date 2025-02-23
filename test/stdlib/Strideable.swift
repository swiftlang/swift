//===--- Strideable.swift - Tests for strided iteration -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//

import StdlibUnittest


// Check that the generic parameter is called 'Element'.
protocol TestProtocol1 {}

extension StrideToIterator where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension StrideTo where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension StrideThroughIterator where Element : TestProtocol1 {
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

struct R : Strideable {
  typealias Distance = Int
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func distance(to rhs: R) -> Int {
    return rhs.x - x
  }
  func advanced(by n: Int) -> R {
    return R(x + n)
  }
}

StrideTestSuite.test("Double") {
  func checkOpen(from start: Double, to end: Double, by stepSize: Double, sum: Double) {
    // Work on Doubles
    expectEqual(
      sum,
      stride(from: start, to: end, by: stepSize).reduce(0.0, +))
  }

  func checkClosed(from start: Double, through end: Double, by stepSize: Double, sum: Double) {
    // Work on Doubles
    expectEqual(
      sum,
      stride(from: start, through: end, by: stepSize).reduce(0.0, +))
  }

  checkOpen(from: 1.0, to: 15.0, by: 3.0, sum: 35.0)
  checkOpen(from: 1.0, to: 16.0, by: 3.0, sum: 35.0)
  checkOpen(from: 1.0, to: 17.0, by: 3.0, sum: 51.0)

  checkOpen(from: 1.0, to: -13.0, by: -3.0, sum: -25.0)
  checkOpen(from: 1.0, to: -14.0, by: -3.0, sum: -25.0)
  checkOpen(from: 1.0, to: -15.0, by: -3.0, sum: -39.0)

  checkOpen(from: 4.0, to: 16.0, by: -3.0, sum: 0.0)
  checkOpen(from: 1.0, to: -16.0, by: 3.0, sum: 0.0)

  checkClosed(from: 1.0, through: 15.0, by: 3.0, sum: 35.0)
  checkClosed(from: 1.0, through: 16.0, by: 3.0, sum: 51.0)
  checkClosed(from: 1.0, through: 17.0, by: 3.0, sum: 51.0)

  checkClosed(from: 1.0, through: -13.0, by: -3.0, sum: -25.0)
  checkClosed(from: 1.0, through: -14.0, by: -3.0, sum: -39.0)
  checkClosed(from: 1.0, through: -15.0, by: -3.0, sum: -39.0)

  checkClosed(from: 4.0, through: 16.0, by: -3.0, sum: 0.0)
  checkClosed(from: 1.0, through: -16.0, by: 3.0, sum: 0.0)
}

StrideTestSuite.test("HalfOpen") {
  func check(from start: Int, to end: Int, by stepSize: Int, sum: Int) {
    // Work on Ints
    expectEqual(
      sum,
      stride(from: start, to: end, by: stepSize).reduce(
        0, +))

    // Work on an arbitrary RandomAccessIndex
    expectEqual(
      sum,
      stride(from: R(start), to: R(end), by: stepSize).reduce(0) { $0 + $1.x })
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
      stride(from: start, through: end, by: stepSize).reduce(
        0, +))

    // Work on an arbitrary RandomAccessIndex
    expectEqual(
      sum,
      stride(from: R(start), through: R(end), by: stepSize).reduce(
        0, { $0 + $1.x })
    )
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

  do {
    var result = r1.advanced(by: stride)
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  do {
    var result = r1.advanced(by: stride)
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  do {
    var result = r1.advanced(by: -stride)
    expectType(R.self, &result)
    expectEqual(45, result.x)
  }
  do {
    var result = r2.distance(to: r1)
    expectType(Int.self, &result)
    expectEqual(-20, result)
  }
}

StrideTestSuite.test("FloatingPointStride") {
  var result = [Double]()
  for i in stride(from: 1.4, through: 3.4, by: 1) {
    result.append(i)
  }
  expectEqual([ 1.4, 2.4, 3.4 ], result)
}

StrideTestSuite.test("FloatingPointStride/rounding error") {
  // Ensure that there is no error accumulation
  let a = Array(stride(from: 1 as Float, through: 2, by: 0.1))
  expectEqual(11, a.count)
  expectEqual(2 as Float, a.last)
  let b = Array(stride(from: 1 as Float, to: 10, by: 0.9))
  expectEqual(10, b.count)

  // Ensure that there is no intermediate rounding error on supported platforms
  if (-0.2).addingProduct(0.2, 6) == 1 {
    let c = Array(stride(from: -0.2, through: 1, by: 0.2))
    expectEqual(7, c.count)
    expectEqual(1 as Double, c.last)
  }

  if (1 as Float).addingProduct(0.9, 6) == 6.3999996 {
    let d = Array(stride(from: 1 as Float, through: 6.3999996, by: 0.9))
    expectEqual(7, d.count)
    // The reason that `d` has seven elements and not six is that the fused
    // multiply-add operation `(1 as Float).addingProduct(0.9, 6)` gives the
    // result `6.3999996`. This is nonetheless the desired behavior because
    // avoiding error accumulation and intermediate rounding error wherever
    // possible will produce better results more often than not (see
    // https://github.com/apple/swift/issues/48927).
    //
    // If checking of end bounds has been inadvertently modified such that we're
    // computing the distance from the penultimate element to the end (in this
    // case, `6.3999996 - (1 as Float).addingProduct(0.9, 5)`), then the last
    // element will be omitted here.
    //
    // Therefore, if the test has failed, there may have been a regression in
    // the bounds-checking logic of `Stride*Iterator`. Restore the expected
    // behavior here by ensuring that floating-point strides are opted out of
    // any bounds checking that performs arithmetic with values other than the
    // bounds themselves and the stride.
  }
}

func strideIteratorTest<
  Stride : Sequence
>(_ stride: Stride, nonNilResults: Int) {
  var i = stride.makeIterator()
  for _ in 0..<nonNilResults {
     expectNotNil(i.next())
  }
  for _ in 0..<10 {
    expectNil(i.next())
  }
}

StrideTestSuite.test("StrideThroughIterator/past end") {
  strideIteratorTest(stride(from: 0, through: 3, by: 1), nonNilResults: 4)
  strideIteratorTest(
    stride(from: UInt8(0), through: 255, by: 5), nonNilResults: 52)
}

StrideTestSuite.test("StrideThroughIterator/past end/backward") {
  strideIteratorTest(stride(from: 3, through: 0, by: -1), nonNilResults: 4)
}

StrideTestSuite.test("StrideToIterator/past end") {
  strideIteratorTest(stride(from: 0, to: 3, by: 1), nonNilResults: 3)
}

StrideTestSuite.test("StrideToIterator/past end/backward") {
  strideIteratorTest(stride(from: 3, to: 0, by: -1), nonNilResults: 3)
}

if #available(SwiftStdlib 5.6, *) {
  StrideTestSuite.test("Contains") {
    expectTrue(stride(from: 1, through: 5, by: 1).contains(3))
    expectTrue(stride(from: 1, to: 5, by: 1).contains(3))
    expectTrue(stride(from: 1, through: 5, by: 1).contains(5))
    expectFalse(stride(from: 1, to: 5, by: 1).contains(5))
    expectFalse(stride(from: 1, through: 5, by: -1).contains(3))
    expectFalse(stride(from: 1, to: 5, by: -1).contains(3))
    expectFalse(stride(from: 1, through: 5, by: -1).contains(1))
    expectFalse(stride(from: 1, to: 5, by: -1).contains(1))

    expectTrue(stride(from: 5, through: 1, by: -1).contains(3))
    expectTrue(stride(from: 5, to: 1, by: -1).contains(3))
    expectTrue(stride(from: 5, through: 1, by: -1).contains(1))
    expectFalse(stride(from: 5, to: 1, by: -1).contains(1))
    expectFalse(stride(from: 5, through: 1, by: 1).contains(3))
    expectFalse(stride(from: 5, to: 1, by: 1).contains(3))
    expectFalse(stride(from: 5, through: 1, by: 1).contains(5))
    expectFalse(stride(from: 5, to: 1, by: 1).contains(5))
  }
}

runAllTests()

