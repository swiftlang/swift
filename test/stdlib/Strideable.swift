//===--- Strideable.swift - Tests for strided iteration -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
    var result = r1 + stride
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  do {
    var result = stride + r1
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  do {
    var result = r1 - stride
    expectType(R.self, &result)
    expectEqual(45, result.x)
  }
  do {
    var result = r1 - r2
    expectType(Int.self, &result)
    expectEqual(-20, result)
  }
  do {
    var result = r1
    result += stride
    expectType(R.self, &result)
    expectEqual(55, result.x)
  }
  do {
    var result = r1
    result -= stride
    expectType(R.self, &result)
    expectEqual(45, result.x)
  }
}

StrideTestSuite.test("FloatingPointStride") {
  var result = [Double]()
  for i in stride(from: 1.4, through: 3.4, by: 1) {
    result.append(i)
  }
  expectEqual([ 1.4, 2.4, 3.4 ], result)
}

StrideTestSuite.test("ErrorAccumulation") {
  let a = Array(stride(from: Float(1.0), through: Float(2.0), by: Float(0.1)))
  expectEqual(11, a.count)
  expectEqual(Float(2.0), a.last)
  let b = Array(stride(from: Float(1.0), to: Float(10.0), by: Float(0.9)))
  expectEqual(10, b.count)
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

runAllTests()

