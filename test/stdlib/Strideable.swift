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
// RUN: %target-run-simple-swift -swift-version=3
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

StrideTestSuite.test("StrideToCollection/Int/IterationAndArraySameSequence") {
  do {
    let odds = stride(from: -42, to: 42, by: 2)
    expectEqualSequence(Array(odds), odds)
  }
  do {
    let odds = stride(from: 42, to: -42, by: -2)
    expectEqualSequence(Array(odds), odds)
  }
}

StrideTestSuite.test("StrideToCollection/Int") {
  func test(to end: Int) {
    let odds = stride(from: 1, to: end, by: 2)
    let expected = (1..<end).filter { $0 % 2 != 0 }
    expectEqualSequence(expected, odds)
  }
  test(to: 41)
  test(to: 42)
}

StrideTestSuite.test("StrideToCollection/Int/NegativeStride") {
  let odds = stride(from: 41, to: 0, by: -2)
  let expected = (0...41).reversed().filter { $0 % 2 != 0 }
  expectEqualSequence(expected, odds)
}

StrideTestSuite.test("StrideToCollection/Int/NegativeValues") {
  let odds = stride(from: -41, to: 0, by: 2)
  let expected = (-41..<0).filter { $0 % 2 != 0 }
  expectEqualSequence(expected, odds)
}

StrideTestSuite.test("StrideTo/SubSequence") {
  var sub = stride(from: 1, to: 1, by: 1).dropFirst()
  expectType(StrideTo.self, &sub)
}

StrideTestSuite.test("StrideTo/underestimatedCount") {
  expectEqual(0, stride(from: 1, to: 1, by: 2).underestimatedCount)
  expectEqual(1, stride(from: 1, to: 2, by: 2).underestimatedCount)
  expectEqual(2, stride(from: 1, to: 4, by: 2).underestimatedCount)
  expectEqual(5, stride(from: 0.1, to: 1.0, by: 0.2).underestimatedCount)
}

StrideTestSuite.test("StrideTo/dropFirst") {
  let getStride = { stride(from: 1, to: 10, by: 3) }
  expectEqualSequence([1, 4, 7], getStride().dropFirst(0))
  expectEqualSequence([4, 7], getStride().dropFirst(1))
  expectEqualSequence([], getStride().dropFirst(3))
  expectEqualSequence([], getStride().dropFirst(42))
  do {
    expectCrashLater()
    _ = getStride().dropFirst(-1)
  }
}

StrideTestSuite.test("StrideTo/dropLast") {
  let getStride = { stride(from: 1, to: 10, by: 3) }
  expectEqualSequence([1, 4, 7], getStride().dropLast(0))
  expectEqualSequence([1, 4], getStride().dropLast(1))
  expectEqualSequence([1], getStride().dropLast(2))
  expectEqualSequence([], getStride().dropLast(3))
  expectEqualSequence([], getStride().dropLast(42))
  do {
    expectCrashLater()
    _ = getStride().dropLast(-1)
  }
}

StrideTestSuite.test("StrideTo/drop(while:)") {
  let getStride = { stride(from: 1, to: 10, by: 3) }
  expectEqualSequence([1, 4, 7], getStride().drop { _ in false })
  expectEqualSequence([], getStride().drop { _ in true })
  expectEqualSequence([4, 7], getStride().drop { $0 < 4 })
  expectEqualSequence([], stride(from: 1, to: 1, by: 1).drop { _ in false })
  expectEqualSequence([], stride(from: 1, to: 1, by: 1).drop { _ in true })
}

StrideTestSuite.test("StrideTo/prefix(_:)") {
  let getStride = { stride(from: 1, to: 10, by: 3) }
  expectEqualSequence([], getStride().prefix(0))
  expectEqualSequence([1], getStride().prefix(1))
  expectEqualSequence([1, 4, 7], getStride().prefix(42))
  do {
    expectCrashLater()
    _ = getStride().prefix(-1)
  }
}

StrideTestSuite.test("StrideTo/prefix(while:)") {
  let getStride = { stride(from: 1, to: 10, by: 3) }
  expectEqualSequence([1, 4, 7], getStride().prefix { _ in true })
  expectEqualSequence([], getStride().prefix { _ in false })
  expectEqualSequence([1], getStride().prefix { $0 < 4 })
  expectEqualSequence([], stride(from: 1, to: 1, by: 1).prefix { _ in false })
  expectEqualSequence([], stride(from: 1, to: 1, by: 1).prefix { _ in true })
}

StrideTestSuite.test("StrideTo/suffix(_:)") {
  let getStride = { stride(from: 1, to: 10, by: 3) }
  expectEqualSequence([], getStride().suffix(0))
  expectEqualSequence([7], getStride().suffix(1))
  expectEqualSequence([1, 4, 7], getStride().suffix(42))
  do {
    expectCrashLater()
    _ = getStride().suffix(-1)
  }
}

StrideTestSuite.test("StrideTo/split") {
  let getStride = { stride(from: 1, to: 21, by: 3) }
  let getArray = { [1, 4, 7, 10, 13, 16, 19] }
  func splitTest(
    _ arr: [Int],
    _ strd: StrideTo<Int>,
    _ maxSplits: Int,
    _ omittingEmpty: Bool,
    file: String = #file,
    line: UInt = #line,
    _ whereSeparator: (Int) throws -> Bool
  ) rethrows {
    let arraySub = try arr.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmpty,
      whereSeparator: whereSeparator).map { Array($0) }
    let strideSub = try strd.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmpty,
      whereSeparator: whereSeparator).map { Array($0) }

    expectEqualSequence(arraySub, strideSub, file: file, line: line)
  }
  // corner cases
  splitTest(getArray(), getStride(), 0, false) { _ in true }
  splitTest(getArray(), getStride(), 1, false) { _ in true }
  splitTest(getArray(), getStride(), 42, false) { _ in true }
  splitTest(getArray(), getStride(), 0, true) { _ in true }
  splitTest(getArray(), getStride(), 1, true) { _ in true }
  splitTest(getArray(), getStride(), 42, true) { _ in true }
  splitTest(getArray(), getStride(), 0, false) { _ in false }
  splitTest(getArray(), getStride(), 1, false) { _ in false }
  splitTest(getArray(), getStride(), 42, false) { _ in false }
  splitTest(getArray(), getStride(), 0, true) { _ in false }
  splitTest(getArray(), getStride(), 1, true) { _ in false }
  splitTest(getArray(), getStride(), 42, true) { _ in false }

  // sparse seaparators
  splitTest(getArray(), getStride(), 0, true) { $0 % 2 == 0 }
  splitTest(getArray(), getStride(), 1, true) { $0 % 2 == 0 }
  splitTest(getArray(), getStride(), 3, true) { $0 % 2 == 0 }
  splitTest(getArray(), getStride(), 42, true) { $0 % 2 == 0 }
  splitTest(getArray(), getStride(), 0, false) { $0 % 2 == 0 }
  splitTest(getArray(), getStride(), 1, false) { $0 % 2 == 0 }
  splitTest(getArray(), getStride(), 3, false) { $0 % 2 == 0 }
  splitTest(getArray(), getStride(), 42, false) { $0 % 2 == 0 }

  // adjacent seaparators
  splitTest(getArray(), getStride(), 0, true) { 10...13 ~= $0 }
  splitTest(getArray(), getStride(), 1, true) { 10...13 ~= $0 }
  splitTest(getArray(), getStride(), 3, true) { 10...13 ~= $0 }
  splitTest(getArray(), getStride(), 42, true) { 10...13 ~= $0 }
  splitTest(getArray(), getStride(), 0, false) { 10...13 ~= $0 }
  splitTest(getArray(), getStride(), 1, false) { 10...13 ~= $0 }
  splitTest(getArray(), getStride(), 3, false) { 10...13 ~= $0 }
  splitTest(getArray(), getStride(), 42, false) { 10...13 ~= $0 }

  do {
    expectCrashLater()
    _ = getStride().split(
      maxSplits: -1, omittingEmptySubsequences: false) { _ in false }
  }
}

#if false
StrideTestSuite.test(
  "StrideThroughCollection/Int/IterationAndArraySameSequence"
) {
  do {
    let odds = stride(from: -42, through: 42, by: 2)
    expectEqualSequence(Array(odds), odds)
  }
  do {
    let odds = stride(from: 42, through: -42, by: -2)
    expectEqualSequence(Array(odds), odds)
  }
}

StrideTestSuite.test("StrideThroughCollection/Int") {
  func test(through end: Int) {
    let odds = stride(from: 1, through: end, by: 2)
    let expected = (1...end).filter { $0 % 2 != 0 }
    expectEqualSequence(expected, odds)
  }
  test(through: 41)
  test(through: 42)
}

StrideTestSuite.test("StrideThroughCollection/Int/NegativeStride") {
  let odds = stride(from: 41, through: 1, by: -2)
  let expected = (0...41).reversed().filter { $0 % 2 != 0 }
  expectEqualSequence(expected, odds)
}

StrideTestSuite.test("StrideThroughCollection/Int/NegativeValues") {
  let odds = stride(from: -41, through: -1, by: 2)
  let expected = (-41..<0).filter { $0 % 2 != 0 }
  expectEqualSequence(expected, odds)
}

#endif


runAllTests()

