//===--- Interval.swift ---------------------------------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// XFAIL: interpret

import StdlibUnittest

var IntervalTestSuite = TestSuite("Interval")

IntervalTestSuite.test("Ambiguity") {
  // Ensure type deduction still works as expected; these will fail to
  // compile if it's broken
  var pieToPie = -3.1415927..<3.1415927
  expectType(HalfOpenInterval<Double>.self, &pieToPie)

  var pieThruPie = -3.1415927...3.1415927
  expectType(ClosedInterval<Double>.self, &pieThruPie)

  var zeroToOne = 0..<1
  expectType(Range<Int>.self, &zeroToOne)

  var zeroThruOne = 0...1
  // If/when we get a separate ClosedRange representation, this test
  // will have to change.
  expectType(Range<Int>.self, &zeroThruOne)
}

IntervalTestSuite.test("PatternMatching") {

  let pie = 3.1415927

  let expectations : [(Double, halfOpen: Bool, closed: Bool)] = [
    (-2 * pie, false, false),
    (-pie, true, true),
    (0, true, true),
    (pie, false, true),
    (2 * pie, false, false)
  ]

  for (x, halfOpenExpected, closedExpected) in expectations {
    var halfOpen: Bool
    switch x {
    case -3.1415927..<3.1415927:
      halfOpen = true
    default:
      halfOpen = false
    }

    var closed: Bool
    switch x {
    case -3.1415927...3.1415927:
      closed = true
    default:
      closed = false
    }
    
    expectEqual(halfOpenExpected, halfOpen)
    expectEqual(closedExpected, closed)
  }
}

IntervalTestSuite.test("Overlaps") {
  
  func expectOverlaps<
    I0: IntervalType, I1: IntervalType where I0.Bound == I1.Bound
  >(expectation: Bool, _ lhs: I0, _ rhs: I1) {
    if expectation {
      expectTrue(overlaps(lhs, rhs))
      expectTrue(overlaps(rhs, lhs))
    }
    else {
      expectFalse(overlaps(lhs, rhs))
      expectFalse(overlaps(rhs, lhs))
    }
  }
  
  // 0-4, 5-10
  expectOverlaps(false, 0..<4, 5..<10)
  expectOverlaps(false, 0..<4, 5...10)
  expectOverlaps(false, 0...4, 5..<10)
  expectOverlaps(false, 0...4, 5...10)

  // 0-5, 5-10
  expectOverlaps(false, 0..<5, 5..<10)
  expectOverlaps(false, 0..<5, 5...10)
  expectOverlaps(true, 0...5, 5..<10)
  expectOverlaps(true, 0...5, 5...10)

  // 0-6, 5-10
  expectOverlaps(true, 0..<6, 5..<10)
  expectOverlaps(true, 0..<6, 5...10)
  expectOverlaps(true, 0...6, 5..<10)
  expectOverlaps(true, 0...6, 5...10)

  // 0-20, 5-10
  expectOverlaps(true, 0..<20, 5..<10)
  expectOverlaps(true, 0..<20, 5...10)
  expectOverlaps(true, 0...20, 5..<10)
  expectOverlaps(true, 0...20, 5...10)
}

IntervalTestSuite.test("Emptiness") {
  expectTrue((0.0..<0.0).isEmpty)
  expectFalse((0.0...0.0).isEmpty)
  expectFalse((0.0..<0.1).isEmpty)
  expectFalse((0.0..<0.1).isEmpty)
}

IntervalTestSuite.test("start/end") {
  expectEqual(0.0, (0.0..<0.1).start)
  expectEqual(0.0, (0.0...0.1).start)
  expectEqual(0.1, (0.0..<0.1).end)
  expectEqual(0.1, (0.0...0.1).end)
}

// Something to test with that distinguishes debugDescription from description
struct X<T : Comparable> : Comparable, CustomStringConvertible, CustomDebugStringConvertible {
  init(_ a: T) {
    self.a = a
  }

  var description: String {
    return String(a)
  }

  var debugDescription: String {
    return "X(\(String(reflecting: a)))"
  }
  
  var a: T
}

func < <T : Comparable>(lhs: X<T>, rhs: X<T>) -> Bool {
  return lhs.a < rhs.a
}

func == <T : Comparable>(lhs: X<T>, rhs: X<T>) -> Bool {
  return lhs.a == rhs.a
}

IntervalTestSuite.test("CustomStringConvertible/CustomDebugStringConvertible") {
  expectEqual("0.0..<0.1", String(X(0.0)..<X(0.1)))
  expectEqual("0.0...0.1", String(X(0.0)...X(0.1)))
  
  expectEqual(
    "HalfOpenInterval(X(0.0)..<X(0.1))",
    String(reflecting: HalfOpenInterval(X(0.0)..<X(0.1))))
  expectEqual(
    "ClosedInterval(X(0.0)...X(0.1))",
    String(reflecting: ClosedInterval(X(0.0)...X(0.1))))
}

IntervalTestSuite.test("rdar12016900") {
  if true {
    let wc = 0
    expectFalse((0x00D800 ..< 0x00E000).contains(wc))
  }
  if true {
    let wc = 0x00D800
    expectTrue((0x00D800 ..< 0x00E000).contains(wc))
  }
}

IntervalTestSuite.test("clamp") {
  expectEqual(
    (5..<10).clamp(0..<3), 5..<5)
  expectEqual(
    (5..<10).clamp(0..<9), 5..<9)
  expectEqual(
    (5..<10).clamp(0..<13), 5..<10)
  expectEqual(
    (5..<10).clamp(7..<9), 7..<9)
  expectEqual(
    (5..<10).clamp(7..<13), 7..<10)
  expectEqual(
    (5..<10).clamp(13..<15), 10..<10)

  expectEqual(
    (5...10).clamp(0...3), 5...5)
  expectEqual(
    (5...10).clamp(0...9), 5...9)
  expectEqual(
    (5...10).clamp(0...13), 5...10)
  expectEqual(
    (5...10).clamp(7...9), 7...9)
  expectEqual(
    (5...10).clamp(7...13), 7...10)
  expectEqual(
    (5...10).clamp(13...15), 10...10)
}

runAllTests()

