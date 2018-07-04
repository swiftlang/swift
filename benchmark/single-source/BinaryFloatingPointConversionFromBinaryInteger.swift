//===--- BinaryFloatingPointConversionFromBinaryInteger.swift -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test checks performance of generic binary floating-point conversion from
// a binary integer.

import Foundation
import TestsUtils

#if swift(>=4.2)
public let BinaryFloatingPointConversionFromBinaryInteger = BenchmarkInfo(
  name: "BinaryFloatingPointConversionFromBinaryInteger",
  runFunction: run_BinaryFloatingPointConversionFromBinaryInteger,
  tags: [.validation, .algorithm]
)
#else
public let BinaryFloatingPointConversionFromBinaryInteger: [BenchmarkInfo] = []
#endif

struct MockBinaryInteger<T : BinaryInteger> {
  var _value: T

  init(_ value: T) {
    _value = value
  }
}

extension MockBinaryInteger : CustomStringConvertible {
  var description: String {
    return _value.description
  }
}

extension MockBinaryInteger : ExpressibleByIntegerLiteral {
  init(integerLiteral value: T.IntegerLiteralType) {
    _value = T(integerLiteral: value)
  }
}

extension MockBinaryInteger : Comparable {
  static func < (lhs: MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) -> Bool {
    return lhs._value < rhs._value
  }

  static func == (
    lhs: MockBinaryInteger<T>, rhs: MockBinaryInteger<T>
  ) -> Bool {
    return lhs._value == rhs._value
  }
}

extension MockBinaryInteger : Hashable {
  func hash(into hasher: inout Hasher) {
    hasher.combine(_value)
  }
}

extension MockBinaryInteger : BinaryInteger {
  static var isSigned: Bool {
    return T.isSigned
  }

  init<Source>(_ source: Source) where Source : BinaryFloatingPoint {
    _value = T(source)
  }

  init?<Source>(exactly source: Source) where Source : BinaryFloatingPoint {
    guard let result = T(exactly: source) else { return nil }
    _value = result
  }

  init<Source>(_ source: Source) where Source : BinaryInteger {
    _value = T(source)
  }

  init?<Source>(exactly source: Source) where Source : BinaryInteger {
    guard let result = T(exactly: source) else { return nil }
    _value = result
  }

  init<Source>(truncatingIfNeeded source: Source) where Source : BinaryInteger {
    _value = T(truncatingIfNeeded: source)
  }

  init<Source>(clamping source: Source) where Source : BinaryInteger {
    _value = T(clamping: source)
  }

  var magnitude: MockBinaryInteger<T.Magnitude> {
    return MockBinaryInteger<T.Magnitude>(_value.magnitude)
  }

  var words: T.Words {
    return _value.words
  }

  var bitWidth: Int {
    return _value.bitWidth
  }

  var trailingZeroBitCount: Int {
    return _value.trailingZeroBitCount
  }

  static func + (
    lhs: MockBinaryInteger<T>, rhs: MockBinaryInteger<T>
  ) -> MockBinaryInteger<T> {
    return MockBinaryInteger(lhs._value + rhs._value)
  }

  static func += (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value += rhs._value
  }

  static func - (
    lhs: MockBinaryInteger<T>, rhs: MockBinaryInteger<T>
  ) -> MockBinaryInteger<T> {
    return MockBinaryInteger(lhs._value - rhs._value)
  }

  static func -= (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value -= rhs._value
  }

  static func * (
    lhs: MockBinaryInteger<T>, rhs: MockBinaryInteger<T>
  ) -> MockBinaryInteger<T> {
    return MockBinaryInteger(lhs._value * rhs._value)
  }

  static func *= (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value *= rhs._value
  }

  static func / (
    lhs: MockBinaryInteger<T>, rhs: MockBinaryInteger<T>
  ) -> MockBinaryInteger<T> {
    return MockBinaryInteger(lhs._value / rhs._value)
  }

  static func /= (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value /= rhs._value
  }

  static func % (
    lhs: MockBinaryInteger<T>, rhs: MockBinaryInteger<T>
  ) -> MockBinaryInteger<T> {
    return MockBinaryInteger(lhs._value % rhs._value)
  }

  static func %= (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value %= rhs._value
  }

  static func &= (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value &= rhs._value
  }

  static func |= (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value |= rhs._value
  }

  static func ^= (lhs: inout MockBinaryInteger<T>, rhs: MockBinaryInteger<T>) {
    lhs._value ^= rhs._value
  }

  static prefix func ~ (x: MockBinaryInteger<T>) -> MockBinaryInteger<T> {
    return MockBinaryInteger(~x._value)
  }

  static func >>= <RHS>(
    lhs: inout MockBinaryInteger<T>, rhs: RHS
  ) where RHS : BinaryInteger {
    lhs._value >>= rhs
  }

  static func <<= <RHS>(
    lhs: inout MockBinaryInteger<T>, rhs: RHS
  ) where RHS : BinaryInteger {
    lhs._value <<= rhs
  }
}

#if swift(>=4.2)

@inline(never)
public func run_BinaryFloatingPointConversionFromBinaryInteger(_ N: Int) {
  var xs = [Double]()
  xs.reserveCapacity(N)
  for _ in 1...N {
    var x = 0 as Double
    for i in 0..<2000 {
      x += Double._convert(from: MockBinaryInteger(getInt(i))).value
    }
    xs.append(x)
  }
  CheckResults(xs[getInt(0)] == 1999000)
}

#endif

