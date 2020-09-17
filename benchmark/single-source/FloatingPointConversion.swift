//===--- FloatingPointConversion.swift ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let FloatingPointConversion = [
  BenchmarkInfo(
    name: "ConvertFloatingPoint.MockFloat64ToDouble",
    runFunction: run_ConvertFloatingPoint_MockFloat64ToDouble,
    tags: [.validation, .api],
    setUpFunction: { blackHole(mockFloat64s) }),
  BenchmarkInfo(
    name: "ConvertFloatingPoint.MockFloat64Exactly",
    runFunction: run_ConvertFloatingPoint_MockFloat64Exactly,
    tags: [.validation, .api],
    setUpFunction: { blackHole(mockFloat64s) }),
  BenchmarkInfo(
    name: "ConvertFloatingPoint.MockFloat64Exactly2",
    runFunction: run_ConvertFloatingPoint_MockFloat64Exactly2,
    tags: [.validation, .api],
    setUpFunction: { blackHole(mockFloat64s) }),
  BenchmarkInfo(
    name: "ConvertFloatingPoint.MockFloat64ToInt64",
    runFunction: run_ConvertFloatingPoint_MockFloat64ToInt64,
    tags: [.validation, .api],
    setUpFunction: { blackHole(mockFloat64s) }),
]

protocol MockBinaryFloatingPoint: BinaryFloatingPoint {
  associatedtype _Value: BinaryFloatingPoint
  var _value: _Value { get set }
  init(_ _value: _Value)
}

extension MockBinaryFloatingPoint {
  static var exponentBitCount: Int { _Value.exponentBitCount }
  static var greatestFiniteMagnitude: Self {
    Self(_Value.greatestFiniteMagnitude)
  }
  static var infinity: Self { Self(_Value.infinity) }
  static var leastNonzeroMagnitude: Self { Self(_Value.leastNonzeroMagnitude) }
  static var leastNormalMagnitude: Self { Self(_Value.leastNormalMagnitude) }
  static var nan: Self { Self(_Value.nan) }
  static var pi: Self { Self(_Value.pi) }
  static var signalingNaN: Self { Self(_Value.signalingNaN) }
  static var significandBitCount: Int { _Value.significandBitCount }
  
  static func + (lhs: Self, rhs: Self) -> Self { Self(lhs._value + rhs._value) }
  static func += (lhs: inout Self, rhs: Self) { lhs._value += rhs._value }
  static func - (lhs: Self, rhs: Self) -> Self { Self(lhs._value - rhs._value) }
  static func -= (lhs: inout Self, rhs: Self) { lhs._value -= rhs._value }
  static func * (lhs: Self, rhs: Self) -> Self { Self(lhs._value * rhs._value) }
  static func *= (lhs: inout Self, rhs: Self) { lhs._value *= rhs._value }
  static func / (lhs: Self, rhs: Self) -> Self { Self(lhs._value / rhs._value) }
  static func /= (lhs: inout Self, rhs: Self) { lhs._value /= rhs._value }
  
  init(_ value: Int) { self.init(_Value(value)) }
  init(_ value: Float) { self.init(_Value(value)) }
  init(_ value: Double) { self.init(_Value(value)) }
#if !(os(Windows) || os(Android)) && (arch(i386) || arch(x86_64))
  init(_ value: Float80) { self.init(_Value(value)) }
#endif
  init(integerLiteral value: _Value.IntegerLiteralType) {
    self.init(_Value(integerLiteral: value))
  }
  init(floatLiteral value: _Value.FloatLiteralType) {
    self.init(_Value(floatLiteral: value))
  }
  init(sign: FloatingPointSign, exponent: _Value.Exponent, significand: Self) {
    self.init(
      _Value(sign: sign, exponent: exponent, significand: significand._value))
  }
  init(
    sign: FloatingPointSign,
    exponentBitPattern: _Value.RawExponent,
    significandBitPattern: _Value.RawSignificand
  ) {
    self.init(
      _Value(
        sign: sign,
        exponentBitPattern: exponentBitPattern,
        significandBitPattern: significandBitPattern))
  }
  
  var binade: Self { Self(_value.binade) }
  var exponent: _Value.Exponent { _value.exponent }
  var exponentBitPattern: _Value.RawExponent { _value.exponentBitPattern }
  var isCanonical: Bool { _value.isCanonical }
  var isFinite: Bool { _value.isFinite }
  var isInfinite: Bool { _value.isInfinite }
  var isNaN: Bool { _value.isNaN }
  var isNormal: Bool { _value.isNormal }
  var isSignalingNaN: Bool { _value.isSignalingNaN }
  var isSubnormal: Bool { _value.isSubnormal }
  var isZero: Bool { _value.isZero }
  var magnitude: Self { Self(_value.magnitude) }
  var nextDown: Self { Self(_value.nextDown) }
  var nextUp: Self { Self(_value.nextUp) }
  var sign: FloatingPointSign { _value.sign }
  var significand: Self { Self(_value.significand) }
  var significandBitPattern: _Value.RawSignificand {
    _value.significandBitPattern
  }
  var significandWidth: Int { _value.significandWidth }
  var ulp: Self { Self(_value.ulp) }
  
  mutating func addProduct(_ lhs: Self, _ rhs: Self) {
    _value.addProduct(lhs._value, rhs._value)
  }
  func advanced(by n: _Value.Stride) -> Self { Self(_value.advanced(by: n)) }
  func distance(to other: Self) -> _Value.Stride {
    _value.distance(to: other._value)
  }
  mutating func formRemainder(dividingBy other: Self) {
    _value.formRemainder(dividingBy: other._value)
  }
  mutating func formSquareRoot() { _value.formSquareRoot() }
  mutating func formTruncatingRemainder(dividingBy other: Self) {
    _value.formTruncatingRemainder(dividingBy: other._value)
  }
  func isEqual(to other: Self) -> Bool { _value.isEqual(to: other._value) }
  func isLess(than other: Self) -> Bool { _value.isLess(than: other._value) }
  func isLessThanOrEqualTo(_ other: Self) -> Bool {
    _value.isLessThanOrEqualTo(other._value)
  }
  mutating func round(_ rule: FloatingPointRoundingRule) { _value.round(rule) }
}

struct MockFloat64: MockBinaryFloatingPoint {
  var _value: Double
  init(_ _value: Double) { self._value = _value }
}

struct MockFloat32: MockBinaryFloatingPoint {
  var _value: Float
  init(_ _value: Float) { self._value = _value }
}

let doubles = [
   1.8547832857295,  26.321549267719135, 98.9544480962058,    73.70286973782363,
  82.04918555938816, 76.38902969312758,  46.35647857011161,   64.0821426030317,
  97.82373347320156, 55.742361037720634, 23.677941665488856,  93.7347588108058,
  80.72657040828412, 32.137580733275826, 64.78192587530002,   21.459686568896863,
  24.88407660280718, 85.25905561999171,  12.858847331083556,  29.418845887252864,
  67.64627066438761, 68.09883494078815,  57.781587230862094,  63.38335631088038,
  83.31376661495327, 87.45936846358906,   0.6757674136841918, 86.45465036820696,
  84.72715137492781, 82.67894289189142,  26.1667640621554,    21.24895661442493,
  65.06399183516027, 90.06549073883058,  59.2736650501005,    94.5800380563246,
  84.22617424003917, 26.93158630395639,   9.069952095976841,  96.10067836567679,
  62.60505762081415, 29.57878462599286,  66.06040114311294,   51.709999429326636,
  64.79777579583545, 45.25948795832151,  94.31492354198335,   52.31096166433902,
]

let mockFloat64s = doubles.map { MockFloat64($0) }

// See also: test/SILOptimizer/floating_point_conversion.swift

@inline(never)
public func run_ConvertFloatingPoint_MockFloat64ToDouble(_ N: Int) {
  for _ in 0..<(N * 100) {
    for element in mockFloat64s {
      let f = Double(identity(element))
      blackHole(f)
    }
  }
}

@inline(__always)
func convert<
  T: BinaryFloatingPoint, U: BinaryFloatingPoint
>(exactly value: T, to: U.Type) -> U? {
  U(exactly: value)
}

@inline(never)
public func run_ConvertFloatingPoint_MockFloat64Exactly(_ N: Int) {
  for _ in 0..<(N * 25) {
    for element in mockFloat64s {
      let f = convert(exactly: identity(element), to: Double.self)
      blackHole(f)
    }
  }
}

@inline(never)
public func run_ConvertFloatingPoint_MockFloat64Exactly2(_ N: Int) {
  for _ in 0..<(N * 25) {
    for element in mockFloat64s {
      let f = convert(exactly: identity(element), to: MockFloat32.self)
      blackHole(f)
    }
  }
}

@inline(never)
public func run_ConvertFloatingPoint_MockFloat64ToInt64(_ N: Int) {
  for _ in 0..<(N * 1000) {
    for element in mockFloat64s {
      let i = Int64(identity(element))
      blackHole(i)
    }
  }
}
