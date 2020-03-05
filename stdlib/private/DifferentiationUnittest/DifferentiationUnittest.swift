//===--- DifferentiationUnittest.swift ------------------------------------===//
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

import StdlibUnittest

public enum _GlobalLeakCount {
  public static var count = 0
}

/// Execute body and check expected leak count.
public func withLeakChecking(
  expectedLeakCount: Int = 0, file: String = #file, line: UInt = #line,
  _ body: () -> Void
) {
  // Note: compare expected leak count with relative leak count after
  // running `body`.
  // This approach is more robust than comparing leak count with zero
  // and resetting leak count to zero, which is stateful and causes issues.
  let beforeLeakCount = _GlobalLeakCount.count
  body()
  let leakCount = _GlobalLeakCount.count - beforeLeakCount
  expectEqual(
    expectedLeakCount, leakCount, "Leaks detected: \(leakCount)",
    file: file, line: line)
}

public extension TestSuite {
  /// Execute test function and check expected leak count.
  func testWithLeakChecking(
    _ name: String,
    expectedLeakCount: Int = 0,
    file: String = #file, line: UInt = #line,
    _ testFunction: @escaping () -> Void
  ) {
    test(name, file: file, line: line) {
      withLeakChecking(expectedLeakCount: expectedLeakCount, file: file,
                       line: line, testFunction)
    }
  }
}

/// A type that tracks the number of live instances of a wrapped value type.
///
/// `Tracked<T>` is used to check for memory leaks in functions created via
/// automatic differentiation.
public struct Tracked<T> {
  fileprivate class Box {
    fileprivate var value : T
    init(_ value: T) {
      self.value = value
      _GlobalLeakCount.count += 1
    }
    deinit {
      _GlobalLeakCount.count -= 1
    }
  }
  private var handle: Box

  @differentiable(where T : Differentiable, T == T.TangentVector)
  public init(_ value: T) {
    self.handle = Box(value)
  }

  @differentiable(where T : Differentiable, T == T.TangentVector)
  public var value: T {
    get { handle.value }
    set { handle.value = newValue }
  }
}

extension Tracked : ExpressibleByFloatLiteral where T : ExpressibleByFloatLiteral {
  public init(floatLiteral value: T.FloatLiteralType) {
    self.handle = Box(T(floatLiteral: value))
  }
}

extension Tracked : CustomStringConvertible {
  public var description: String { return "Tracked(\(value))" }
}

extension Tracked : ExpressibleByIntegerLiteral where T : ExpressibleByIntegerLiteral {
  public init(integerLiteral value: T.IntegerLiteralType) {
    self.handle = Box(T(integerLiteral: value))
  }
}

extension Tracked : Comparable where T : Comparable {
  public static func < (lhs: Tracked, rhs: Tracked) -> Bool {
    return lhs.value < rhs.value
  }
  public static func <= (lhs: Tracked, rhs: Tracked) -> Bool {
    return lhs.value <= rhs.value
  }
  public static func > (lhs: Tracked, rhs: Tracked) -> Bool {
    return lhs.value > rhs.value
  }
  public static func >= (lhs: Tracked, rhs: Tracked) -> Bool {
    return lhs.value >= rhs.value
  }
}

extension Tracked : AdditiveArithmetic where T : AdditiveArithmetic {
  public static var zero: Tracked { return Tracked(T.zero) }
  public static func + (lhs: Tracked, rhs: Tracked) -> Tracked {
    return Tracked(lhs.value + rhs.value)
  }
  public static func - (lhs: Tracked, rhs: Tracked) -> Tracked {
    return Tracked(lhs.value - rhs.value)
  }
}

extension Tracked : Equatable where T : Equatable {
  public static func == (lhs: Tracked, rhs: Tracked) -> Bool {
    return lhs.value == rhs.value
  }
}

extension Tracked : SignedNumeric & Numeric where T : SignedNumeric, T == T.Magnitude {
  public typealias Magnitude = Tracked<T.Magnitude>

  public init?<U>(exactly source: U) where U : BinaryInteger {
    if let t = T(exactly: source) {
      self.init(t)
    }
    return nil
  }
  public var magnitude: Magnitude { return Magnitude(value.magnitude) }

  public static func * (lhs: Tracked, rhs: Tracked) -> Tracked {
    return Tracked(lhs.value * rhs.value)
  }

  public static func *= (lhs: inout Tracked, rhs: Tracked) {
    lhs = lhs * rhs
  }
}

extension Tracked where T : FloatingPoint {
  public static func / (lhs: Tracked, rhs: Tracked) -> Tracked {
    return Tracked(lhs.value / rhs.value)
  }

  public static func /= (lhs: inout Tracked, rhs: Tracked) {
    lhs = lhs / rhs
  }
}

extension Tracked : Strideable where T : Strideable, T.Stride == T.Stride.Magnitude {
  public typealias Stride = Tracked<T.Stride>

  public func distance(to other: Tracked) -> Stride {
    return Stride(value.distance(to: other.value))
  }
  public func advanced(by n: Stride) -> Tracked {
    return Tracked(value.advanced(by: n.value))
  }
}

// For now, `T` must be restricted to trivial types (like `Float` or `Tensor`).
extension Tracked : Differentiable where T : Differentiable, T == T.TangentVector {
  public typealias TangentVector = Tracked<T.TangentVector>
}

extension Tracked where T : Differentiable, T == T.TangentVector {
  @usableFromInline
  @derivative(of: init)
  internal static func _vjpInit(_ value: T)
      -> (value: Self, pullback: (Self.TangentVector) -> (T.TangentVector)) {
    return (Tracked(value), { v in v.value })
  }

  @usableFromInline
  @derivative(of: init)
  internal static func _jvpInit(_ value: T)
      -> (value: Self, differential: (T.TangentVector) -> (Self.TangentVector)) {
    return (Tracked(value), { v in Tracked(v) })
  }

  @usableFromInline
  @derivative(of: value)
  internal func _vjpValue() -> (value: T, pullback: (T.TangentVector) -> Self.TangentVector) {
    return (value, { v in Tracked(v) })
  }

  @usableFromInline
  @derivative(of: value)
  internal func _jvpValue() -> (value: T, differential: (Self.TangentVector) -> T.TangentVector) {
    return (value, { v in v.value })
  }
}

extension Tracked where T : Differentiable, T == T.TangentVector {
  @usableFromInline
  @derivative(of: +)
  internal static func _vjpAdd(lhs: Self, rhs: Self)
      -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (lhs + rhs, { v in (v, v) })
  }

  @usableFromInline
  @derivative(of: +)
  internal static func _jvpAdd(lhs: Self, rhs: Self)
      -> (value: Self, differential: (Self, Self) -> Self) {
    return (lhs + rhs, { $0 + $1 })
  }

  @usableFromInline
  @derivative(of: -)
  internal static func _vjpSubtract(lhs: Self, rhs: Self)
      -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (lhs - rhs, { v in (v, .zero - v) })
  }

  @usableFromInline
  @derivative(of: -)
  internal static func _jvpSubtract(lhs: Self, rhs: Self)
      -> (value: Self, differential: (Self, Self) -> Self) {
    return (lhs - rhs, { $0 - $1 })
  }
}

extension Tracked where T : Differentiable & SignedNumeric, T == T.Magnitude,
                        T == T.TangentVector {
  @usableFromInline
  @derivative(of: *)
  internal static func _vjpMultiply(lhs: Self, rhs: Self)
      -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (lhs * rhs, { v in (v * rhs, v * lhs) })
  }

  @usableFromInline
  @derivative(of: *)
  internal static func _jvpMultiply(lhs: Self, rhs: Self)
      -> (value: Self, differential: (Self, Self) -> (Self)) {
    return (lhs * rhs, { (dx, dy) in dx * rhs + dy * lhs })
  }
}

extension Tracked where T : Differentiable & FloatingPoint, T == T.TangentVector {
  @usableFromInline
  @derivative(of: /)
  internal static func _vjpDivide(lhs: Self, rhs: Self)
      -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (lhs / rhs, { v in (v / rhs, -lhs / (rhs * rhs) * v) })
  }

  @usableFromInline
  @derivative(of: /)
  internal static func _jvpDivide(lhs: Self, rhs: Self)
      -> (value: Self, differential: (Self, Self) -> (Self)) {
    return (lhs / rhs, { (dx, dy) in dx / rhs - lhs / (rhs * rhs) * dy })
  }
}

// Differential operators for `Tracked<T>`.

/*
public func gradient<T, R: FloatingPoint>(
  at x: T, in f: @differentiable (T) -> Tracked<R>
) -> T.TangentVector where R.TangentVector == R {
  return pullback(at: x, in: f)(1)
}

public func gradient<T, U, R: FloatingPoint>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> Tracked<R>
) -> (T.TangentVector, U.TangentVector) where R.TangentVector == R {
  return pullback(at: x, y, in: f)(1)
}

public func derivative<T: FloatingPoint, R>(
  at x: Tracked<T>, in f: @differentiable (Tracked<T>) -> R
) -> R.TangentVector where T.TangentVector == T {
  return differential(at: x, in: f)(1)
}

public func derivative<T: FloatingPoint, U: FloatingPoint, R>(
  at x: Tracked<T>, _ y: Tracked<U>,
  in f: @differentiable (Tracked<T>, Tracked<U>) -> R
) -> R.TangentVector where T.TangentVector == T, U.TangentVector == U {
  return differential(at: x, y, in: f)(1, 1)
}

public func valueWithGradient<T, R: FloatingPoint>(
  at x: T, in f: @differentiable (T) -> Tracked<R>
) -> (value: Tracked<R>, gradient: T.TangentVector) {
  let (y, pullback) = valueWithPullback(at: x, in: f)
  return (y, pullback(1))
}

public func valueWithDerivative<T: FloatingPoint, R>(
  at x: Tracked<T>, in f: @differentiable (Tracked<T>) -> R
) -> (value: R, derivative: R.TangentVector) {
  let (y, differential) = valueWithDifferential(at: x, in: f)
  return (y, differential(1))
}
*/
