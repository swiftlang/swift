//===--- GenericLifetimeTracked.swift -------------------------------------===//
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

public enum _GlobalLeakCount {
  public static var count = 0
}

/// A type that tracks the number of live instances of a wrapped value type.
///
/// `Tracked<T>` is used to check for memory leaks in functions created via
/// automatic differentiation.
public struct Tracked<T> {
  fileprivate class Box {
    fileprivate let value : T
    init(_ value: T) {
      self.value = value
        _GlobalLeakCount.count += 1
    }
    deinit {
      _GlobalLeakCount.count -= 1
    }
  }
  private let handle: Box
  public init(_ value: T) {
    self.handle = Box(value)
  }
  public var value: T { return handle.value }
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
    lhs = Tracked(lhs.value * rhs.value)
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
extension Tracked : Differentiable
  where T : Differentiable, T == T.AllDifferentiableVariables,
        T == T.TangentVector, T == T.CotangentVector
{
  public typealias AllDifferentiableVariables = Tracked<T.AllDifferentiableVariables>
  public typealias TangentVector = Tracked<T.TangentVector>
  public typealias CotangentVector = Tracked<T.CotangentVector>
  @inlinable @inline(__always)
  public func tangentVector(from cotangent: CotangentVector) -> TangentVector {
    return Tracked<T.TangentVector>(value.tangentVector(from: cotangent.value))
  }
}

@differentiable(vjp: _vjpAdd)
public func + (_ a: Tracked<Float>, _ b: Tracked<Float>) -> Tracked<Float> {
  return Tracked<Float>(a.value + b.value)
}

@usableFromInline
func _vjpAdd(_ a: Tracked<Float>, _ b: Tracked<Float>)
    -> (Tracked<Float>, (Tracked<Float>) -> (Tracked<Float>, Tracked<Float>)) {
  return (Tracked<Float>(a.value + b.value), { v in
    return (v, v)
  })
}

// Differential operators for `Tracked<Float>`.
public extension Differentiable {
  @inlinable
  func gradient(
    in f: @differentiable (Self) -> Tracked<Float>
  ) -> CotangentVector {
    return self.pullback(in: f)(1)
  }

  @inlinable
  func gradient<T : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> Tracked<Float>
  ) -> (CotangentVector, T.CotangentVector) {
    return self.pullback(at: x, in: f)(1)
  }
}
