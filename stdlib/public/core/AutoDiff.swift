//===--- AutoDiff.swift ---------------------------------------*- swift -*-===//
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
//
// SWIFT_ENABLE_TENSORFLOW
//
// This file defines support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Compiler Protocols
//===----------------------------------------------------------------------===//

/// A type that represents an unranked vector space. Values of this type are
/// elements in this vector space and with a specific dimensionality.
public protocol VectorNumeric {
  /// The type of scalars in the real vector space.
  associatedtype ScalarElement

  /// The type whose values specifies the dimensionality of an object in the
  /// real vector space.
  associatedtype Dimensionality

  /// Create a scalar in the real vector space that the type represents.
  ///
  /// - Parameter scalar: the scalar
  init(_ scalar: ScalarElement)

  /// Create an object in the real vector space with the specified
  /// dimensionality by repeatedly filling the object with the specified
  /// value.
  ///
  /// - Parameters:
  ///   - dimensionality: the dimensionality
  ///   - repeatedValue: the value repeat for the specified dimensionality
  init(dimensionality: Dimensionality, repeating repeatedValue: ScalarElement)

  static func + (lhs: Self, rhs: Self) -> Self
  static func + (lhs: Self, rhs: ScalarElement) -> Self
  static func + (lhs: ScalarElement, rhs: Self) -> Self

  static func - (lhs: Self, rhs: Self) -> Self
  static func - (lhs: Self, rhs: ScalarElement) -> Self
  static func - (lhs: ScalarElement, rhs: Self) -> Self

  static func * (lhs: Self, rhs: Self) -> Self
  static func * (lhs: Self, rhs: ScalarElement) -> Self
  static func * (lhs: ScalarElement, rhs: Self) -> Self
}

public extension VectorNumeric {
  static func + (lhs: Self, rhs: ScalarElement) -> Self {
    return lhs + Self(rhs)
  }

  static func + (lhs: ScalarElement, rhs: Self) -> Self {
    return Self(lhs) + rhs
  }

  static func - (lhs: Self, rhs: ScalarElement) -> Self {
    return lhs - Self(rhs)
  }

  static func - (lhs: ScalarElement, rhs: Self) -> Self {
    return Self(lhs) - rhs
  }

  static func * (lhs: Self, rhs: ScalarElement) -> Self {
    return lhs * Self(rhs)
  }

  static func * (lhs: ScalarElement, rhs: Self) -> Self {
    return Self(lhs) * rhs
  }
}

//===----------------------------------------------------------------------===//
// Differential Operators
//===----------------------------------------------------------------------===//

@_transparent @usableFromInline
func _gradientBodyUnreachable() {
  // This implementation is never used, since calls to `Swift.gradient(of:)` are
  // resolved as a special case by the type checker.
  Builtin.staticReport(_trueAfterDiagnostics(), true._value,
    ("internal consistency error: 'gradient(of:)' operation failed to resolve"
      as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

@_transparent @usableFromInline
func _valueAndGradientBodyUnreachable() {
  // This implementation is never used, since calls to
  // `Swift.valueAndGradient(of:)` are resolved as a special case by the type
  // checker.
  Builtin.staticReport(_trueAfterDiagnostics(), true._value,
    ("internal consistency error: 'valueAndGradient(of:)' operation failed to resolve"
     as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, Result>(of function: (T) -> Result) -> (T) -> T
  where T : VectorNumeric, Result : VectorNumeric {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, Result>(
  of function: (T, U) -> Result
) -> (T, U) -> (T, U)
  where T : VectorNumeric, U : VectorNumeric, Result : VectorNumeric,
        T.ScalarElement : FloatingPoint, U.ScalarElement : FloatingPoint {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, V, Result>(
  of function: (T, U, V) -> Result
) -> (T, U, V) -> (T, U, V)
  where T : VectorNumeric, U : VectorNumeric, V : VectorNumeric,
        Result : VectorNumeric, T.ScalarElement : FloatingPoint,
        U.ScalarElement : FloatingPoint, V.ScalarElement : FloatingPoint {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, V, W, Result>(
  of function: (T, U, V, W) -> Result
) -> (T, U, V, W) -> (T, U, V, W)
  where T : VectorNumeric, U : VectorNumeric, V : VectorNumeric,
        W : VectorNumeric, Result : VectorNumeric, T.ScalarElement : FloatingPoint,
        U.ScalarElement : FloatingPoint, V.ScalarElement : FloatingPoint,
        W.ScalarElement : FloatingPoint {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, Result>(
  of function: (T) -> Result
) -> (T) -> (value: Result, gradient: T)
  where T : VectorNumeric, Result : VectorNumeric, T.ScalarElement : FloatingPoint {
  _valueAndGradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, Result>(
  of function: (T, U) -> Result
) -> (T, U) -> (value: Result, gradient: (T, U))
  where T : VectorNumeric, U : VectorNumeric, Result : VectorNumeric,
        T.ScalarElement : FloatingPoint, U.ScalarElement : FloatingPoint {
  _valueAndGradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, V, Result>(
  of function: (T, U, V) -> Result
) -> (T, U, V) -> (value: Result, gradient: (T, U, V))
  where T : VectorNumeric, U : VectorNumeric, V : VectorNumeric,
        Result : VectorNumeric, T.ScalarElement : FloatingPoint,
        U.ScalarElement : FloatingPoint, V.ScalarElement : FloatingPoint {
  _valueAndGradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, V, W, Result>(
  of function: (T, U, V, W) -> Result
) -> (T, U, V, W) -> (value: Result, gradient: (T, U, V, W))
  where T : VectorNumeric, U : VectorNumeric, V : VectorNumeric,
        W : VectorNumeric, Result : VectorNumeric, T.ScalarElement : FloatingPoint,
        U.ScalarElement : FloatingPoint, V.ScalarElement : FloatingPoint,
        W.ScalarElement : FloatingPoint {
  _valueAndGradientBodyUnreachable()
}

//===----------------------------------------------------------------------===//
// Builtins
//===----------------------------------------------------------------------===//

@usableFromInline @_fixed_layout
class _AutoDiffTape<Element> {}
