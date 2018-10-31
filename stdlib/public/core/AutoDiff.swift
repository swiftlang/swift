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
/// elements in this vector space and with a specific shape.
public protocol VectorNumeric : AdditiveArithmetic {
  /// The type of scalars in the vector space.
  associatedtype Scalar : AdditiveArithmetic

  /// The type whose values specifies the dimensionality of an object in the
  /// vector space.
  associatedtype Shape

  // TODO: This is a degenerate API. To be removed.
  init(_ scalar: Scalar)

  /// Create an object in the vector space with the specified shape by
  /// repeatedly filling the object with the specified scalar value.
  ///
  /// - Parameters:
  ///   - shape: the shape
  ///   - repeatedValue: the value repeat for the specified shape
  init(repeating repeatedValue: Scalar, shape: Shape)

  static func * (lhs: Scalar, rhs: Self) -> Self
  static func *= (lhs: inout Self, rhs: Scalar)
}

public extension VectorNumeric {
  static func * (lhs: Self, rhs: Scalar) -> Self {
    return rhs * lhs
  }

  static func *= (lhs: inout Self, rhs: Scalar) {
    lhs = rhs * lhs
  }
}

/// A type that mathematically represents a differentiable manifold whose
/// tangent spaces are finite-dimensional.
///
/// In automatic differentiation, differentiation will produce a Jacobian whose
/// elements are of `Tangent` type.
public protocol Differentiable {
  /// The tangent vector space of this differentiable manifold.
  associatedtype TangentVector : Differentiable
    where TangentVector.TangentVector == TangentVector
  /// The cotangent space of this differentiable manifold.
  associatedtype CotangentVector : Differentiable
    where CotangentVector.CotangentVector == CotangentVector

  /// Returns `self` moved along the value space towards the given tangent
  /// vector. In Riemannian geometry (mathematics), this represents an
  /// exponential map.
  func moved(toward direction: TangentVector) -> Self

  /// Convert a cotangent vector to its corresponding tangent vector.
  func tangentVector(from cotangent: CotangentVector) -> TangentVector
}

public extension Differentiable
  where Self : VectorNumeric, TangentVector == Self {
  func moved(toward direction: TangentVector) -> Self {
    return self + direction
  }
}

public extension Differentiable where TangentVector == CotangentVector {
  func tangentVector(from cotangent: CotangentVector) -> TangentVector {
    return cotangent
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
    ("""
     internal consistency error: 'valueAndGradient(of:)' operation failed to \
     resolve
     """ as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent @_semantics("typechecker.gradient(of:)")
public func gradient<T, R>(of function: (T) -> R) -> (T) -> T.CotangentVector
  where T : Differentiable, R : Differentiable {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, R>(
  of function: (T, U) -> R
) -> (T, U) -> (T.CotangentVector, U.CotangentVector)
  where T : Differentiable, U : Differentiable,
        R : Differentiable {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, V, R>(
  of function: (T, U, V) -> R
) -> (T, U, V) -> (T.CotangentVector, U.CotangentVector, V.CotangentVector)
  where T : Differentiable, U : Differentiable,
        V : Differentiable, R : Differentiable {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, V, W, R>(
  of function: (T, U, V, W) -> R
) -> (T, U, V, W) -> (T.CotangentVector, U.CotangentVector, V.CotangentVector,
                      W.CotangentVector)
  where T : Differentiable, U : Differentiable,
        V : Differentiable, W : Differentiable,
        R : Differentiable {
  _gradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, R>(
  of function: (T) -> R
) -> (T) -> (value: R, gradient: T.CotangentVector)
  where T : Differentiable, R : Differentiable {
  _valueAndGradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, R>(
  of function: (T, U) -> R
) -> (T, U) -> (value: R, gradient: (T.CotangentVector, U.CotangentVector))
  where T : Differentiable, U : Differentiable,
        R : Differentiable {
  _valueAndGradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, V, R>(
  of function: (T, U, V) -> R
) -> (T, U, V) -> (value: R, gradient: (T.CotangentVector, U.CotangentVector,
                                        V.CotangentVector))
  where T : Differentiable, U : Differentiable,
        V : Differentiable, R : Differentiable {
  _valueAndGradientBodyUnreachable()
}

@inlinable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, V, W, R>(
  of function: (T, U, V, W) -> R
) -> (T, U, V, W)
  -> (value: R, gradient: (T.CotangentVector, U.CotangentVector,
                           V.CotangentVector, W.CotangentVector))
  where T : Differentiable, U : Differentiable,
        V : Differentiable, W : Differentiable,
        R : Differentiable {
  _valueAndGradientBodyUnreachable()
}

//===----------------------------------------------------------------------===//
// Builtins
//===----------------------------------------------------------------------===//

@usableFromInline @_fixed_layout
class _AutoDiffTape<Element> {}
