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
/// elements in this vector space and have either no shape or a static shape.
public protocol VectorNumeric : AdditiveArithmetic {
  /// The type of scalars in the vector space.
  associatedtype Scalar : AdditiveArithmetic

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

/// A type that represents an unranked vector space. Values of this type are
/// elements in this vector space and have a dynamic shape.
public protocol ShapedVectorNumeric : VectorNumeric {
  /// The type whose values specifies the dimensionality of an object in the
  /// vector space.
  associatedtype Shape

  /// Create an object in the vector space with the specified shape by filling
  /// the object with the specified scalar value.
  ///
  /// - Parameters:
  ///   - shape: the shape
  ///   - repeatedValue: the value repeat for the specified shape
  init(repeating repeatedValue: Scalar, shape: Shape)
}

/// A type that mathematically represents a differentiable manifold whose
/// tangent spaces are finite-dimensional.
///
/// - Note: Do not use this protocol directly. Use `Differentiable` instead.
///
// TODO(TF-213): Merge this into `Differentiable` when the generic signature
// minimization bug (SR-9595) is fixed.
public protocol __Differentiable {
  /// The tangent bundle of this differentiable manifold.
  associatedtype Derivative : AdditiveArithmetic
  typealias TangentVector = Derivative
  /// The cotangent bundle of this differentiable manifold.
  associatedtype Gradient : AdditiveArithmetic
  typealias CotangentVector = Gradient

  /// The type of all differentiable variables in this type.
  associatedtype AllDifferentiableVariables : Differentiable

  /// All differentiable variables in this type.
  var allDifferentiableVariables: AllDifferentiableVariables { get set }

  /// Returns `self` moved along the value space towards the given tangent
  /// vector. In Riemannian geometry (mathematics), this represents an
  /// exponential map.
  func moved(along direction: Derivative) -> Self

  /// Convert a cotangent vector to its corresponding tangent vector.
  func tangentVector(from cotangent: Gradient) -> Derivative
}

/// A type that mathematically represents a differentiable manifold whose
/// tangent spaces are finite-dimensional.
///
/// - Note: Do not use this protocol directly. Use `Differentiable` instead.
///
// TODO(TF-213): Merge this into `Differentiable` when the generic signature
// minimization bug (SR-9595) is fixed.
public protocol _Differentiable : __Differentiable
  where Derivative : Differentiable, Gradient : Differentiable {
}

/// A type that mathematically represents a differentiable manifold whose
/// tangent spaces are finite-dimensional.
public protocol Differentiable : _Differentiable
  where Derivative.Derivative == Derivative,
        Derivative.Gradient == Gradient,
        Gradient.Derivative == Gradient,
        Gradient.Gradient == Derivative,
        AllDifferentiableVariables.AllDifferentiableVariables ==
          AllDifferentiableVariables,
        AllDifferentiableVariables.Derivative == Derivative,
        AllDifferentiableVariables.Gradient == Gradient {
}

public extension Differentiable where AllDifferentiableVariables == Self {
  var allDifferentiableVariables: AllDifferentiableVariables {
    get { return self }
    set { self = newValue }
  }
}

// FIXME: The `Self : AdditiveArithmetic` constraint should be implied by
// `Derivative == Self`, but the type checker errors out when it does not
// exist.
public extension Differentiable
  where Derivative == Self, Self : AdditiveArithmetic {
  func moved(along direction: Derivative) -> Self {
    return self + direction
  }
}

public extension Differentiable {
  /// Identity function that stops gradients from propagating.
  @inline(__always)
  @_semantics("autodiff.nonvarying")
  func withoutDerivative() -> Self { return self }
}

//===----------------------------------------------------------------------===//
// Functional utilities
//===----------------------------------------------------------------------===//

/// Create a differentiable function from a vector-Jacobian products function.
@inlinable
public func differentiableFunction<T : Differentiable, R : Differentiable>(
  from vjp: @escaping (T)
           -> (value: R, pullback: (R.Gradient) -> T.Gradient)
) -> @differentiable (T) -> R {
  @differentiable(vjp: _vjp)
  func original(_ x: T) -> R {
    return vjp(x).value
  }
  func _vjp(_ x: T) -> (R, (R.Gradient) -> T.Gradient) {
    return vjp(x)
  }
  return original
}

/// Create a differentiable function from a vector-Jacobian products function.
@inlinable
public func differentiableFunction<T, U, R>(
  from vjp: @escaping (T, U)
           -> (value: R, pullback: (R.Gradient)
             -> (T.Gradient, U.Gradient))
) -> @differentiable (T, U) -> R
  where T : Differentiable, U : Differentiable, R : Differentiable {
  @differentiable(vjp: _vjp)
  func original(_ x: T, _ y: U) -> R {
    return vjp(x, y).value
  }
  func _vjp(_ x: T, _ y: U)
    -> (R, (R.Gradient) -> (T.Gradient, U.Gradient)) {
    return vjp(x, y)
  }
  return original
}

public extension Differentiable {
  @differentiable(wrt: self, vjp: _vjpWithGrad)
  func withGradient(_ body: @escaping (inout Gradient) -> Void) -> Self {
    return self
  }

  @inlinable
  internal func _vjpWithGrad(
    _ body: @escaping (inout Gradient) -> Void
  ) -> (Self, (Gradient) -> Gradient) {
    return (self, { grad in
      var grad = grad
      body(&grad)
      return grad
    })
  }

  @differentiable(wrt: self, vjp: _vjpWithGrad)
  func withGradient(_ body: @escaping (Gradient) -> Void) -> Self {
    return self
  }

  @inlinable
  internal func _vjpWithGrad(
    _ body: @escaping (Gradient) -> Void
  ) -> (Self, (Gradient) -> Gradient) {
    return (self, { grad in
      body(grad)
      return grad
    })
  }
}

/// Make a function be recomputed in its pullback, known as "checkpointing" in
/// traditional automatic differentiation.
@inlinable
public func withRecomputationInPullbacks<T, U>(
  _ body: @escaping @differentiable (T) -> U
) -> @differentiable (T) -> U where T : Differentiable, U : Differentiable {
  return differentiableFunction { x in
    (value: body(x), pullback: { v in pullback(at: x, in: body)(v) })
  }
}

public extension Differentiable {
  @inlinable
  @differentiable(wrt: self, vjp: _vjp_withRecomputationInPullbacks)
  func withRecomputationInPullbacks<Result : Differentiable>(
    _ body: @escaping @differentiable (Self) -> Result
  ) -> Result {
    return body(self)
  }

  @inlinable
  internal func _vjp_withRecomputationInPullbacks<Result : Differentiable>(
    _ body: @escaping @differentiable (Self) -> Result
  ) -> (Result, (Result.Gradient) -> Gradient) {
    return valueWithPullback(in: Swift.withRecomputationInPullbacks(body))
  }
}

//===----------------------------------------------------------------------===//
// Method-style differential operators
//===----------------------------------------------------------------------===//

public extension Differentiable {
  @inlinable
  func valueWithPullback<R : Differentiable>(
    in f: @differentiable (Self) -> R
  ) -> (value: R, pullback: (R.Gradient) -> Gradient) {
    return Builtin.autodiffApply_vjp_arity1(f, self)
  }

  @inlinable
  func pullback<R : Differentiable>(
    in f: @differentiable (Self) -> R
  ) -> (R.Gradient) -> Gradient {
    return Builtin.autodiffApply_vjp_arity1(f, self).1
  }

  @inlinable
  func gradient<R : Differentiable>(
    in f: @differentiable (Self) -> R
  ) -> Gradient
    where R : FloatingPoint, R.Gradient == R {
    return self.pullback(in: f)(R(1))
  }

  @inlinable
  func valueWithGradient<R : Differentiable>(
    in f: @differentiable (Self) -> R
  ) -> (value: R, gradient: Gradient)
    where R : FloatingPoint, R.Gradient == R {
    let (y, pb) = self.valueWithPullback(in: f)
    return (y, pb(R(1)))
  }

  @inlinable
  func valueWithPullback<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (value: R,
        pullback: (R.Gradient) -> (Gradient, T.Gradient)) {
    return Builtin.autodiffApply_vjp_arity2(f, self, x)
  }

  @inlinable
  func pullback<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (R.Gradient) -> (Gradient, T.Gradient) {
    return Builtin.autodiffApply_vjp_arity2(f, self, x).1
  }

  @inlinable
  func gradient<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (Gradient, T.Gradient)
    where R : FloatingPoint, R.Gradient == R {
    return self.pullback(at: x, in: f)(R(1))
  }

  @inlinable
  func valueWithGradient<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (value: R, gradient: (Gradient, T.Gradient))
    where R : FloatingPoint, R.Gradient == R {
    let (y, pb) = self.valueWithPullback(at: x, in: f)
    return (y, pb(R(1)))
  }
}

//===----------------------------------------------------------------------===//
// Free-function-style differential operators
//===----------------------------------------------------------------------===//

// Value with pullback

@inlinable
public func valueWithPullback<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, pullback: (R.Gradient) -> T.Gradient)
  where T : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp(f, x)
}

@inlinable
public func valueWithPullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R,
      pullback: (R.Gradient) -> (T.Gradient, U.Gradient))
  where T : Differentiable, U : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp_arity2(f, x, y)
}

@inlinable
public func valueWithPullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R,
      pullback: (R.Gradient)
        -> (T.Gradient, U.Gradient, V.Gradient))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : Differentiable {
  return Builtin.autodiffApply_vjp_arity3(f, x, y, z)
}

// Pullback

@inlinable
public func pullback<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (R.Gradient) -> T.Gradient
  where T : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp(f, x).1
}

@inlinable
public func pullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (R.Gradient) -> (T.Gradient, U.Gradient)
  where T : Differentiable, U : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp_arity2(f, x, y).1
}

@inlinable
public func pullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (R.Gradient)
    -> (T.Gradient, U.Gradient, V.Gradient)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : Differentiable {
  return Builtin.autodiffApply_vjp_arity3(f, x, y, z).1
}

// Value with gradient

@inlinable
public func valueWithGradient<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, gradient: T.Gradient)
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.Gradient == R {
  let (y, pullback) = valueWithPullback(at: x, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R, gradient: (T.Gradient, U.Gradient))
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable, R.Gradient == R {
  let (y, pullback) = valueWithPullback(at: x, y, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R,
      gradient: (T.Gradient, U.Gradient, V.Gradient))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable, R.Gradient == R {
  let (y, pullback) = valueWithPullback(at: x, y, z, in: f)
  return (y, pullback(R(1)))
}

// Value with gradient (curried)

@inlinable
public func valueWithGradient<T, R>(
  of f: @escaping @differentiable (T) -> R
) -> (T) -> (value: R, gradient: T.Gradient)
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.Gradient == R {
  return { x in valueWithGradient(at: x, in: f) }
}

@inlinable
public func valueWithGradient<T, U, R>(
  of f: @escaping @differentiable (T, U) -> R
) -> (T, U) -> (value: R, gradient: (T.Gradient, U.Gradient))
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable,
        R.Gradient == R {
  return { x, y in valueWithGradient(at: x, y, in: f) }
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  of f: @escaping @differentiable (T, U, V) -> R
) -> (T, U, V)
    -> (value: R,
        gradient: (T.Gradient, U.Gradient, V.Gradient))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable,
        R.Gradient == R {
  return { x, y, z in valueWithGradient(at: x, y, z, in: f) }
}

// Gradient

@inlinable
public func gradient<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> T.Gradient
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.Gradient == R {
  return pullback(at: x, in: f)(R(1))
}

@inlinable
public func gradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (T.Gradient, U.Gradient)
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable, R.Gradient == R {
  return pullback(at: x, y, in: f)(R(1))
}

@inlinable
public func gradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (T.Gradient, U.Gradient, V.Gradient)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable, R.Gradient == R {
  return pullback(at: x, y, z, in: f)(R(1))
}

// Gradient (curried)

@inlinable
public func gradient<T, R>(
  of f: @escaping @differentiable (T) -> R
) -> (T) -> T.Gradient
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.Gradient == R {
  return { x in gradient(at: x, in: f) }
}

@inlinable
public func gradient<T, U, R>(
  of f: @escaping @differentiable (T, U) -> R
) -> (T, U) -> (T.Gradient, U.Gradient)
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable, R.Gradient == R {
  return { x, y in gradient(at: x, y, in: f) }
}

@inlinable
public func gradient<T, U, V, R>(
  of f: @escaping @differentiable (T, U, V) -> R
) -> (T, U, V) -> (T.Gradient, U.Gradient, V.Gradient)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable, R.Gradient == R {
  return { x, y, z in gradient(at: x, y, z, in: f) }
}

//===----------------------------------------------------------------------===//
// Builtins
//===----------------------------------------------------------------------===//

@usableFromInline @_fixed_layout
class _AutoDiffTape<Element> {}
