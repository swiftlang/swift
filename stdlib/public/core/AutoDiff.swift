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
  associatedtype TangentVector : AdditiveArithmetic
  /// The cotangent bundle of this differentiable manifold.
  associatedtype CotangentVector : AdditiveArithmetic
  /// The type of all differentiable variables in this type.
  associatedtype AllDifferentiableVariables : Differentiable

  /// All differentiable variables of this value.
  var allDifferentiableVariables: AllDifferentiableVariables { get set }

  /// Returns `self` moved along the value space towards the given tangent
  /// vector. In Riemannian geometry (mathematics), this represents an
  /// exponential map.
  func moved(along direction: TangentVector) -> Self

  /// Convert a cotangent vector to its corresponding tangent vector.
  func tangentVector(from cotangent: CotangentVector) -> TangentVector
}

/// A type that mathematically represents a differentiable manifold whose
/// tangent spaces are finite-dimensional.
///
/// - Note: Do not use this protocol directly. Use `Differentiable` instead.
///
// TODO(TF-213): Merge this into `Differentiable` when the generic signature
// minimization bug (SR-9595) is fixed.
public protocol _Differentiable : __Differentiable
  where TangentVector : Differentiable, CotangentVector : Differentiable {
}

/// A type that mathematically represents a differentiable manifold whose
/// tangent spaces are finite-dimensional.
// BEGIN DIFFERENTIABLE
// - Note: these marks are identified during API doc generation and the
//   contents are replaced with the ideal `Differentiable` protocol design.
public protocol Differentiable : _Differentiable
  where TangentVector.TangentVector == TangentVector,
        TangentVector.CotangentVector == CotangentVector,
        CotangentVector.TangentVector == CotangentVector,
        CotangentVector.CotangentVector == TangentVector,
        AllDifferentiableVariables.AllDifferentiableVariables ==
          AllDifferentiableVariables,
        AllDifferentiableVariables.TangentVector == TangentVector,
        AllDifferentiableVariables.CotangentVector == CotangentVector {
}
// END DIFFERENTIABLE

public extension Differentiable where AllDifferentiableVariables == Self {
  var allDifferentiableVariables: AllDifferentiableVariables {
    get { return self }
    set { self = newValue }
  }
}

// FIXME: The `Self : AdditiveArithmetic` constraint should be implied by
// `TangentVector == Self`, but the type checker errors out when it does not
// exist.
public extension Differentiable
  where TangentVector == Self, Self : AdditiveArithmetic {
  func moved(along direction: TangentVector) -> Self {
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
           -> (value: R, pullback: (R.CotangentVector) -> T.CotangentVector)
) -> @differentiable (T) -> R {
  func original(_ x: T) -> R {
    return vjp(x).value
  }
  @differentiating(original)
  func derivative(_ x: T)
    -> (value: R, pullback: (R.CotangentVector) -> T.CotangentVector) {
    return vjp(x)
  }
  return original
}

/// Create a differentiable function from a vector-Jacobian products function.
@inlinable
public func differentiableFunction<T, U, R>(
  from vjp: @escaping (T, U)
           -> (value: R, pullback: (R.CotangentVector)
             -> (T.CotangentVector, U.CotangentVector))
) -> @differentiable (T, U) -> R
  where T : Differentiable, U : Differentiable, R : Differentiable {
  func original(_ x: T, _ y: U) -> R {
    return vjp(x, y).value
  }
  @differentiating(original)
  func derivative(_ x: T, _ y: U)
    -> (value: R,
        pullback: (R.CotangentVector)
                    -> (T.CotangentVector, U.CotangentVector)) {
    return vjp(x, y)
  }
  return original
}

public extension Differentiable {
  @differentiable(wrt: self, vjp: _vjpWithGrad)
  func withGradient(_ body: @escaping (inout CotangentVector) -> Void) -> Self {
    return self
  }

  @inlinable
  internal func _vjpWithGrad(
    _ body: @escaping (inout CotangentVector) -> Void
  ) -> (Self, (CotangentVector) -> CotangentVector) {
    return (self, { grad in
      var grad = grad
      body(&grad)
      return grad
    })
  }

  @differentiable(wrt: self, vjp: _vjpWithGrad)
  func withGradient(_ body: @escaping (CotangentVector) -> Void) -> Self {
    return self
  }

  @inlinable
  internal func _vjpWithGrad(
    _ body: @escaping (CotangentVector) -> Void
  ) -> (Self, (CotangentVector) -> CotangentVector) {
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
  ) -> (Result, (Result.CotangentVector) -> CotangentVector) {
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
  ) -> (value: R, pullback: (R.CotangentVector) -> CotangentVector) {
    return Builtin.autodiffApply_vjp_arity1(f, self)
  }

  @inlinable
  func pullback<R : Differentiable>(
    in f: @differentiable (Self) -> R
  ) -> (R.CotangentVector) -> CotangentVector {
    return Builtin.autodiffApply_vjp_arity1(f, self).1
  }

  @inlinable
  func gradient<R : Differentiable>(
    in f: @differentiable (Self) -> R
  ) -> CotangentVector
    where R : FloatingPoint, R.CotangentVector == R {
    return self.pullback(in: f)(R(1))
  }

  @inlinable
  func valueWithGradient<R : Differentiable>(
    in f: @differentiable (Self) -> R
  ) -> (value: R, gradient: CotangentVector)
    where R : FloatingPoint, R.CotangentVector == R {
    let (y, pb) = self.valueWithPullback(in: f)
    return (y, pb(R(1)))
  }

  @inlinable
  func valueWithPullback<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (value: R,
        pullback: (R.CotangentVector) -> (CotangentVector, T.CotangentVector)) {
    return Builtin.autodiffApply_vjp_arity2(f, self, x)
  }

  @inlinable
  func pullback<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (R.CotangentVector) -> (CotangentVector, T.CotangentVector) {
    return Builtin.autodiffApply_vjp_arity2(f, self, x).1
  }

  @inlinable
  func gradient<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (CotangentVector, T.CotangentVector)
    where R : FloatingPoint, R.CotangentVector == R {
    return self.pullback(at: x, in: f)(R(1))
  }

  @inlinable
  func valueWithGradient<T : Differentiable, R : Differentiable>(
    at x: T, in f: @differentiable (Self, T) -> R
  ) -> (value: R, gradient: (CotangentVector, T.CotangentVector))
    where R : FloatingPoint, R.CotangentVector == R {
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
) -> (value: R, pullback: (R.CotangentVector) -> T.CotangentVector)
  where T : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp(f, x)
}

@inlinable
public func valueWithPullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R,
      pullback: (R.CotangentVector) -> (T.CotangentVector, U.CotangentVector))
  where T : Differentiable, U : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp_arity2(f, x, y)
}

@inlinable
public func valueWithPullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R,
      pullback: (R.CotangentVector)
        -> (T.CotangentVector, U.CotangentVector, V.CotangentVector))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : Differentiable {
  return Builtin.autodiffApply_vjp_arity3(f, x, y, z)
}

// Pullback

@inlinable
public func pullback<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (R.CotangentVector) -> T.CotangentVector
  where T : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp(f, x).1
}

@inlinable
public func pullback<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (R.CotangentVector) -> (T.CotangentVector, U.CotangentVector)
  where T : Differentiable, U : Differentiable, R : Differentiable {
  return Builtin.autodiffApply_vjp_arity2(f, x, y).1
}

@inlinable
public func pullback<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (R.CotangentVector)
    -> (T.CotangentVector, U.CotangentVector, V.CotangentVector)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : Differentiable {
  return Builtin.autodiffApply_vjp_arity3(f, x, y, z).1
}

// Value with gradient

@inlinable
public func valueWithGradient<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, gradient: T.CotangentVector)
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (value: R, gradient: (T.CotangentVector, U.CotangentVector))
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable, R.CotangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, y, in: f)
  return (y, pullback(R(1)))
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (value: R,
      gradient: (T.CotangentVector, U.CotangentVector, V.CotangentVector))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable, R.CotangentVector == R {
  let (y, pullback) = valueWithPullback(at: x, y, z, in: f)
  return (y, pullback(R(1)))
}

// Value with gradient (curried)

@inlinable
public func valueWithGradient<T, R>(
  of f: @escaping @differentiable (T) -> R
) -> (T) -> (value: R, gradient: T.CotangentVector)
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  return { x in valueWithGradient(at: x, in: f) }
}

@inlinable
public func valueWithGradient<T, U, R>(
  of f: @escaping @differentiable (T, U) -> R
) -> (T, U) -> (value: R, gradient: (T.CotangentVector, U.CotangentVector))
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  return { x, y in valueWithGradient(at: x, y, in: f) }
}

@inlinable
public func valueWithGradient<T, U, V, R>(
  of f: @escaping @differentiable (T, U, V) -> R
) -> (T, U, V)
    -> (value: R,
        gradient: (T.CotangentVector, U.CotangentVector, V.CotangentVector))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  return { x, y, z in valueWithGradient(at: x, y, z, in: f) }
}

// Gradient

@inlinable
public func gradient<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> T.CotangentVector
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  return pullback(at: x, in: f)(R(1))
}

@inlinable
public func gradient<T, U, R>(
  at x: T, _ y: U, in f: @differentiable (T, U) -> R
) -> (T.CotangentVector, U.CotangentVector)
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable, R.CotangentVector == R {
  return pullback(at: x, y, in: f)(R(1))
}

@inlinable
public func gradient<T, U, V, R>(
  at x: T, _ y: U, _ z: V, in f: @differentiable (T, U, V) -> R
) -> (T.CotangentVector, U.CotangentVector, V.CotangentVector)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable, R.CotangentVector == R {
  return pullback(at: x, y, z, in: f)(R(1))
}

// Gradient (curried)

@inlinable
public func gradient<T, R>(
  of f: @escaping @differentiable (T) -> R
) -> (T) -> T.CotangentVector
  where T : Differentiable, R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  return { x in gradient(at: x, in: f) }
}

@inlinable
public func gradient<T, U, R>(
  of f: @escaping @differentiable (T, U) -> R
) -> (T, U) -> (T.CotangentVector, U.CotangentVector)
  where T : Differentiable, U : Differentiable,
        R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  return { x, y in gradient(at: x, y, in: f) }
}

@inlinable
public func gradient<T, U, V, R>(
  of f: @escaping @differentiable (T, U, V) -> R
) -> (T, U, V) -> (T.CotangentVector, U.CotangentVector, V.CotangentVector)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        R : FloatingPoint & Differentiable,
        R.CotangentVector == R {
  return { x, y, z in gradient(at: x, y, z, in: f) }
}

//===----------------------------------------------------------------------===//
// Type-erased `AnyDerivative`
//===----------------------------------------------------------------------===//

@usableFromInline
internal protocol AnyDerivativeBase {
  // `Equatable` requirements (implied by `AdditiveArithmetic`).
  func isEqual(_ other: AnyDerivativeBase) -> Bool
  func isNotEqual(_ other: AnyDerivativeBase) -> Bool

  // `AdditiveArithmetic` requirements.
  static var zero: AnyDerivativeBase { get }
  func plus(_ x: AnyDerivativeBase) -> AnyDerivativeBase
  mutating func plusEqual(_ x: AnyDerivativeBase)
  func minus(_ x: AnyDerivativeBase) -> AnyDerivativeBase
  mutating func minusEqual(_ x: AnyDerivativeBase)

  // `Differentiable` requirements.
  var allDifferentiableVariables: AnyDerivativeBase { get }
  func moved(along direction: AnyDerivativeBase) -> AnyDerivativeBase
  func tangentVector(from cotangent: AnyDerivativeBase) -> AnyDerivativeBase
}

@usableFromInline
internal struct AnyDerivativeBox<T : Differentiable> : AnyDerivativeBase
  where T.TangentVector == T, T.AllDifferentiableVariables == T,
        // NOTE: The requirement below should be defined on `Differentiable`.
        // But it causes a crash due to generic signature minimization bug.
        T.CotangentVector == T.CotangentVector.AllDifferentiableVariables
{
  /// The underlying value.
  var concrete: T

  public init(_ concrete: T) {
    self.concrete = concrete
  }

  // `Equatable` requirements (implied by `AdditiveArithmetic`).

  @usableFromInline
  func isEqual(_ other: AnyDerivativeBase) -> Bool {
    // 0 == 0 => true
    if T.self == AnyDerivative.Zero.self, other is AnyDerivativeBox<AnyDerivative.Zero> {
      return true
    }
    if T.self == AnyDerivative.Zero.self {
      return type(of: other).zero.isEqual(other)
    }
    if other is AnyDerivativeBox<AnyDerivative.Zero> {
      return AnyDerivativeBox<T>(T.zero).isEqual(self)
    }
    guard let other = other as? AnyDerivativeBox<T> else {
      fatalError()
    }
    return concrete == other.concrete
  }

  @usableFromInline
  func isNotEqual(_ other: AnyDerivativeBase) -> Bool {
    // 0 != 0 => false
    if T.self == AnyDerivative.Zero.self,
       other is AnyDerivativeBox<AnyDerivative.Zero> {
      return false
    }
    if T.self == AnyDerivative.Zero.self {
      return type(of: other).zero.isNotEqual(other)
    }
    if other is AnyDerivativeBox<AnyDerivative.Zero> {
      return AnyDerivativeBox<T>(T.zero).isNotEqual(self)
    }
    guard let other = other as? AnyDerivativeBox<T> else {
      fatalError()
    }
    return concrete != other.concrete
  }

  // `AdditiveArithmetic` requirements.

  @usableFromInline
  static var zero: AnyDerivativeBase {
    return AnyDerivativeBox(T.zero)
  }

  @usableFromInline
  func plus(_ x: AnyDerivativeBase) -> AnyDerivativeBase {
    // 0 + x = x
    if T.self == AnyDerivative.Zero.self {
      return x
    }
    // y + 0 = y
    if x is AnyDerivativeBox<AnyDerivative.Zero> {
      return self
    }
    guard let x = x as? AnyDerivativeBox<T> else {
      fatalError()
    }
    return AnyDerivativeBox(concrete + x.concrete)
  }

  @usableFromInline
  mutating func plusEqual(_ x: AnyDerivativeBase) {
    if T.self == AnyDerivative.Zero.self {
      // FIXME: How to mutate `AnyDerivativeBox<AnyDerivative.Zero>`?
      // That would require changing `T` generic parameter.
      return
    }
    if x is AnyDerivativeBox<AnyDerivative.Zero> {
      return
    }
    guard let x = x as? AnyDerivativeBox<T> else {
      fatalError()
    }
    concrete += x.concrete
  }

  @usableFromInline
  func minus(_ x: AnyDerivativeBase) -> AnyDerivativeBase {
    // 0 - 0 = 0
    if T.self == AnyDerivative.Zero.self, x is AnyDerivativeBox<AnyDerivative.Zero> {
      return self
    }
    // 0 - x = x - x - x = -x
    if T.self == AnyDerivative.Zero.self {
      // TODO: Find a more efficient calculation.
      return x.minus(x).minus(x)
    }
    // y - 0 = y
    if x is AnyDerivativeBox<AnyDerivative.Zero> {
      return self
    }
    guard let x = x as? AnyDerivativeBox<T> else {
      fatalError()
    }
    return AnyDerivativeBox(concrete - x.concrete)
  }

  @usableFromInline
  mutating func minusEqual(_ x: AnyDerivativeBase) {
    if T.self == AnyDerivative.Zero.self {
      // FIXME: How to mutate `AnyDerivativeBox<AnyDerivative.Zero>`?
      // That would require changing `T` generic parameter.
      return
    }
    if x is AnyDerivativeBox<AnyDerivative.Zero> {
      return
    }
    guard let x = x as? AnyDerivativeBox<T> else {
      fatalError()
    }
    concrete -= x.concrete
  }

  // `Differentiable` requirements.

  @usableFromInline
  var allDifferentiableVariables: AnyDerivativeBase {
    if T.self == AnyDerivative.Zero.self {
      return self
    }
    return AnyDerivativeBox(concrete.allDifferentiableVariables)
  }

  @usableFromInline
  func moved(along direction: AnyDerivativeBase) -> AnyDerivativeBase {
    if T.self == AnyDerivative.Zero.self {
      return direction
    }
    if direction is AnyDerivativeBox<AnyDerivative.Zero> {
      return self
    }
    guard let direction = direction as? AnyDerivativeBox<T.TangentVector> else {
      fatalError()
    }
    return AnyDerivativeBox(concrete.moved(along: direction.concrete))
  }

  @usableFromInline
  func tangentVector(from cotangent: AnyDerivativeBase) -> AnyDerivativeBase {
    if T.self == AnyDerivative.Zero.self {
      return cotangent
    }
    if cotangent is AnyDerivativeBox<AnyDerivative.Zero> {
      return cotangent
    }
    guard let cotangent = cotangent as? AnyDerivativeBox<T.CotangentVector> else {
      fatalError()
    }
    return AnyDerivativeBox(concrete.tangentVector(from: cotangent.concrete))
  }
}

/// A type-erased derivative.
///
/// This type forwards its operations to an arbitrary underlying base
/// derivative conforming to `Differentiable` and `AdditiveArithmetic`, hiding
/// the specifics of the underlying derivative.
@_fixed_layout
public struct AnyDerivative : Differentiable & AdditiveArithmetic {
  @usableFromInline
  internal var base: AnyDerivativeBase

  @usableFromInline
  internal init(base: AnyDerivativeBase) {
    self.base = base
  }

  /// Creates a type-erased derivative from the given derivative.
  public init<T : Differentiable>(_ base: T)
    where T.TangentVector == T, T.AllDifferentiableVariables == T,
          // NOTE: The requirement below should be defined on `Differentiable`.
          // But it causes a crash due to generic signature minimization bug.
          T.CotangentVector == T.CotangentVector.AllDifferentiableVariables
  {
    self.base = AnyDerivativeBox<T>(base)
  }

  public typealias TangentVector = AnyDerivative
  public typealias CotangentVector = AnyDerivative
  public typealias AllDifferentiableVariables = AnyDerivative

  // `Equatable` requirements (implied by `AdditiveArithmetic`).
  public static func == (lhs: AnyDerivative, rhs: AnyDerivative) -> Bool {
    return lhs.base.isEqual(rhs.base)
  }
  public static func != (lhs: AnyDerivative, rhs: AnyDerivative) -> Bool {
    return lhs.base.isNotEqual(rhs.base)
  }

  // `AdditiveArithmetic` requirements.
  /// Internal struct representing the untyped value zero.
  @_fixed_layout
  @usableFromInline
  internal struct Zero : Differentiable & AdditiveArithmetic {}

  public static var zero: AnyDerivative {
    return AnyDerivative(base: AnyDerivativeBox<Zero>(Zero.zero))
  }
  public static func +(lhs: AnyDerivative, rhs: AnyDerivative) -> AnyDerivative {
    return AnyDerivative(base: lhs.base.plus(rhs.base))
  }
  public static func +=(lhs: inout AnyDerivative, rhs: AnyDerivative) {
    lhs.base.plusEqual(rhs.base)
  }
  public static func -(lhs: AnyDerivative, rhs: AnyDerivative) -> AnyDerivative {
    return AnyDerivative(base: lhs.base.minus(rhs.base))
  }
  public static func -=(lhs: inout AnyDerivative, rhs: AnyDerivative) {
    lhs.base.minusEqual(rhs.base)
  }

  // `Differentiable` requirements.
  public var allDifferentiableVariables: AllDifferentiableVariables {
    get { return AnyDerivative(base: base.allDifferentiableVariables) }
    // set { base.allDifferentiableVariables = newValue.base }
  }
  public func moved(along direction: TangentVector) -> AnyDerivative {
    return AnyDerivative(base: base.moved(along: direction.base))
  }
  public func tangentVector(from cotangent: CotangentVector) -> TangentVector {
    return AnyDerivative(base: base.tangentVector(from: cotangent.base))
  }
}

//===----------------------------------------------------------------------===//
// Builtins
//===----------------------------------------------------------------------===//

@usableFromInline @_fixed_layout
class _AutoDiffTape<Element> {}
