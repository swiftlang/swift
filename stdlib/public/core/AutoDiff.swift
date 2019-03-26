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

internal protocol _AnyDerivativeBox {
  // `Equatable` requirements (implied by `AdditiveArithmetic`).
  func _isEqual(to other: _AnyDerivativeBox) -> Bool
  func _isNotEqual(to other: _AnyDerivativeBox) -> Bool

  // `AdditiveArithmetic` requirements.
  static var _zero: _AnyDerivativeBox { get }
  static var _dualSpaceZero: _AnyDerivativeBox { get }
  func _adding(_ x: _AnyDerivativeBox) -> _AnyDerivativeBox
  func _subtracting(_ x: _AnyDerivativeBox) -> _AnyDerivativeBox

  // `Differentiable` requirements.
  var _allDifferentiableVariables: _AnyDerivativeBox { get }
  func _moved(along direction: _AnyDerivativeBox) -> _AnyDerivativeBox
  func _tangentVector(from cotangent: _AnyDerivativeBox) -> _AnyDerivativeBox

  /// The underlying base value, type-erased to `Any`.
  var _typeErasedBase: Any { get }

  /// Returns the underlying value unboxed to the given type, if possible.
  func _unboxed<U>(to type: U.Type) -> U?
    where U : Differentiable, U.TangentVector == U,
          U.AllDifferentiableVariables == U,
          // NOTE: The requirement below should be defined on `Differentiable`.
          // But it causes a crash due to generic signature minimization bug.
          U.CotangentVector == U.CotangentVector.AllDifferentiableVariables
}

extension _AnyDerivativeBox {
  /// Returns true if the underlying value has type `AnyDerivative.OpaqueZero`.
  func _isOpaqueZero() -> Bool {
    return _unboxed(to: AnyDerivative.OpaqueZero.self) != nil
  }
}

@inline(never)
@usableFromInline
internal func _derivativeTypeMismatch(
  _ x: Any.Type, _ y: Any.Type, file: StaticString = #file, line: UInt = #line
) -> Never {
  preconditionFailure("""
    Derivative type mismatch: \
    \(String(reflecting: x)) and \(String(reflecting: y))
    """, file: file, line: line)
}

internal struct _ConcreteDerivativeBox<T> : _AnyDerivativeBox
  where T : Differentiable, T.TangentVector == T,
        T.AllDifferentiableVariables == T,
        // NOTE: The requirement below should be defined on `Differentiable`.
        // But it causes a crash due to generic signature minimization bug.
        T.CotangentVector == T.CotangentVector.AllDifferentiableVariables
{
  /// The underlying base value.
  var _base: T

  init(_ base: T) {
    self._base = base
  }

  /// The underlying base value, type-erased to `Any`.
  var _typeErasedBase: Any {
    return _base
  }

  func _unboxed<U>(to type: U.Type) -> U?
    where U : Differentiable, U.TangentVector == U,
          U.AllDifferentiableVariables == U,
          // NOTE: The requirement below should be defined on `Differentiable`.
          // But it causes a crash due to generic signature minimization bug.
          U.CotangentVector == U.CotangentVector.AllDifferentiableVariables
  {
    return (self as? _ConcreteDerivativeBox<U>)?._base
  }

  // `Equatable` requirements (implied by `AdditiveArithmetic`).

  func _isEqual(to other: _AnyDerivativeBox) -> Bool {
    return _base == other._unboxed(to: T.self)
  }

  func _isNotEqual(to other: _AnyDerivativeBox) -> Bool {
    return _base != other._unboxed(to: T.self)
  }

  // `AdditiveArithmetic` requirements.

  static var _zero: _AnyDerivativeBox {
    return _ConcreteDerivativeBox(T.zero)
  }

  static var _dualSpaceZero: _AnyDerivativeBox {
    return _ConcreteDerivativeBox<T.CotangentVector>(T.CotangentVector.zero)
  }

  func _adding(_ x: _AnyDerivativeBox) -> _AnyDerivativeBox {
    // 0 + x = x
    if _isOpaqueZero() {
      return x
    }
    // y + 0 = y
    if x._isOpaqueZero() {
      return self
    }
    guard let xBase = x._unboxed(to: T.self) else {
      _derivativeTypeMismatch(T.self, type(of: x._typeErasedBase))
    }
    return _ConcreteDerivativeBox(_base + xBase)
  }

  func _subtracting(_ x: _AnyDerivativeBox) -> _AnyDerivativeBox {
    // y - 0 = y
    if x._isOpaqueZero() {
      return self
    }
    // 0 - x = -x
    if _isOpaqueZero() {
      return type(of: x)._zero._subtracting(x)
    }
    guard let xBase = x._unboxed(to: T.self) else {
      _derivativeTypeMismatch(T.self, type(of: x._typeErasedBase))
    }
    return _ConcreteDerivativeBox(_base - xBase)
  }

  // `Differentiable` requirements.

  var _allDifferentiableVariables: _AnyDerivativeBox {
    return _ConcreteDerivativeBox(_base.allDifferentiableVariables)
  }

  func _moved(along direction: _AnyDerivativeBox) -> _AnyDerivativeBox {
    if _isOpaqueZero() {
      return direction
    }
    if direction._isOpaqueZero() {
      return self
    }
    guard let directionBase =
      direction._unboxed(to: T.TangentVector.self) else {
      _derivativeTypeMismatch(T.self, type(of: direction._typeErasedBase))
    }
    return _ConcreteDerivativeBox<T>(_base.moved(along: directionBase))
  }

  func _tangentVector(from cotangent: _AnyDerivativeBox) -> _AnyDerivativeBox {
    if _isOpaqueZero() {
      return type(of: cotangent)._dualSpaceZero._tangentVector(from: cotangent)
    }
    if cotangent._isOpaqueZero() {
      return cotangent
    }
    guard let cotangentBase =
      cotangent._unboxed(to: T.CotangentVector.self) else {
      _derivativeTypeMismatch(T.self, type(of: cotangent._typeErasedBase))
    }
    return _ConcreteDerivativeBox<T.TangentVector>(
      _base.tangentVector(from: cotangentBase))
  }
}

/// A type-erased derivative value.
///
/// The `AnyDerivative` type forwards its operations to an arbitrary underlying
/// base derivative value conforming to `Differentiable` and
/// `AdditiveArithmetic`, hiding the specifics of the underlying value.
public struct AnyDerivative : Differentiable & AdditiveArithmetic {
  internal var _box: _AnyDerivativeBox

  internal init(_box: _AnyDerivativeBox) {
    self._box = _box
  }

  /// The underlying base value.
  public var base: Any {
    return _box._typeErasedBase
  }

  /// Creates a type-erased derivative from the given derivative.
  public init<T>(_ base: T)
    where T : Differentiable, T.TangentVector == T,
          T.AllDifferentiableVariables == T,
          // NOTE: The requirement below should be defined on `Differentiable`.
          // But it causes a crash due to generic signature minimization bug.
          T.CotangentVector == T.CotangentVector.AllDifferentiableVariables
  {
    self._box = _ConcreteDerivativeBox<T>(base)
  }

  public typealias TangentVector = AnyDerivative
  public typealias CotangentVector = AnyDerivative
  public typealias AllDifferentiableVariables = AnyDerivative

  // `Equatable` requirements (implied by `AdditiveArithmetic`).
  public static func == (lhs: AnyDerivative, rhs: AnyDerivative) -> Bool {
    return lhs._box._isEqual(to: rhs._box)
  }
  public static func != (lhs: AnyDerivative, rhs: AnyDerivative) -> Bool {
    return lhs._box._isNotEqual(to: rhs._box)
  }

  // `AdditiveArithmetic` requirements.

  /// Internal struct representing an opaque zero value.
  @_fixed_layout
  @usableFromInline
  internal struct OpaqueZero : Differentiable & AdditiveArithmetic {}

  public static var zero: AnyDerivative {
    return AnyDerivative(
      _box: _ConcreteDerivativeBox<OpaqueZero>(OpaqueZero.zero))
  }
  public static func + (
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> AnyDerivative {
    return AnyDerivative(_box: lhs._box._adding(rhs._box))
  }
  public static func - (
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> AnyDerivative {
    return AnyDerivative(_box: lhs._box._subtracting(rhs._box))
  }

  // `Differentiable` requirements.
  public var allDifferentiableVariables: AllDifferentiableVariables {
    get { return AnyDerivative(_box: _box._allDifferentiableVariables) }
    // set { _box._allDifferentiableVariables = newValue._box }
  }
  public func moved(along direction: TangentVector) -> AnyDerivative {
    return AnyDerivative(_box: _box._moved(along: direction._box))
  }
  public func tangentVector(from cotangent: CotangentVector) -> TangentVector {
    return AnyDerivative(_box: _box._tangentVector(from: cotangent._box))
  }
}

//===----------------------------------------------------------------------===//
// Builtins
//===----------------------------------------------------------------------===//

@usableFromInline @_fixed_layout
class _AutoDiffTape<Element> {}
