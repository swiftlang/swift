//===--- DifferentiationSupport.swift -------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// This file defines not-yet-upstreamed support for differentiable programming
// and deep learning APIs.
//
//===----------------------------------------------------------------------===//

infix operator .* : MultiplicationPrecedence
infix operator .*= : AssignmentPrecedence

//===----------------------------------------------------------------------===//
// Compiler Protocols
//===----------------------------------------------------------------------===//

/// A type with values that support pointwise multiplication.
// TODO: Add API documentation.
public protocol PointwiseMultiplicative : AdditiveArithmetic {
  /// The one value.
  ///
  /// One is the identity element for multiplication. For any value,
  /// `x .* .one == x` and `.one .* x == x`.
  static var one: Self { get }

  /// The multiplicative inverse of self.
  ///
  /// For any value, `x .* x.reciprocal == .one` and
  /// `x.reciprocal .* x == .one`.
  var reciprocal: Self { get }

  /// Multiplies two values and produces their product.
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  static func .*(lhs: Self, rhs: Self) -> Self

  /// Multiplies two values and produces their product.
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  static func .*=(lhs: inout Self, rhs: Self)
}

public extension PointwiseMultiplicative {
  static func .*=(lhs: inout Self, rhs: Self) {
    lhs = lhs .* rhs
  }
}

public extension PointwiseMultiplicative
  where Self : ExpressibleByIntegerLiteral {
  static var one: Self {
    return 1
  }
}

/// A type that represents an unranked vector space. Values of this type are
/// elements in this vector space and have either no shape or a static shape.
public protocol VectorProtocol : AdditiveArithmetic {
  /// The type of scalars in the vector space.
  associatedtype VectorSpaceScalar : AdditiveArithmetic

  func adding(_ x: VectorSpaceScalar) -> Self

  mutating func add(_ x: VectorSpaceScalar)

  func subtracting(_ x: VectorSpaceScalar) -> Self

  mutating func subtract(_ x: VectorSpaceScalar)

  /// Returns `self` multiplied by the given scalar.
  func scaled(by scalar: VectorSpaceScalar) -> Self

  /// Multiplies `self` by the given scalar.
  mutating func scale(by scalar: VectorSpaceScalar)
}

public extension VectorProtocol {
  mutating func add(_ x: VectorSpaceScalar) {
    self = adding(x)
  }

  mutating func subtract(_ x: VectorSpaceScalar) {
    self = subtracting(x)
  }

  mutating func scale(by scalar: VectorSpaceScalar) {
    self = scaled(by: scalar)
  }
}

/*
// Note: These default-implemented operators will slow down type-checking
// performance and break existing code.

public extension VectorProtocol {
  static func + (lhs: Self, rhs: VectorSpaceScalar) -> Self {
    lhs.adding(rhs)
  }

  static func + (lhs: VectorSpaceScalar, rhs: Self) -> Self {
    rhs.adding(lhs)
  }

  static func += (lhs: inout Self, rhs: VectorSpaceScalar) {
    lhs.add(rhs)
  }

  static func - (lhs: Self, rhs: VectorSpaceScalar) -> Self {
    lhs.subtracting(rhs)
  }

  static func -= (lhs: inout Self, rhs: VectorSpaceScalar) {
    lhs.subtract(rhs)
  }

  static func * (lhs: Self, rhs: VectorSpaceScalar) -> Self {
    lhs.scaled(by: rhs)
  }

  static func * (lhs: VectorSpaceScalar, rhs: Self) -> Self {
    rhs.scaled(by: lhs)
  }

  static func *= (lhs: inout Self, rhs: VectorSpaceScalar) {
    lhs.scale(by: rhs)
  }
}

public extension VectorProtocol where VectorSpaceScalar : SignedNumeric {
  static func - (lhs: VectorSpaceScalar, rhs: Self) -> Self {
    -rhs.adding(lhs)
  }

  static prefix func - (x: Self) -> Self {
    .zero - x
  }
}
*/

/// A type that is differentiable in the Euclidean space.
/// The type may represent a vector space, or consist of a vector space and some
/// other non-differentiable component.
///
/// Mathematically, this represents a product manifold that consists of
/// a differentiable vector space and some arbitrary manifold, where the tangent
/// bundle of the entire product manifold is equal to the vector space
/// component.
///
/// This abstraction is useful for representing common differentiable data
/// structures that contain both differentiable vector properties and other
/// stored properties that do not have a derivative, e.g.
///
/// ```swift
/// struct Perceptron: @memberwise EuclideanDifferentiable {
///     var weight: SIMD16<Float>
///     var bias: Float
///     @noDerivative var useBias: Bool
/// }
/// ```
///
/// - Note: Conform a type to `EuclideanDifferentiable` if it is differentiable
///   only with respect to its vector space component and when its
///   `TangentVector` is equal to its vector space component.
public protocol EuclideanDifferentiable: Differentiable {
  /// The differentiable vector component of `self`.
  var differentiableVectorView: TangentVector { get }
}

public extension EuclideanDifferentiable where TangentVector == Self {
  var differentiableVectorView: TangentVector { _read { yield self } }
}

//===----------------------------------------------------------------------===//
// Functional utilities
//===----------------------------------------------------------------------===//

public extension Differentiable {
  @differentiable(wrt: self)
  func withDerivative(_ body: @escaping (inout TangentVector) -> Void) -> Self {
    return self
  }

  @inlinable
  @derivative(of: withDerivative)
  internal func _vjpWithDerivative(
    _ body: @escaping (inout TangentVector) -> Void
  ) -> (value: Self, pullback: (TangentVector) -> TangentVector) {
    return (self, { grad in
      var grad = grad
      body(&grad)
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
  @differentiable(wrt: self)
  func withRecomputationInPullbacks<Result : Differentiable>(
    _ body: @escaping @differentiable (Self) -> Result
  ) -> Result {
    return body(self)
  }

  @inlinable
  @derivative(of: withRecomputationInPullbacks)
  internal func _vjp_withRecomputationInPullbacks<Result : Differentiable>(
    _ body: @escaping @differentiable (Self) -> Result
  ) -> (value: Result, pullback: (Result.TangentVector) -> TangentVector) {
    return Swift.valueWithPullback(
      at: self, in: Swift.withRecomputationInPullbacks(body)
    )
  }
}

//===----------------------------------------------------------------------===//
// Type-erased `AnyDerivative`
//===----------------------------------------------------------------------===//

@usableFromInline
internal protocol _AnyDerivativeBox {
  // `Equatable` requirements (implied by `AdditiveArithmetic`).
  func _isEqual(to other: _AnyDerivativeBox) -> Bool
  func _isNotEqual(to other: _AnyDerivativeBox) -> Bool

  // `AdditiveArithmetic` requirements.
  static var _zero: _AnyDerivativeBox { get }
  func _adding(_ x: _AnyDerivativeBox) -> _AnyDerivativeBox
  func _subtracting(_ x: _AnyDerivativeBox) -> _AnyDerivativeBox

  // `Differentiable` requirements.
  mutating func _move(along direction: _AnyDerivativeBox)

  // `EuclideanDifferentiable` requirements.
  var _differentiableVectorView: _AnyDerivativeBox { get }

  /// The underlying base value, type-erased to `Any`.
  var _typeErasedBase: Any { get }

  /// Returns the underlying value unboxed to the given type, if possible.
  func _unboxed<U>(to type: U.Type) -> U?
    where U: Differentiable, U.TangentVector == U
}

extension _AnyDerivativeBox {
  /// Returns true if the underlying value has type `AnyDerivative.OpaqueZero`.
  @inlinable
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

@frozen
@usableFromInline
internal struct _ConcreteDerivativeBox<T>: _AnyDerivativeBox
  where T: Differentiable, T.TangentVector == T
{
  /// The underlying base value.
  @usableFromInline
  var _base: T

  @inlinable
  internal init(_ base: T) {
    self._base = base
  }

  /// The underlying base value, type-erased to `Any`.
  @inlinable
  var _typeErasedBase: Any {
    return _base
  }

  @inlinable
  func _unboxed<U>(to type: U.Type) -> U?
    where U: Differentiable, U.TangentVector == U
  {
    return (self as? _ConcreteDerivativeBox<U>)?._base
  }

  // `Equatable` requirements (implied by `AdditiveArithmetic`).
  @inlinable
  func _isEqual(to other: _AnyDerivativeBox) -> Bool {
    return _base == other._unboxed(to: T.self)
  }
  @inlinable
  func _isNotEqual(to other: _AnyDerivativeBox) -> Bool {
    return _base != other._unboxed(to: T.self)
  }

  // `AdditiveArithmetic` requirements.

  @inlinable
  static var _zero: _AnyDerivativeBox {
    return _ConcreteDerivativeBox(T.zero)
  }

  @inlinable
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

  @inlinable
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
  @inlinable
  mutating func _move(along direction: _AnyDerivativeBox) {
    if direction._isOpaqueZero() {
      return
    }
    // The case where `self._isOpaqueZero()` returns true is handled in
    // `AnyDerivative.move(along:)`.
    guard let directionBase =
      direction._unboxed(to: T.TangentVector.self) else {
      _derivativeTypeMismatch(T.self, type(of: direction._typeErasedBase))
    }
    _base.move(along: directionBase)
  }

  // `EuclideanDifferentiable` requirements.
  @inlinable
  var _differentiableVectorView: _AnyDerivativeBox {
    return self
  }
}

/// A type-erased derivative value.
///
/// The `AnyDerivative` type forwards its operations to an arbitrary underlying
/// base derivative value conforming to `Differentiable` and
/// `AdditiveArithmetic`, hiding the specifics of the underlying value.
@frozen
public struct AnyDerivative: EuclideanDifferentiable & AdditiveArithmetic {
  @usableFromInline
  internal var _box: _AnyDerivativeBox

  @inlinable
  internal init(_box: _AnyDerivativeBox) {
    self._box = _box
  }

  /// The underlying base value.
  @inlinable
  public var base: Any {
    return _box._typeErasedBase
  }

  /// Creates a type-erased derivative from the given derivative.
  @inlinable
  @differentiable
  public init<T>(_ base: T) where T: Differentiable, T.TangentVector == T {
    self._box = _ConcreteDerivativeBox<T>(base)
  }

  @inlinable
  @derivative(of: init)
  internal static func _vjpInit<T>(
    _ base: T
  ) -> (value: AnyDerivative, pullback: (AnyDerivative) -> T.TangentVector)
    where T: Differentiable, T.TangentVector == T
  {
    return (AnyDerivative(base), { v in v.base as! T.TangentVector })
  }

  @inlinable
  @derivative(of: init)
  internal static func _jvpInit<T>(
    _ base: T
  ) -> (value: AnyDerivative, differential: (T.TangentVector) -> AnyDerivative)
    where T: Differentiable, T.TangentVector == T
  {
    return (AnyDerivative(base), { dbase in AnyDerivative(dbase) })
  }

  public typealias TangentVector = AnyDerivative

  // `Equatable` requirements (implied by `AdditiveArithmetic`).
  @inlinable
  public static func == (lhs: AnyDerivative, rhs: AnyDerivative) -> Bool {
    return lhs._box._isEqual(to: rhs._box)
  }
  @inlinable
  public static func != (lhs: AnyDerivative, rhs: AnyDerivative) -> Bool {
    return lhs._box._isNotEqual(to: rhs._box)
  }

  // `AdditiveArithmetic` requirements.

  /// Internal struct representing an opaque zero value.
  @frozen
  @usableFromInline
  internal struct OpaqueZero: EuclideanDifferentiable & AdditiveArithmetic {}

  @inlinable
  public static var zero: AnyDerivative {
    return AnyDerivative(
      _box: _ConcreteDerivativeBox<OpaqueZero>(OpaqueZero.zero))
  }

  @inlinable
  public static func + (
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> AnyDerivative {
    return AnyDerivative(_box: lhs._box._adding(rhs._box))
  }

  @derivative(of: +)
  @inlinable
  internal static func _vjpAdd(
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> (value: AnyDerivative,
        pullback: (AnyDerivative) -> (AnyDerivative, AnyDerivative)) {
    return (lhs + rhs, { v in (v, v) })
  }

  @derivative(of: +)
  @inlinable
  internal static func _jvpAdd(
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> (value: AnyDerivative,
    differential: (AnyDerivative, AnyDerivative) -> (AnyDerivative)) {
      return (lhs + rhs, { (dlhs, drhs) in dlhs + drhs })
  }

  @inlinable
  public static func - (
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> AnyDerivative {
    return AnyDerivative(_box: lhs._box._subtracting(rhs._box))
  }

  @derivative(of: -)
  @inlinable
  internal static func _vjpSubtract(
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> (value: AnyDerivative,
        pullback: (AnyDerivative) -> (AnyDerivative, AnyDerivative)) {
    return (lhs - rhs, { v in (v, .zero - v) })
  }

  @derivative(of: -)
  @inlinable
  internal static func _jvpSubtract(
    lhs: AnyDerivative, rhs: AnyDerivative
  ) -> (value: AnyDerivative,
        differential: (AnyDerivative, AnyDerivative) -> AnyDerivative) {
    return (lhs - rhs, { (dlhs, drhs) in dlhs - drhs })
  }

  // `Differentiable` requirements.
  @inlinable
  public mutating func move(along direction: TangentVector) {
    if _box._isOpaqueZero() {
      _box = direction._box
      return
    }
    _box._move(along: direction._box)
  }

  // `EuclideanDifferentiable` requirements.
  @inlinable
  public var differentiableVectorView: TangentVector {
    return self
  }
}

//===----------------------------------------------------------------------===//
// JVP diagnostics
//===----------------------------------------------------------------------===//

@_silgen_name("_printJVPErrorAndExit")
public func _printJVPErrorAndExit() -> Never {
    fatalError("""
        JVP does not exist. Differential-first differentiation APIs are \
        experimental and should not be used.
        """)
}

//===----------------------------------------------------------------------===//
// SIMD differentiation
//===----------------------------------------------------------------------===//

extension SIMD
  where Self : Differentiable,
        TangentVector : SIMD,
        Scalar : BinaryFloatingPoint,
        TangentVector.Scalar : BinaryFloatingPoint {
  @inlinable
  @derivative(of: +)
  static func _vjpAdd(lhs: Self, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs + rhs, { v in
      return (v, v)
    })
  }

  @inlinable
  @derivative(of: -)
  static func _vjpSubtract(lhs: Self, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs - rhs, { v in
      return (v, -v)
    })
  }

  @inlinable
  @derivative(of: -)
  static func _vjpNegate(rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector)) {
    return (-rhs, { v in
      return -v
    })
  }
}

extension SIMD
  where Self : Differentiable,
        TangentVector : SIMD,
        Scalar : BinaryFloatingPoint,
        Self.TangentVector == Self {
  @inlinable
  @derivative(of: *)
  static func _vjpMultiply(lhs: Self, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs * rhs, { v in
      return (v * rhs, v * lhs)
    })
  }

  @inlinable
  @derivative(of: /)
  static func _vjpDivide(lhs: Self, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs / rhs, { v in
      (v / rhs, -lhs / (rhs * rhs) * v)
    })
  }
}

extension SIMD
  where Self : Differentiable,
        TangentVector : SIMD,
        Scalar : BinaryFloatingPoint & Differentiable,
        Scalar.TangentVector : BinaryFloatingPoint,
        TangentVector.Scalar == Scalar.TangentVector {
  @inlinable
  @derivative(of: +)
  static func _vjpAdd(lhs: Scalar, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (Scalar.TangentVector, TangentVector)) {
    return (lhs + rhs, { v in
      return (v.sum(), v)
    })
  }

  @inlinable
  @derivative(of: -)
  static func _vjpSubtract(lhs: Scalar, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (Scalar.TangentVector, TangentVector)) {
    return (lhs - rhs, { v in
      return (v.sum(), -v)
    })
  }

  @inlinable
  @derivative(of: +)
  static func _vjpAdd(lhs: Self, rhs: Scalar)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, Scalar.TangentVector)) {
    return (lhs + rhs, { v in
      return (v, v.sum())
    })
  }

  @inlinable
  @derivative(of: -)
  static func _vjpSubtract(lhs: Self, rhs: Scalar)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, Scalar.TangentVector)) {
    return (lhs - rhs, { v in
      return (v, -v.sum())
    })
  }
}

extension SIMD
  where Self : Differentiable,
        TangentVector : SIMD,
        Scalar : BinaryFloatingPoint & Differentiable,
        Self.TangentVector == Self,
        Scalar.TangentVector == Scalar {
  @inlinable
  @derivative(of: *)
  static func _vjpMultiply(lhs: Self, rhs: Scalar)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, Scalar.TangentVector)) {
    return (lhs * rhs, { v in
      return (v * rhs, (v * lhs).sum())
    })
  }

  @inlinable
  @derivative(of: /)
  static func _vjpDivide(lhs: Self, rhs: Scalar)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, Scalar.TangentVector)) {
    return (lhs / rhs, { v in
      (v / rhs, (-lhs / (rhs * rhs) * v).sum())
    })
  }

  @inlinable
  @derivative(of: *)
  static func _vjpMultiply(lhs: Scalar, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (Scalar.TangentVector, TangentVector)) {
    return (lhs * rhs, { v in
      return ((v * rhs).sum(), v * lhs)
    })
  }

  @inlinable
  @derivative(of: /)
  static func _vjpDivide(lhs: Scalar, rhs: Self)
    -> (value: Self, pullback: (TangentVector) -> (Scalar.TangentVector, TangentVector)) {
    return (lhs / rhs, { v in
      ((v / rhs).sum(), -lhs / (rhs * rhs) * v)
    })
  }
}

extension SIMD
  where Self : Differentiable,
        TangentVector : SIMD,
        Scalar : BinaryFloatingPoint & Differentiable,
        Scalar.TangentVector : BinaryFloatingPoint,
        TangentVector == Self {
  @inlinable
  @derivative(of: sum)
  func _vjpSum() -> (value: Scalar, pullback: (Scalar.TangentVector) -> TangentVector) {
    return (sum(), { v in Self(repeating: Scalar(v)) })
  }
}

extension SIMD
  where Self : Differentiable,
        Self.TangentVector : SIMD,
        Scalar : BinaryFloatingPoint & Differentiable,
        Self.TangentVector == Self,
        Scalar.TangentVector == Scalar {
  @inlinable
  @derivative(of: init(repeating:))
  static func _vjpInit(repeating value: Scalar)
    -> (value: Self, pullback: (TangentVector) -> Scalar.TangentVector) {
      return (Self(repeating: value), { v in v.sum() })
  }
}
