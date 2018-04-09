//===-- Ops.swift ------------------------------------------*- swift -*-===//
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
// This file contains definitions of tensor operations common to TensorProtocol.
//
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// Ops and Convenience Methods
//===----------------------------------------------------------------------===//
//
// The majority of the Tensor API is implemented in terms of 'ops' that are
// partitioned out to the TensorFlow graph when the compiler runs.  These
// ops are intentially designed to reflect TensorFlow ops, but provide nicer
// Swift syntax for accessing them.  In addition to the core ops themselves,
// we also define some helper function wrappers, e.g. to make things symmetric
// and generally feel nice to use.
//
// The ops themselves are defined by the primitive #tfop(...) syntax, here are
// some examples:
//     result = #tfop("Add", lhs, rhs)
//     result = #tfop("Const", dtype: Float.self, value$tensor: 4.0)
//
// The first parameter to this syntax is the TensorFlow op name as a string.
// After that, the inputs are specified, and then attributes are specified
// with their name as the keyword argument.
//
// Inputs and outputs must be of TensorHandle, ResourceHandle, or VariantHandle
// type.  These are magic types known to the compiler.
//

// Python PEP 465 makes a compelling argument that matrix multiplication should
// not be spelled with the standard * operator, so we need a new one.  We'll use
// this operator, though it is defensible to use a variety of other ones as
// well.
infix operator ⊗ : MultiplicationPrecedence

infix operator ++ : AdditionPrecedence

// TODO:
// - Consider explicit broadcasting for elementwise binary ops when
//   scalarization and rank getter are implemented.

//===----------------------------------------------------------------------===//
// Scalar type cast
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : Numeric {
  /// Perform an element-wise type conversion from a `Bool` tensor.
  @_inlineable @inline(__always)
  init(_ other: BoolTensor) {
    self.init(handle: #tfop("Cast", other.handle, DstT: Scalar.self))
  }
}

//===----------------------------------------------------------------------===//
// Element-wise binary arithmetics
//===----------------------------------------------------------------------===//

extension TensorProtocol where Scalar : Numeric {
  /// Adds two tensors and produces their sum.
  /// - Note: `+` supports broadcasting.
  @_inlineable @inline(__always)
  @differentiable(reverse, adjoint: _adjointAdd(_:_:partial:seed:))
  public static func + (lhs: Self, rhs: Self) -> Self {
    return #tfop("Add", lhs, rhs)
  }

  /// Subtracts one tensor from another and produces their difference.
  /// - Note: `-` supports broadcasting.
  @_inlineable @inline(__always)
  @differentiable(reverse, adjoint: _adjointSubtract(_:_:partial:seed:))
  public static func - (lhs: Self, rhs: Self) -> Self {
    return #tfop("Sub", lhs, rhs)
  }

  /// Multiplies two tensors and produces their product.
  /// - Note: `*` supports broadcasting.
  @_inlineable @inline(__always)
  @differentiable(reverse, adjoint: _adjointMultiply(_:_:partial:seed:))
  public static func * (lhs: Self, rhs: Self) -> Self {
    return #tfop("Mul", lhs, rhs)
  }
}

public extension TensorProtocol where Scalar : Numeric {
  /// Adds the scalar to every scalar of the tensor and produces the sum.
  @_inlineable @inline(__always)
  static func + (lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) + rhs
  }

  /// Adds the scalar to every scalar of the tensor and produces the sum.
  @_inlineable @inline(__always)
  static func + (lhs: Self, rhs: Scalar) -> Self {
    return lhs + Self(handle: _TFMakeScalarTensor(rhs))
  }

  /// Adds two tensors and stores the result in the left-hand-side variable.
  /// - Note: `+=` supports broadcasting.
  @_inlineable @inline(__always)
  static func += (lhs: inout Self, rhs: Self) {
    lhs = lhs + rhs
  }

  /// Adds the scalar to every scalar of the tensor and stores the result in the
  /// left-hand-side variable.
  @_inlineable @inline(__always)
  static func += (lhs: inout Self, rhs: Scalar) {
    lhs = lhs + rhs
  }

  /// Subtracts the scalar from every scalar of the tensor and produces the
  /// difference.
  @_inlineable @inline(__always)
  static func - (lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) - rhs
  }

  /// Subtracts the scalar from every scalar of the tensor and produces the
  /// difference.
  @_inlineable @inline(__always)
  static func - (lhs: Self, rhs: Scalar) -> Self {
    return lhs - Self(handle: _TFMakeScalarTensor(rhs))
  }

  /// Subtracts the second tensor from the first and stores the result in the
  /// left-hand-side variable.
  /// - Note: `-=` supports broadcasting.
  @_inlineable @inline(__always)
  static func -= (lhs: inout Self, rhs: Self) {
    lhs = lhs - rhs
  }

  /// Subtracts the scalar from every scalar of the tensor and stores the result
  /// in the left-hand-side variable.
  @_inlineable @inline(__always)
  static func -= (lhs: inout Self, rhs: Scalar) {
    lhs = lhs - rhs
  }

  /// Multiplies the scalar with every scalar of the tensor and produces the
  /// product.
  @_inlineable @inline(__always)
  static func * (lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) * rhs
  }

  /// Multiplies the scalar with every scalar of the tensor and produces the
  /// product.
  @_inlineable @inline(__always)
  static func * (lhs: Self, rhs: Scalar) -> Self {
    return lhs * Self(handle: _TFMakeScalarTensor(rhs))
  }

  /// Multiplies two tensors and stores the result in the left-hand-side
  /// variable.
  /// - Note: `*=` supports broadcasting.
  @_inlineable @inline(__always)
  static func *= (lhs: inout Self, rhs: Self) {
    lhs = lhs * rhs
  }

  /// Multiplies the scalar with every scalar of the tensor and stores the
  /// result in the left-hand-side variable.
  @_inlineable @inline(__always)
  static func *= (lhs: inout Self, rhs: Scalar) {
    lhs = lhs * rhs
  }

  /// Returns the quotient of dividing the first tensor by the second.
  /// - Note: `/` supports broadcasting.
  @_inlineable @inline(__always)
  @differentiable(reverse, adjoint: _adjointDivide(_:_:partial:seed:))
  static func / (lhs: Self, rhs: Self) -> Self {
    return #tfop("Div", lhs, rhs)
  }

  /// Returns the quotient of dividing the scalar by the tensor, broadcasting
  /// the scalar.
  @_inlineable @inline(__always)
  static func / (lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) / rhs
  }

  /// Returns the quotient of dividing the tensor by the scalar, broadcasting
  /// the scalar.
  @_inlineable @inline(__always)
  static func / (lhs: Self, rhs: Scalar) -> Self {
    return lhs / Self(handle: _TFMakeScalarTensor(rhs))
  }

  /// Divides the first tensor by the second and stores the quotient in the
  /// left-hand-side variable.
  @_inlineable @inline(__always)
  static func /= (lhs: inout Self, rhs: Self) {
    lhs = lhs / rhs
  }

  /// Divides the tensor by the scalar, broadcasting the scalar, and stores the
  /// quotient in the left-hand-side variable.
  @_inlineable @inline(__always)
  static func /= (lhs: inout Self, rhs: Scalar) {
    lhs = lhs / rhs
  }

  /// Returns the remainder of dividing the first tensor by the second.
  /// - Note: `%` supports broadcasting.
  @_inlineable @inline(__always)
  static func % (lhs: Self, rhs: Self) -> Self {
    return #tfop("Mod", lhs, rhs)
  }

  /// Returns the remainder of dividing the tensor by the scalar, broadcasting
  /// the scalar.
  @_inlineable @inline(__always)
  static func % (lhs: Self, rhs: Scalar) -> Self {
    return #tfop("Mod", lhs, _TFMakeScalarTensor(rhs))
  }

  /// Returns the remainder of dividing the scalar by the tensor, broadcasting
  /// the scalar.
  @_inlineable @inline(__always)
  static func % (lhs: Scalar, rhs: Self) -> Self {
    return #tfop("Mod", _TFMakeScalarTensor(lhs), rhs)
  }

  /// Divides the first tensor by the second and stores the remainder in the
  /// left-hand-side variable.
  @_inlineable @inline(__always)
  static func %= (lhs: inout Self, rhs: Self) {
    lhs = lhs % rhs
  }

  /// Divides the tensor by the scalar and stores the remainder in the
  /// left-hand-side variable.
  @_inlineable @inline(__always)
  static func %= (lhs: inout Self, rhs: Scalar) {
    lhs = lhs % rhs
  }
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : Numeric {
  /// Performs matrix multiplication with another tensor and produces the
  /// result.
  @_inlineable @inline(__always)
  @differentiable(
    reverse, withRespectTo: (self, .0), adjoint: _adjointDot(_:partial:seed:)
  )
  func dot(_ other: Self) -> Self {
    return #tfop("MatMul", self, other)
  }

  /// Performs matrix multiplication between two tensors and produces the
  /// result.
  @_inlineable @inline(__always)
  static func ⊗ (lhs: Self, rhs: Self) -> Self {
    return lhs.dot(rhs)
  }
}

//===----------------------------------------------------------------------===//
// Element-wise binary comparison
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : Comparable {
  /// Computes `lhs < rhs` element-wise.
  /// - Note: `<` supports broadcasting.
  @_inlineable @inline(__always)
  static func < (lhs: Self, rhs: Self) -> BoolTensor {
    return #tfop("Less", lhs, rhs)
  }

  /// Computes `lhs < rhs`, broadcasting `rhs`.
  @_inlineable @inline(__always)
  static func < (lhs: Self, rhs: Scalar) -> BoolTensor {
    return #tfop("Less", lhs, _TFMakeScalarTensor(rhs))
  }

  /// Computes `lhs < rhs`, broadcasting `lhs`.
  @_inlineable @inline(__always)
  static func < (lhs: Scalar, rhs: Self) -> BoolTensor {
    return #tfop("Less", _TFMakeScalarTensor(lhs), rhs)
  }

  /// Computes `lhs <= rhs` element-wise.
  /// - Note: `<=` supports broadcasting.
  @_inlineable @inline(__always)
  static func <= (lhs: Self, rhs: Self) -> BoolTensor {
    return #tfop("LessEqual", lhs, rhs)
  }

  /// Computes `lhs <= rhs`, broadcasting `rhs`.
  @_inlineable @inline(__always)
  static func <= (lhs: Self, rhs: Scalar) -> BoolTensor {
    return #tfop("LessEqual", lhs, _TFMakeScalarTensor(rhs))
  }

  /// Computes `lhs <= rhs`, broadcasting `rhs`.
  @_inlineable @inline(__always)
  static func <= (lhs: Scalar, rhs: Self) -> BoolTensor {
    return #tfop("LessEqual", _TFMakeScalarTensor(lhs), rhs)
  }

  /// Computes `lhs > rhs` element-wise.
  /// - Note: `>` supports broadcasting.
  @_inlineable @inline(__always)
  static func > (lhs: Self, rhs: Self) -> BoolTensor {
    return #tfop("Greater", lhs, rhs)
  }

  /// Computes `lhs <= rhs`, broadcasting `rhs`.
  @_inlineable @inline(__always)
  static func > (lhs: Self, rhs: Scalar) -> BoolTensor {
    return #tfop("Greater", lhs, _TFMakeScalarTensor(rhs))
  }

  /// Computes `lhs <= rhs`, broadcasting `lhs`.
  @_inlineable @inline(__always)
  static func > (lhs: Scalar, rhs: Self) -> BoolTensor {
    return #tfop("Greater", _TFMakeScalarTensor(lhs), rhs)
  }

  /// Computes `lhs >= rhs` element-wise.
  /// - Note: `>=` supports broadcasting.
  @_inlineable @inline(__always)
  static func >= (lhs: Self, rhs: Self) -> BoolTensor {
    return #tfop("GreaterEqual", lhs, rhs)
  }

  /// Computes `lhs >= rhs`, broadcasting `rhs`.
  @_inlineable @inline(__always)
  static func >= (lhs: Self, rhs: Scalar) -> BoolTensor {
    return #tfop("GreaterEqual", lhs, _TFMakeScalarTensor(rhs))
  }

  /// Computes `lhs >= rhs`, broadcasting `lhs`.
  @_inlineable @inline(__always)
  static func >= (lhs: Scalar, rhs: Self) -> BoolTensor {
    return #tfop("GreaterEqual", _TFMakeScalarTensor(lhs), rhs)
  }
}

public extension TensorProtocol where Scalar : Equatable {
  /// Computes `self == other` element-wise.
  /// - Note: `elementsEqual` supports broadcasting.
  @_inlineable @inline(__always)
  func elementsEqual(_ other: Self) -> BoolTensor {
    return #tfop("Equal", self, other)
  }

  /// Computes `self == other` element-wise, broadcasting `other`.
  @_inlineable @inline(__always)
  func elementsEqual(_ other: Scalar) -> BoolTensor {
    return #tfop("Equal", self, _TFMakeScalarTensor(other))
  }

  /// Computes `self != other` element-wise.
  /// - Note: `elementsNotEqual` supports broadcasting.
  @_inlineable @inline(__always)
  func elementsNotEqual(_ other: Self) -> BoolTensor {
    return #tfop("NotEqual", self, other)
  }

  /// Computes `self != other` element-wise, broadcasting `other`.
  @_inlineable @inline(__always)
  func elementsNotEqual(_ other: Scalar) -> BoolTensor {
    return #tfop("NotEqual", self, _TFMakeScalarTensor(other))
  }
}

public extension TensorProtocol where Scalar == Bool {
  /// Performs a logical NOT operation element-wise.
  @_inlineable @inline(__always)
  static prefix func ! (x: Self) -> Self {
    return #tfop("LogicalNot", x)
  }

  /// Performs a logical AND operation element-wise.
  /// - Note: `&&` supports broadcasting.
  @_inlineable @inline(__always)
  static func && (lhs: Self, rhs: Self) -> Self {
    return #tfop("LogicalAnd", lhs, rhs)
  }

  /// Performs a logical AND operation element-wise, broadcasting `rhs`.
  @_inlineable @inline(__always)
  static func && (lhs: Self, rhs: Scalar) -> Self {
    return #tfop("LogicalAnd", lhs, _TFMakeScalarTensor(rhs))
  }

  /// Performs a logical AND operation element-wise, broadcasting `lhs`.
  @_inlineable @inline(__always)
  static func && (lhs: Scalar, rhs: Self) -> Self {
    return #tfop("LogicalAnd", _TFMakeScalarTensor(lhs), rhs)
  }

  /// Performs a logical OR operation element-wise.
  /// - Note: `||` supports broadcasting.
  @_inlineable @inline(__always)
  static func || (lhs: Self, rhs: Self) -> Self {
    return #tfop("LogicalOr", lhs, rhs)
  }

  /// Performs a logical OR operation element-wise, broadcasting `rhs`.
  @_inlineable @inline(__always)
  static func || (lhs: Self, rhs: Scalar) -> Self {
    return #tfop("LogicalOr", lhs, _TFMakeScalarTensor(rhs))
  }

  /// Performs a logical OR operation element-wise, broadcasting `lhs`.
  @_inlineable @inline(__always)
  static func || (lhs: Scalar, rhs: Self) -> Self {
    return #tfop("LogicalOr", _TFMakeScalarTensor(lhs), rhs)
  }
}

//===----------------------------------------------------------------------===//
// Element-wise binary ops with scalar on one side for Tensor
//
// NOTE: scalar-tensor binary ops are already defined on TensorProtocol.
// However, since Tensor conforms to ExpressibleByXXLiteral, it requires
// concrete implementations for the same scalar-tensor binary ops to prevent
// ambiguity errors. Note that TensorXD do not need concrete implementations
// because they do not conform to ExpressibleByXXLiteral.
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  @_inlineable @inline(__always)
  static func + (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) + rhs
  }

  @_inlineable @inline(__always)
  static func + (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs + Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func += (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs + rhs
  }

  @_inlineable @inline(__always)
  static func - (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) - rhs
  }

  @_inlineable @inline(__always)
  static func - (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs - Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func -= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs - Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func * (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) * rhs
  }

  @_inlineable @inline(__always)
  static func * (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs * Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func *= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs * rhs
  }

  @_inlineable @inline(__always)
  static func / (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) / rhs
  }

  @_inlineable @inline(__always)
  static func / (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs / Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func /= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs / rhs
  }

  @_inlineable @inline(__always)
  static func % (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs % Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func % (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) % rhs
  }

  @_inlineable @inline(__always)
  static func %= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs % rhs
  }
}

public extension Tensor where Scalar : Comparable {
  @_inlineable @inline(__always)
  static func > (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return lhs > Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func > (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) > rhs
  }

  @_inlineable @inline(__always)
  static func >= (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return lhs >= Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func >= (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) >= rhs
  }

  @_inlineable @inline(__always)
  static func < (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return lhs < Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func < (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) < rhs
  }

  @_inlineable @inline(__always)
  static func <= (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return lhs <= Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func <= (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) <= rhs
  }
}

public extension Tensor where Scalar : Equatable {
  @_inlineable @inline(__always)
  func elementsEqual(_ other: Scalar) -> Tensor<Bool> {
    return elementsEqual(Tensor(other))
  }

  @_inlineable @inline(__always)
  func elementsNotEqual(_ other: Scalar) -> Tensor<Bool> {
    return elementsNotEqual(Tensor(other))
  }
}

extension Tensor where Scalar : Equatable {
  @_inlineable @inline(__always)
  public static func == (lhs: Tensor, rhs: Scalar) -> Bool {
    return lhs == Tensor(rhs)
  }

  @_inlineable @inline(__always)
  public static func == (lhs: Scalar, rhs: Tensor) -> Bool {
    return Tensor(lhs) == rhs
  }

  @_inlineable @inline(__always)
  public static func != (lhs: Tensor, rhs: Scalar) -> Bool {
    return lhs != Tensor(rhs)
  }

  @_inlineable @inline(__always)
  public static func != (lhs: Scalar, rhs: Tensor) -> Bool {
    return Tensor(lhs) != rhs
  }
}

public extension Tensor where Scalar == Bool {
  @_inlineable @inline(__always)
  static func && (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs && Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func && (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) && rhs
  }

  @_inlineable @inline(__always)
  static func || (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs || Tensor(rhs)
  }

  @_inlineable @inline(__always)
  static func || (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) || rhs
  }
}

//===----------------------------------------------------------------------===//
// Transforms
//===----------------------------------------------------------------------===//

public extension TensorProtocol {
  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @_versioned @_inlineable @inline(__always)
  @differentiable(
    reverse, withRespectTo: (self), adjoint: _adjointTransposed(_:partial:seed:)
  )
  internal func transposed(
    withPermutations permutations: Tensor<Int32>
  ) -> Self {
    return #tfop("Transpose", handle, permutations, Tperm: Int32.self)
  }

  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @_inlineable @inline(__always)
  func transposed(withPermutations permutations: Int32...) -> Self {
    return transposed(withPermutations: Tensor(permutations))
  }

  /// Returns a transposed tensor, with dimensions permuted in reverse order.
  @_inlineable @inline(__always)
  func transposed() -> Self {
    let defaultPermutations = rankTensor - 1 - Tensor<Int32>(
      rangeFrom: 0, to: rank, stride: 1
    )
    return transposed(withPermutations: Tensor(defaultPermutations))
  }

  /// Concatenates tensors along the first dimension.
  /// - Precondition: The tensors must have the same shape, except for the
  ///   leading dimension.
  @_inlineable @inline(__always)
  func concatenated(with other: Self) -> Self {
    return #tfop("ConcatV2", [self, other], Tensor<Int32>(0), Tidx: Int32.self)
  }

  /// Concatenates tensors along the specified axis.
  /// - Precondition: The tensors must have the same dimensions, except for the
  ///   specified axis.
  /// - Precondition: The axis must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func concatenated(with other: Self, alongAxis axis: Int32) -> Self {
    return #tfop("ConcatV2", [self, other], Tensor<Int32>(axis),
                 Tidx: Int32.self)
  }

  /// Concatenation operator.
  /// - Note: `++` is a custom operator that does not exist in Swift, but does
  ///   in Haskell/Scala. Its addition is not an insignificant language change
  ///   and may be controversial. The existence/naming of `++` will be discussed
  ///   during a later API design phase.
  @_inlineable @inline(__always)
  static func ++ (lhs: Self, rhs: Self) -> Self {
    return lhs.concatenated(with: rhs)
  }
}

//===----------------------------------------------------------------------===//
// Element-wise math functions
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : SignedNumeric {
  /// Computes the negation of the specified tensor element-wise.
  @_inlineable @inline(__always)
  @differentiable(reverse, adjoint: _adjointNegate(_:partial:seed:))
  static prefix func - (rhs: Self) -> Self {
    return #tfop("Neg", rhs)
  }
}

/// Computes the absolute value of the specified tensor element-wise.
@_inlineable @inline(__always)
public func abs<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : SignedNumeric {
  return #tfop("Abs", x)
}

/// Computes the natural logarithm of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointLog(_:partial:seed:))
public func log<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Log", x)
}

/// Computes `sin` of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointSin(_:partial:seed:))
public func sin<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Sin", x)
}

/// Computes `cos` of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointCos(_:partial:seed:))
public func cos<T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar : FloatingPoint {
  return #tfop("Cos", x)
}

/// Computes `cos` of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointTan(_:partial:seed:))
public func tan<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Tan", x)
}

/// Computes `cos` of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointSinh(_:partial:seed:))
public func sinh<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Sinh", x)
}

/// Computes `cos` of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointCosh(_:partial:seed:))
public func cosh<T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar : FloatingPoint {
  return #tfop("Cosh", x)
}

/// Computes `cos` of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointTanh(_:partial:seed:))
public func tanh<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Tanh", x)
}

/// Computes the square root of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointSqrt(_:partial:seed:))
public func sqrt<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Sqrt", x)
}

/// Computes the inverse square root of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointRsqrt(_:partial:seed:))
public func rsqrt<T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar : FloatingPoint {
  return #tfop("Rsqrt", x)
}

/// Computes `exp` of the specified tensor element-wise.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointExp(_:partial:seed:))
public func exp<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Exp", x)
}

/// Computes the power of the first tensor to the second tensor.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointPow(_:_:partial:seed:))
public func pow<T : TensorProtocol>(_ lhs: T, _ rhs: T) -> T
  where T.Scalar : FloatingPoint {
  return #tfop("Pow", lhs, rhs)
}

/// Computes the power of the scalar to the tensor, broadcasting the scalar.
@_inlineable @inline(__always)
public func pow<T : TensorProtocol>(_ lhs: T.Scalar, _ rhs: T) -> T
  where T.Scalar : FloatingPoint {
  return pow(T(handle: _TFMakeScalarTensor(lhs)), rhs)
}

/// Computes the power of the tensor to the scalar, broadcasting the scalar.
@_inlineable @inline(__always)
public func pow<T : TensorProtocol>(_ lhs: T, _ rhs: T.Scalar) -> T
  where T.Scalar : FloatingPoint {
  return pow(lhs, T(handle: _TFMakeScalarTensor(rhs)))
}

/// Computes the element-wise maximum of two tensors.
/// - Note: `max` supports broadcasting.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointMax(_:_:partial:seed:))
public func max<T : TensorProtocol>(_ lhs: T, _ rhs: T) -> T
  where T.Scalar : Numeric & Comparable {
  return #tfop("Maximum", lhs, rhs)
}

/// Computes the element-wise maximum of the scalar and the tensor, broadcasting
/// the scalar.
@_inlineable @inline(__always)
public func max<T : TensorProtocol>(_ lhs: T.Scalar, _ rhs: T) -> T
  where T.Scalar : Numeric & Comparable {
  return max(T(handle: _TFMakeScalarTensor(lhs)), rhs)
}

/// Computes the element-wise maximum of the scalar and the tensor, broadcasting
/// the scalar.
@_inlineable @inline(__always)
public func max<T : TensorProtocol>(_ lhs: T, _ rhs: T.Scalar) -> T
  where T.Scalar : Numeric & Comparable {
  return max(lhs, T(handle: _TFMakeScalarTensor(rhs)))
}

/// Computes the element-wise minimum of two tensors.
/// - Note: `min` supports broadcasting.
@_inlineable @inline(__always)
@differentiable(reverse, adjoint: _adjointMin(_:_:partial:seed:))
public func min<T : TensorProtocol>(_ lhs: T, _ rhs: T) -> T
  where T.Scalar : Numeric & Comparable {
  return #tfop("Minimum", lhs, rhs)
}

/// Computes the element-wise minimum of the scalar and the tensor, broadcasting
/// the scalar.
@_inlineable @inline(__always)
public func min<T : TensorProtocol>(_ lhs: T.Scalar, _ rhs: T) -> T
  where T.Scalar : Numeric & Comparable {
  return min(T(handle: _TFMakeScalarTensor(lhs)), rhs)
}

/// Computes the element-wise minimum of the scalar and the tensor, broadcasting
/// the scalar.
@_inlineable @inline(__always)
public func min<T : TensorProtocol>(_ lhs: T, _ rhs: T.Scalar) -> T
  where T.Scalar : Numeric & Comparable {
  return min(lhs, T(handle: _TFMakeScalarTensor(rhs)))
}

/// Computes the square of the tensor.
public extension TensorProtocol where Scalar : Numeric {
  @_inlineable @inline(__always)
  func squared() -> Self {
    return #tfop("Square", handle)
  }
}

//===----------------------------------------------------------------------===//
// Non-elementwise math functions
//===----------------------------------------------------------------------===//

/// Computes the log-softmax of the specified tensor element-wise.
@_inlineable @inline(__always)
public func logSoftmax<T : TensorProtocol>(_ x: T) -> T
  where T.Scalar : BinaryFloatingPoint {
  return #tfop("LogSoftmax", x)
}

//===----------------------------------------------------------------------===//
// Selection
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar == Bool {
  /// Returns a new tensor containing elements from either `left` or `right`,
  /// depending on the elements of `self`.
  ///
  /// `self` acts as a amsk that chooses, based on the value at each scalar,
  ///  whether the corresponding scalar in the output should be taken from
  /// `left` (if `true`) or `right` (if `false`).
  ///
  /// - Precondition: `left` and `right` must have the same shape. If
  ///   `left` and `right` are scalar, then `self` must also be scalar. If
  ///   `left` and `right` have rank greater than or equal to 1, then `self`
  ///   must be either have the same shape as `left` or be a 1-D `Tensor` such
  ///   that `self.scalarCount == left[0]`.
  @_inlineable @inline(__always)
  public func selecting<T : TensorProtocol>(_ left: T, _ right: T) -> T {
    return #tfop("Select", handle, left, right)
  }

  // FIXME: "Select" is non-broadcasting: `left` and `right` are required to
  // have the same shapes. An explicit broadcast must be added.
  @_inlineable @inline(__always)
  public func selecting<T : TensorProtocol>(_ left: T.Scalar, _ right: T) -> T {
    return #tfop("Select", handle, _TFMakeScalarTensor(left), right)
  }

  // FIXME: "Select" is non-broadcasting: `left` and `right` are required to
  // have the same shapes. An explicit broadcast must be added.
  @_inlineable @inline(__always)
  public func selecting<T : TensorProtocol>(_ left: T, _ right: T.Scalar) -> T {
    return #tfop("Select", handle, left, _TFMakeScalarTensor(right))
  }
}

//===----------------------------------------------------------------------===//
// Reduction
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : Numeric {
  /// Returns the arithmetic mean of all scalars.
  @_inlineable @inline(__always)
  func mean() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Mean", self, axes))
  }

  /// Returns the sum of all scalars.
  @_inlineable @inline(__always)
  func sum() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Sum", self, axes))
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func mean(alongAxes axes: Int32...) -> Self {
    return #tfop("Mean", handle, Tensor<Int32>(axes), keep_dims: true,
                 Tidx: Int32.self)
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func sum(alongAxes axes: Int32...) -> Self {
    return #tfop("Sum", handle, Tensor<Int32>(axes), keep_dims: true,
                 Tidx: Int32.self)
  }
}

public extension TensorProtocol where Scalar : Numeric & Comparable {
  /// Returns the minimum of all scalars.
  @_inlineable @inline(__always)
  func min() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Min", self, axes))
  }

  /// Returns the maximum of all scalars.
  @_inlineable @inline(__always)
  func max() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Max", self, axes))
  }

  /// Returns the minimum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func min(alongAxes axes: Int32...) -> Self {
    return #tfop("Min", handle, Tensor<Int32>(axes), keep_dims: true,
                 Tidx: Int32.self)
  }

  /// Returns the minimum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func max(alongAxes axes: Int32...) -> Self {
    return #tfop("Max", handle, Tensor<Int32>(axes), keep_dims: true,
                 Tidx: Int32.self)
  }

  /// Returns the index of the maximum value of the flattened scalars.
  @_inlineable @inline(__always)
  func argmax() -> Int32 {
    let flattened: Self = #tfop("Reshape", handle, Tensor<Int32>([-1]))
    return _TFGetScalarOrDie(#tfop("ArgMax", flattened, Tensor<Int32>(0),
                                   output_type: Int32.self))
  }

  /// Returns the index of the minimum value of the flattened scalars.
  @_inlineable @inline(__always)
  func argmin() -> Int32 {
    let flattened: Self = #tfop("Reshape", handle, Tensor<Int32>([-1]))
    return _TFGetScalarOrDie(#tfop("ArgMin", flattened, Tensor<Int32>(0),
                                   output_type: Int32.self))
  }
}

public extension TensorProtocol where Scalar == Bool {
  /// Returns `true` if all scalars are equal to `true`. Otherwise, returns
  /// `false`.
  @_inlineable @inline(__always)
  func all() -> Bool {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("All", self, axes))
  }

  /// Returns `true` if any scalars are equal to `true`. Otherwise, returns
  /// `false`.
  @_inlineable @inline(__always)
  func any() -> Bool {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Any", self, axes))
  }

  /// Performs a logical AND operation along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func all(alongAxes axes: Int32...) -> Self {
    return #tfop("All", handle, Tensor<Int32>(axes), keep_dims: true,
                 Tidx: Int32.self)
  }

  /// Performs a logical OR operation along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func any(alongAxes axes: Int32...) -> Self {
    return #tfop("Any", handle, Tensor<Int32>(axes), keep_dims: true,
                 Tidx: Int32.self)
  }
}

public extension Tensor where Scalar == Bool {
  /// Returns `true` if all scalars are equal to `true`. Otherwise, returns
  /// `false`.
  // NOTE: This overload is necessary, otherwise `all()` would refer
  // to the variadic method `all(squeezingAxes:)` with zero indices.
  @_inlineable @inline(__always)
  func all() -> Bool {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("All", self, axes))
  }

  /// Returns `true` if any scalars are equal to `true`. Otherwise, returns
  /// `false`.
  // NOTE: This overload is necessary, otherwise `any()` would refer
  // to the variadic method `any(squeezingAxes:)` with zero indices.
  @_inlineable @inline(__always)
  func any() -> Bool {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Any", self, axes))
  }

  /// Performs a logical AND operation along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func all(squeezingAxes axes: Int32...) -> Tensor {
    return #tfop("All", handle, Tensor<Int32>(axes), keep_dims: false,
                 Tidx: Int32.self)
  }

  /// Performs a logical AND operation along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func any(squeezingAxes axes: Int32...) -> Tensor {
    return #tfop("Any", handle, Tensor<Int32>(axes), keep_dims: false,
                 Tidx: Int32.self)
  }
}

public extension Tensor where Scalar : Numeric & Comparable {
  // NOTE: This overload is necessary, otherwise `min()` would refer
  // to the variadic method `min(squeezingAxes:)` with zero indices.
  @_inlineable @inline(__always)
  func min() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Min", self, axes))
  }

  // NOTE: This overload is necessary, otherwise `max()` would refer
  // to the variadic method `max(squeezingAxes:)` with zero indices.
  @_inlineable @inline(__always)
  func max() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Max", self, axes))
  }

  /// Returns the maximum values along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func max(squeezingAxes axes: Int32...) -> Tensor {
    return #tfop("Max", handle, Tensor<Int32>(axes), keep_dims: false,
                 Tidx: Int32.self)
  }

  /// Returns the minimum values along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func min(squeezingAxes axes: Int32...) -> Tensor {
    return #tfop("Min", handle, Tensor<Int32>(axes), keep_dims: false,
                 Tidx: Int32.self)
  }

  /// Returns the indices of the maximum values along the specified axes. The
  /// reduced dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func argmax(squeezingAxis axis: Int32) -> Tensor<Int32> {
    return #tfop("ArgMax", handle, Tensor<Int32>(axis), output_type: Int32.self)
  }

  /// Returns the indices of the minimum values along the specified axes. The
  /// reduced dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @_inlineable @inline(__always)
  func argmin(squeezingAxis axis: Int32) -> Tensor<Int32> {
    return #tfop("ArgMin", handle, Tensor<Int32>(axis), output_type: Int32.self)
  }
}

public extension Tensor where Scalar : Numeric {
  // NOTE: This overload is necessary, otherwise `mean()` would refer
  // to the variadic method `mean(squeezingAxes:)` with zero indices.
  @_inlineable @inline(__always)
  func mean() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Mean", self, axes))
  }

  // NOTE: This overload is necessary, otherwise `sum()` would refer
  // to the variadic method `sum(squeezingAxes:)` with zero indices.
  @_inlineable @inline(__always)
  func sum() -> Scalar {
    let axes = Tensor<Int32>(rangeFrom: 0, to: rank, stride: 1)
    return _TFGetScalarOrDie(#tfop("Sum", self, axes))
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @_inlineable @inline(__always)
  func mean(squeezingAxes axes: Int32...) -> Tensor {
    return #tfop("Mean", handle, Tensor<Int32>(axes), keep_dims: false,
                 Tidx: Int32.self)
  }

  /// Returns the sum along the specified axes. The reduced dimensions are
  /// removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @_inlineable @inline(__always)
  func sum(squeezingAxes axes: Int32...) -> Tensor {
    return #tfop("Sum", handle, Tensor<Int32>(axes), keep_dims: false,
                 Tidx: Int32.self)
  }
}

//===----------------------------------------------------------------------===//
// Tensor properties
//===----------------------------------------------------------------------===//

public extension TensorProtocol {
  /// The rank of the tensor, represented as a `Tensor<Int32>`.
  @_inlineable
  var rankTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return #tfop("Rank", handle)
    }
  }

  /// The dimensions of the tensor, represented as a `Tensor<Int32>`.
  @_inlineable
  var shapeTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return #tfop("Shape", handle)
    }
  }

  /// The number of scalars in the tensor, represented as a `Tensor<Int32>`.
  @_inlineable
  var scalarCountTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return #tfop("Size", handle)
    }
  }
}

//===----------------------------------------------------------------------===//
// Unbroadcasting
//===----------------------------------------------------------------------===//

public extension TensorProtocol {
  @_inlineable @inline(__always)
  func unbroadcast(toShape otherShape: Tensor<Int32>) -> Self {
    let rankDiff = (rankTensor - otherShape.scalarCountTensor).rankLifted()
    let ones: Tensor<Int32> = #tfop("Fill", rankDiff, Tensor<Int32>(1))
    let paddedShape = ones ++ otherShape
    let nonEqualIndices = paddedShape.elementsNotEqual(shapeTensor)
    let broadcastIndices = Tensor<Int64>(
      handle: #tfop("Where", nonEqualIndices, T: Bool.self)
    ).flattened()
    let unbroadcasted: Self = #tfop(
      "Sum", handle, Tensor<Int32>(broadcastIndices), keep_dims: false,
      Tidx: Int32.self
    )
    return #tfop("Reshape", unbroadcasted, otherShape)
  }

  @_inlineable @inline(__always)
  func unbroadcast(to other: Self) -> Self {
    return unbroadcast(toShape: other.shapeTensor)
  }

  @_inlineable @inline(__always)
  func unbroadcast(to shape: TensorShape) -> Self {
    return unbroadcast(toShape: Tensor(shape.dimensions))
  }
}

//===----------------------------------------------------------------------===//
// Padding
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : Numeric {
  /// Returns a padded tensor according to the specified padding sizes.
  @_inlineable @inline(__always)
  func padded(
    forSizes sizes: @autoclosure () -> [(before: Int32, after: Int32)],
    with value: Scalar = 0
  ) -> Self {
    let paddings: TensorHandle<Int32> = _TFHoistable {
      let sizes = sizes()
      return Tensor(
        shape: [Int32(sizes.count), 2],
        scalars: sizes.flatMap { [$0.before, $0.after] }
      ).handle
    }
    return #tfop("PadV2", self, _TFSend(paddings), Tensor(value),
                 T: Scalar.self, Tpaddings: Int32.self)
  }
}

//===----------------------------------------------------------------------===//
// Indexing and slicing
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Access the element tensor specified by an index in the leading dimension.
  /// - Parameter index: Index of the element tensor.
  @_inlineable
  subscript(index: Int32) -> Tensor {
    @inline(__always)
    get {
      // NOTE: Thought Gather exactly performs element indexing, it is an
      // allocating operation. Slice is used here instead even though the
      // implementation is more convoluted because it is non-allocating.
      // Actual performance/memory tests should be done for some empirical
      // comparison.
      // Gather implementation is below:
      // return #tfop("GatherV2", self, Tensor<Int32>(index), Tensor<Int32>(0),
      //              Tindices: Int32.self)
      let indexTensor = Tensor<Int32>([index])
      let remainingZeros: Tensor<Int32> = #tfop(
        "Fill", (rankTensor - 1).rankLifted(), Tensor<Int32>(0)
      )
      let startIndices = indexTensor.concatenated(with: remainingZeros)

      let firstDimension: Tensor<Float> = #tfop(
        "GatherV2", Tensor<Float>(shapeTensor), Tensor<Int32>(0),
        Tensor<Int32>(0), Tindices: Int32.self
      )
      let boundSize = Tensor<Float>([1]) - firstDimension
      let offset: Tensor<Int32> = Tensor<Int32>(
        Tensor<Float>(
          handle: #tfop("ScatterNd", Tensor<Int32>([[0]]),
                        boundSize, rankTensor.rankLifted())
        )
      )
      let boundSizes: Tensor<Int32> = shapeTensor + offset
      let slice: Tensor = #tfop("Slice", self, startIndices, boundSizes,
                        Index: Int32.self)
      return #tfop("Squeeze", slice, squeeze_dims: [0])
    }
  }

  /// Access the subdimensional tensor at the specified list of indices.
  /// - Parameter indices: List of indices.
  /// - Note: this function is more efficient than using `subscript(index:)`
  ///   multiple times because this produces a single GatherNd op (compared with
  ///   multiple Gather ops).
  @_inlineable
  subscript(indices: Int32...) -> Tensor {
    @inline(__always)
    get {
      // NOTE: Rewriting this using Slice is difficult: the main challenge is in
      // constructing the "sizes" argument, which essentially is
      // `indices.shapeTensor` but with `indices.count` number of leading 1s.
      // Since GatherNd is the exact op that performs subdimensional indexing,
      // it is used here in spite of the fact it may perform allocation(?).
      // TODO: Consider more clearly distinguishing `subscript(index:)` and
      // `subscript(indices:)`, since their implementations are quite different.
      return #tfop("GatherNd", self, Tensor<Int32>(indices),
                   Tindices: Int32.self)
    }
  }

  /// Access the subtensor specified by a contiguous range of indices.
  /// - Parameter bounds: Contiguous range of indices.
  @_inlineable
  subscript(bounds: Range<Int32>) -> Tensor {
    @inline(__always)
    get {
      // NOTE: Though `tf.slice` and `tf.strided_slice` are not easy to use
      // because they require slice bounds for every dimension, they should be
      // used because the are non-allocating. Other slice implementations (like
      // combining Gather and Range) perform allocation and should not be used
      // even though they are easier to write.

      // Let (lo, hi) represent lower and upper bounds respectively.
      // startIndices = [lo, 0, 0, ..., 0]
      // boundSizes = [hi - lo, d1, d2, ..., dn] where di = shape[i]
      // TODO: The horrendous mess of type-casting is necessary due to GPU ops
      // (Gather, ScatterNd) not accepting Int32 for particular inputs. Refactor
      // if possible.
      let lowerBound = Tensor<Int32>([bounds.lowerBound])
      let remainingZeros: Tensor<Int32> = #tfop(
        "Fill", (rankTensor - 1).rankLifted(), Tensor<Int32>(0)
      )
      let startIndices = lowerBound.concatenated(with: remainingZeros)

      let boundSize = Tensor<Int32>([bounds.upperBound])
        - lowerBound - Tensor<Int32>(Tensor<Float>(shapeTensor)[0])
      let offset: Tensor<Int32> = Tensor<Int32>(
        Tensor<Float>(
          handle: #tfop("ScatterNd", Tensor<Int32>([[0]]),
                        Tensor<Float>(boundSize), rankTensor.rankLifted())
        )
      )
      let boundSizes: Tensor<Int32> = shapeTensor + offset
      return #tfop("Slice", self, startIndices, boundSizes, Index: Int32.self)
    }
  }

  // TODO(danielzheng): Add strided slices? (increment by something different
  // than 1)
  // Ideas for strided slice API: it could be another subscript method, or it
  // be a top level `stride` function like Swift's `stride(from:to:by:)`.

  /// Extracts a slice from the tensor defined by lower and upper bounds for
  /// each dimension.
  ///
  /// - Parameter lowerBounds: The lower bounds at each dimension.
  /// - Parameter upperBounds: The upper bounds at each dimension.
  @_inlineable @inline(__always)
  func slice(lowerBounds: [Int32], upperBounds: [Int32]) -> Tensor {
    /// TODO: Precondition `lowerBounds.count == upperBounds.count`,
    /// preferably in graph.
    let lowerBoundsTensor = Tensor<Int32>(lowerBounds)
    return #tfop("Slice", self, lowerBoundsTensor,
                 Tensor<Int32>(upperBounds) - lowerBoundsTensor)
  }
}

//===----------------------------------------------------------------------===//
// Normalization
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : BinaryFloatingPoint {
  /// Computes the batch normalized tensor along the specified axis.
  ///
  /// Specifically, returns `(self - mu)/(var + epsilon) * gamma + beta` where
  /// `mu` and `var` are respectively the mean and variance of `self` along
  /// `axis`.
  ///
  /// - Parameters:
  ///   - axis: The batch dimension.
  ///   - offset: The scalar offset, also known as beta.
  ///   - scale: The scalar scale, also known as gamma.
  ///   - epsilon: A small value added to the denominator for numerical
  ///     stability.
  // NOTE: It is not possible to provide a floating point initial value for
  // arguments (like `epsilon`) because `FloatingPoint` is not
  // `ExpressibleByFloatLiteral`.
  @_inlineable @inline(__always)
  @differentiable(
    reverse, withRespectTo: (self, .1, .2),
    adjoint: _adjointBatchNormalized(alongAxis:offset:scale:epsilon:partial:seed:)
  )
  func batchNormalized(
    alongAxis axis: Int32,
    offset: Tensor = Tensor(0),
    scale: Tensor = Tensor(1),
    epsilon: Tensor = Tensor(0.001)
  ) -> Tensor {
    let mean = self.mean(alongAxes: axis)
    let squaredDiff: Tensor = #tfop("SquaredDifference", self, mean)
    let variance = squaredDiff.mean(alongAxes: axis)
    let inv = rsqrt(variance + epsilon) * scale
    return self * inv + (offset - mean * inv)
  }
}

//===----------------------------------------------------------------------===//
// Convolution and pooling
//===----------------------------------------------------------------------===//

/// A padding scheme. Used by padding, convolution, and pooling ops.
public enum Padding {
  /// The "valid" padding scheme.
  case valid
  /// The "same" padding scheme.
  case same
}

internal extension Padding {
  @_versioned @_inlineable
  var cName: String {
    @inline(__always)
    get {
      switch self {
      case .same: return "SAME"
      case .valid: return "VALID"
      }
    }
  }
}

public extension Tensor where Scalar : FloatingPoint {
  /// Computes a 2-D convolution using `self` as input, with the specified
  /// filter, strides, and padding.
  ///
  /// - Parameters:
  ///   - filter: The convolution filter.
  ///   - strides: The strides of the sliding filter for each dimension of the
  ///     input.
  ///   - padding: The padding for the operation.
  /// - Precondition: `self` must have rank 4.
  /// - Precondition: `filter` must have rank 4.
  @_inlineable @inline(__always)
  @differentiable(
    reverse, withRespectTo: (self, .0),
    adjoint: _adjointConvolved2D(filter:strides:padding:partial:seed:)
  )
  func convolved2D(
    withFilter filter: Tensor,
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor {
    return #tfop("Conv2D", handle, filter,
                 strides: [strides.0, strides.1, strides.2, strides.3],
                 padding: padding.cName)
  }

  /// Computes a 2-D max pooling, with the specified kernel sizes, strides, and
  /// padding.
  ///
  /// - Parameters:
  ///   - kernelSize: The dimensions of the pooling kernel.
  ///   - strides: The strides of the sliding filter for each dimension of the
  ///     input.
  ///   - padding: The padding for the operation.
  @_inlineable @inline(__always)
  @differentiable(
    reverse, withRespectTo: (self),
    adjoint: _adjointMaxPooled(kernelSize:strides:padding:partial:seed:)
  )
  func maxPooled(
    kernelSize: (Int32, Int32, Int32, Int32),
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor {
    return #tfop("MaxPoolV2", handle, Tensor<Int32>(kernelSize),
                 Tensor<Int32>(strides), padding: padding.cName)
  }

  /// Computes a 2-D average pooling, with the specified kernel sizes, strides,
  /// and padding.
  ///
  /// - Parameters:
  ///   - kernelSize: The dimensions of the pooling kernel.
  ///   - strides: The strides of the sliding filter for each dimension of the
  ///     input.
  ///   - padding: The padding for the operation.
  @_inlineable @inline(__always)
  @differentiable(
    reverse, withRespectTo: (self),
    adjoint: _adjointAveragePooled(kernelSize:strides:padding:partial:seed:)
  )
  func averagePooled(
    kernelSize: (Int32, Int32, Int32, Int32),
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor {
    return #tfop(
      "AvgPool", handle,
      ksize: [kernelSize.0, kernelSize.1, kernelSize.2, kernelSize.3],
      strides: [strides.0, strides.1, strides.2, strides.3],
      padding: padding.cName
    )
  }
}

public extension Tensor4D where Scalar : FloatingPoint {
  /// Computes a 2-D convolution using `self` as input, with the specified
  /// filter, strides, and padding.
  ///
  /// - Parameters:
  ///   - filter: The convolution filter.
  ///   - strides: The strides of the sliding filter for each dimension of the
  ///     input.
  ///   - padding: The padding for the operation.
  @_inlineable @inline(__always)
  func convolved2D(
    withFilter filter: Tensor4D,
    strides: (Int32, Int32, Int32, Int32),
    padding: Padding
  ) -> Tensor4D {
    return Tensor4D(
      base: base.convolved2D(
        withFilter: filter.base,
        strides: strides,
        padding: padding
      )
    )
  }

  // NOTE: Conv3D requires the existence of Tensor5D, since the input/filter
  // tensors for a Conv3D operation must have rank 5.
}
