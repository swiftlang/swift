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
// This file contains definitions of most tensor operations.
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

infix operator ++ : AdditionPrecedence

infix operator .< : ComparisonPrecedence
infix operator .<= : ComparisonPrecedence
infix operator .>= : ComparisonPrecedence
infix operator .> : ComparisonPrecedence
infix operator .== : ComparisonPrecedence
infix operator .!= : ComparisonPrecedence
infix operator .=

// TODO:
// - Consider explicit broadcasting for elementwise binary ops when
//   scalarization and rank getter are implemented.

//===----------------------------------------------------------------------===//
// Scalar type cast
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  /// Perform an element-wise type conversion from a `Bool` tensor.
  @inlinable @inline(__always)
  init(_ other: Tensor<Bool>) {
    self = Raw.cast(other)
  }
}

//===----------------------------------------------------------------------===//
// Additive group
//===----------------------------------------------------------------------===//

extension Tensor : AdditiveArithmetic where Scalar : Numeric {
  /// A scalar zero tensor.
  @inlinable
  public static var zero: Tensor {
    @inline(__always)
    get {
      return Tensor(zeros: [])
    }
  }

  /// Adds two tensors and produces their sum.
  /// - Note: `+` supports broadcasting.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpAdd(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  public static func + (lhs: Tensor, rhs: Tensor) -> Tensor {
    return Raw.add(lhs, rhs)
  }

  /// Subtracts one tensor from another and produces their difference.
  /// - Note: `-` supports broadcasting.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpSubtract(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  public static func - (lhs: Tensor, rhs: Tensor) -> Tensor {
    return Raw.sub(lhs, rhs)
  }
}

//===----------------------------------------------------------------------===//
// Vector space
//===----------------------------------------------------------------------===//

extension Tensor : VectorNumeric where Scalar : Numeric {
  /// Multiplies the scalar with every scalar of the tensor and produces the
  /// product.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpMultiply(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  public static func * (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) * rhs
  }
}

extension Tensor : ShapedVectorNumeric where Scalar : Numeric {}

extension Tensor : Differentiable where Scalar : TensorFlowFloatingPoint {
  public typealias TangentVector = Tensor
  public typealias CotangentVector = Tensor
  public typealias AllDifferentiableVariables = Tensor
  @inlinable @inline(__always)
  public func tangentVector(from cotangent: CotangentVector) -> TangentVector {
    return cotangent
  }
}

//===----------------------------------------------------------------------===//
// Additional element-wise operators
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  /// Adds the scalar to every scalar of the tensor and produces the sum.
  @inlinable @inline(__always)
  @differentiable(vjp: _vjpAdd(lhs:rhs:) where Scalar : TensorFlowFloatingPoint)
  static func + (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) + rhs
  }

  /// Adds the scalar to every scalar of the tensor and produces the sum.
  @inlinable @inline(__always)
  @differentiable(vjp: _vjpAdd(lhs:rhs:) where Scalar : TensorFlowFloatingPoint)
  static func + (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs + Tensor(rhs)
  }

  /// Subtracts the scalar from every scalar of the tensor and produces the
  /// difference.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpSubtract(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func - (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) - rhs
  }

  /// Subtracts the scalar from every scalar of the tensor and produces the
  /// difference.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpSubtract(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func - (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs - Tensor(rhs)
  }

  /// Adds two tensors and stores the result in the left-hand-side variable.
  /// - Note: `+=` supports broadcasting.
  @inlinable @inline(__always)
  static func += (lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs + rhs
  }

  /// Adds the scalar to every scalar of the tensor and stores the result in the
  /// left-hand-side variable.
  @inlinable @inline(__always)
  static func += (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs + rhs
  }

  /// Subtracts the second tensor from the first and stores the result in the
  /// left-hand-side variable.
  /// - Note: `-=` supports broadcasting.
  @inlinable @inline(__always)
  static func -= (lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs - rhs
  }

  /// Subtracts the scalar from every scalar of the tensor and stores the result
  /// in the left-hand-side variable.
  @inlinable @inline(__always)
  static func -= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs - rhs
  }

  /// Multiplies two tensors and produces their product.
  /// - Note: `*` supports broadcasting.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpMultiply(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func * (lhs: Tensor, rhs: Tensor) -> Tensor {
    return Raw.mul(lhs, rhs)
  }

  /// Multiplies the scalar with every scalar of the tensor and produces the
  /// product.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpMultiply(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func * (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs * Tensor(rhs)
  }

  /// Multiplies two tensors and stores the result in the left-hand-side
  /// variable.
  /// - Note: `*=` supports broadcasting.
  @inlinable @inline(__always)
  static func *= (lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs * rhs
  }

  @inlinable @inline(__always)
  static func *= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs * rhs
  }

  /// Returns the quotient of dividing the first tensor by the second.
  /// - Note: `/` supports broadcasting.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpDivide(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func / (lhs: Tensor, rhs: Tensor) -> Tensor {
    return Raw.div(lhs, rhs)
  }

  /// Returns the quotient of dividing the scalar by the tensor, broadcasting
  /// the scalar.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpDivide(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func / (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) / rhs
  }

  /// Returns the quotient of dividing the tensor by the scalar, broadcasting
  /// the scalar.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpDivide(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func / (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs / Tensor(rhs)
  }

  /// Divides the first tensor by the second and stores the quotient in the
  /// left-hand-side variable.
  @inlinable @inline(__always)
  static func /= (lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs / rhs
  }

  /// Divides the tensor by the scalar, broadcasting the scalar, and stores the
  /// quotient in the left-hand-side variable.
  @inlinable @inline(__always)
  static func /= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs / rhs
  }

  /// Returns the remainder of dividing the first tensor by the second.
  /// - Note: `%` supports broadcasting.
  @inlinable @inline(__always)
  static func % (lhs: Tensor, rhs: Tensor) -> Tensor {
    return Raw.mod(lhs, rhs)
  }

  /// Returns the remainder of dividing the tensor by the scalar, broadcasting
  /// the scalar.
  @inlinable @inline(__always)
  static func % (lhs: Tensor, rhs: Scalar) -> Tensor {
    return lhs % Tensor(rhs)
  }

  /// Returns the remainder of dividing the scalar by the tensor, broadcasting
  /// the scalar.
  @inlinable @inline(__always)
  static func % (lhs: Scalar, rhs: Tensor) -> Tensor {
    return Tensor(lhs) % rhs
  }

  /// Divides the first tensor by the second and stores the remainder in the
  /// left-hand-side variable.
  @inlinable @inline(__always)
  static func %= (lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs % rhs
  }

  /// Divides the tensor by the scalar and stores the remainder in the
  /// left-hand-side variable.
  @inlinable @inline(__always)
  static func %= (lhs: inout Tensor, rhs: Scalar) {
    lhs = lhs % rhs
  }
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

/// Performs matrix multiplication with another tensor and produces the
/// result.
@inlinable @inline(__always)
@differentiable(
  vjp: _vjpMatmul(_:_:)
  where Scalar : TensorFlowFloatingPoint
)
public func matmul<Scalar : Numeric>(
  _ lhs: Tensor<Scalar>, _ rhs: Tensor<Scalar>
) -> Tensor<Scalar> {
  // Default arguments specified explicitly to avoid "external declarations of
  // SILFunctions with shared visibility is not allowed" SILVerifier error in
  // "tests/AutoDiff/tensor_autodiff_runtime.swift".
  return Raw.matMul(lhs, rhs, transposeA: false, transposeB: false)
}

infix operator • : MultiplicationPrecedence

public extension Tensor where Scalar : Numeric {
  // TODO: We have to define a custom VJP on • because AD can't yet
  // differentiate generic methods. After AD can differentiate generic methods,
  // remove the custom VJP.

  /// Performs matrix multiplication between two tensors and produces the
  /// result.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpMatmulOperator(lhs:rhs:)
    where Scalar : TensorFlowFloatingPoint
  )
  static func • (lhs: Tensor, rhs: Tensor) -> Tensor {
    return matmul(lhs, rhs)
  }
}

//===----------------------------------------------------------------------===//
// Element-wise binary comparison
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric & Comparable {
  /// Computes `lhs < rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  @inlinable @inline(__always)
  static func .< (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Raw.less(lhs, rhs)
  }

  /// Computes `lhs <= rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  @inlinable @inline(__always)
  static func .<= (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Raw.lessEqual(lhs, rhs)
  }

  /// Computes `lhs > rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  @inlinable @inline(__always)
  static func .> (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Raw.greater(lhs, rhs)
  }

  /// Computes `lhs >= rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  @inlinable @inline(__always)
  static func .>= (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Raw.greaterEqual(lhs, rhs)
  }

  /// Computes `lhs < rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.<` supports broadcasting.
  @inlinable @inline(__always)
  static func .< (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Raw.less(Tensor(lhs), rhs)
  }

  /// Computes `lhs <= rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.<=` supports broadcasting.
  @inlinable @inline(__always)
  static func .<= (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Raw.lessEqual(Tensor(lhs), rhs)
  }

  /// Computes `lhs > rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.>` supports broadcasting.
  @inlinable @inline(__always)
  static func .> (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Raw.greater(Tensor(lhs), rhs)
  }

  /// Computes `lhs >= rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.>=` supports broadcasting.
  @inlinable @inline(__always)
  static func .>= (lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Raw.greaterEqual(Tensor(lhs), rhs)
  }

  /// Computes `lhs < rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.<` supports broadcasting.
  @inlinable @inline(__always)
  static func .< (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return Raw.less(lhs, Tensor(rhs))
  }

  /// Computes `lhs <= rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.<=` supports broadcasting.
  @inlinable @inline(__always)
  static func .<= (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return Raw.lessEqual(lhs, Tensor(rhs))
  }

  /// Computes `lhs > rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.>` supports broadcasting.
  @inlinable @inline(__always)
  static func .> (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return Raw.greater(lhs, Tensor(rhs))
  }

  /// Computes `lhs >= rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.>=` supports broadcasting.
  @inlinable @inline(__always)
  static func .>= (lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return Raw.greaterEqual(lhs, Tensor(rhs))
  }
}

extension Tensor : Comparable where Scalar : Numeric & Comparable {
  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically less than that of the second argument.
  @inlinable @inline(__always)
  public static func < (lhs: Tensor, rhs: Tensor) -> Bool {
    return (lhs .< rhs).all()
  }

  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically less than or equal to that of the second argument.
  @inlinable @inline(__always)
  public static func <= (lhs: Tensor, rhs: Tensor) -> Bool {
    return (lhs .<= rhs).all()
  }

  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically greater than that of the second argument.
  @inlinable @inline(__always)
  public static func > (lhs: Tensor, rhs: Tensor) -> Bool {
    return (lhs .> rhs).all()
  }

  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically greater than or equal to that of the second argument.
  @inlinable @inline(__always)
  public static func >= (lhs: Tensor, rhs: Tensor) -> Bool {
    return (lhs .>= rhs).all()
  }
}

public extension Tensor where Scalar : Numeric & Comparable {
  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically less than that of the second argument.
  @inlinable @inline(__always)
  static func < (lhs: Tensor, rhs: Scalar) -> Bool {
    return (lhs .< rhs).all()
  }

  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically less than or equal to that of the second argument.
  @inlinable @inline(__always)
  static func <= (lhs: Tensor, rhs: Scalar) -> Bool {
    return (lhs .<= rhs).all()
  }

  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically greater than that of the second argument.
  @inlinable @inline(__always)
  static func > (lhs: Tensor, rhs: Scalar) -> Bool {
    return (lhs .> rhs).all()
  }

  /// Returns a Boolean value indicating whether the value of the first argument
  /// is lexicographically greater than or equal to that of the second argument.
  @inlinable @inline(__always)
  static func >= (lhs: Tensor, rhs: Scalar) -> Bool {
    return (lhs .>= rhs).all()
  }
}

public extension Tensor where Scalar : Equatable {
  /// Computes `lhs != rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.==` supports broadcasting.
  @inlinable @inline(__always)
  static func .==(lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Raw.equal(lhs, rhs)
  }

  /// Computes `lhs != rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.!=` supports broadcasting.
  @inlinable @inline(__always)
  static func .!=(lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Raw.notEqual(lhs, rhs)
  }

  /// Computes `lhs == rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.==` supports broadcasting.
  @inlinable @inline(__always)
  static func .==(lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) .== rhs
  }

  /// Computes `lhs != rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.!=` supports broadcasting.
  @inlinable @inline(__always)
  static func .!=(lhs: Scalar, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) .!= rhs
  }

  /// Computes `lhs == rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.==` supports broadcasting.
  @inlinable @inline(__always)
  static func .==(lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return lhs .== Tensor(rhs)
  }

  /// Computes `lhs != rhs` element-wise and returns a `Tensor` of Boolean
  /// scalars.
  /// - Note: `.!=` supports broadcasting.
  @inlinable @inline(__always)
  static func .!=(lhs: Tensor, rhs: Scalar) -> Tensor<Bool> {
    return lhs .!= Tensor(rhs)
  }
}

infix operator ≈ : ComparisonPrecedence

public extension Tensor where Scalar : FloatingPoint & Equatable {
  /// Returns a `Tensor` of Boolean values indicating whether the elements of
  /// `self` are approximately equal to those of `other`.
  @inlinable @inline(__always)
  func elementsApproximatelyEqual(_ other: Tensor,
                                  tolerance: Double = 0.00001) -> Tensor<Bool> {
    return Raw.approximateEqual(self, other, tolerance: tolerance)
  }
}

public extension Tensor where Scalar == Bool {
  /// Computes `!self` element-wise.
  @inlinable @inline(__always)
  func elementsLogicalNot() -> Tensor {
    return Raw.logicalNot(self)
  }

  /// Computes `self && other` element-wise.
  /// - Note: `&&` supports broadcasting.
  @inlinable @inline(__always)
  func elementsLogicalAnd(_ other: Tensor) -> Tensor {
    return Raw.logicalAnd(self, other)
  }

  /// Computes `self && other` element-wise, broadcasting `other`.
  @inlinable @inline(__always)
  func elementsLogicalAnd(_ other: Scalar) -> Tensor {
    return elementsLogicalAnd(Tensor(other))
  }

  /// Computes `self || other` element-wise.
  @inlinable @inline(__always)
  func elementsLogicalOr(_ other: Tensor) -> Tensor {
    return Raw.logicalOr(self, other)
  }

  /// Computes `self || other` element-wise, broadcasting `other`.
  @inlinable @inline(__always)
  func elementsLogicalOr(_ other: Scalar) -> Tensor {
    return elementsLogicalOr(Tensor(other))
  }
}

//===----------------------------------------------------------------------===//
// Transforms
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpTransposed(withPermutations:)
    where Scalar : TensorFlowFloatingPoint
  )
  func transposed(
    withPermutations permutations: Tensor<Int32>
  ) -> Tensor {
    return Raw.transpose(self, perm: permutations)
  }

  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpTransposed(withPermutations:)
    where Scalar : TensorFlowFloatingPoint
  )
  func transposed(withPermutations permutations: [Int]) -> Tensor {
    let permutations = permutations.map(Int32.init)
    return transposed(withPermutations: Tensor<Int32>(permutations))
  }

  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpTransposed(withPermutations:)
    where Scalar : TensorFlowFloatingPoint
  )
  func transposed(withPermutations permutations: Int...) -> Tensor {
    return transposed(withPermutations: permutations)
  }

  /// Returns a transposed tensor, with dimensions permuted in reverse order.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpTransposed()
    where Scalar : TensorFlowFloatingPoint
  )
  func transposed() -> Tensor {
    let defaultPermutations = rankTensor - 1 - Tensor<Int32>(
      rangeFrom: 0, to: Int32(rank), stride: 1
    )
    return transposed(withPermutations: Tensor<Int32>(defaultPermutations))
  }
}

public extension Tensor {
  /// Returns a concatenated tensor of the given tensors.
  /// - Precondition: The tensors must have the same dimensions, except for the
  ///   specified axis.
  /// - Precondition: The axis must be in the range `-rank..<rank`.
  init(concatenating tensors: [Tensor<Scalar>], alongAxis axis: Int = 0) {
    self = Raw.concatV2(tensors, axis: Tensor<Int32>(Int32(axis)))
  }

  /// Concatenates tensors along the specified axis.
  /// - Precondition: The tensors must have the same dimensions, except for the
  ///   specified axis.
  /// - Precondition: The axis must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(vjp: _vjpConcatenated where Scalar : TensorFlowFloatingPoint)
  func concatenated(with other: Tensor, alongAxis axis: Int = 0) -> Tensor {
    return Raw.concatV2([self, other], axis: Tensor<Int32>(Int32(axis)))
  }

  /// Concatenation operator.
  /// - Note: `++` is a custom operator that does not exist in Swift, but does
  ///   in Haskell/Scala. Its addition is not an insignificant language change
  ///   and may be controversial. The existence/naming of `++` will be discussed
  ///   during a later API design phase.
  @inlinable @inline(__always)
  @differentiable(where Scalar : TensorFlowFloatingPoint)
  static func ++ (lhs: Tensor, rhs: Tensor) -> Tensor {
    return lhs.concatenated(with: rhs)
  }
}

internal extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable @inline(__always)
  func _vjpConcatenated(with other: Tensor, alongAxis axis: Int)
    -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    let idx = axis < 0 ? axis + rank : axis
    let splits = Tensor<Int32>([shapeTensor[idx], other.shapeTensor[idx]])
    return (concatenated(with: other, alongAxis: axis), { result in
      let ret: (TensorHandle<Scalar>, TensorHandle<Scalar>) = #tfop("SplitV",
        result,
        splits,
        Tensor<Int32>(Int32(axis)),
        num_split: Int64(2),
        T$dtype: Scalar.tensorFlowDataType,
        Tlen$dtype: Int32.tensorFlowDataType)
      return (Tensor(handle: ret.0), Tensor(handle: ret.1))
    })
  }
}

//===----------------------------------------------------------------------===//
// Element-wise math functions
//===----------------------------------------------------------------------===//

// Export Glibc/Darwin math functions. We should not require users to import
// Foundation/Darwin/Glibc in order to use scalar math functions.
//
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
@_exported import Darwin.C
#else
@_exported import Glibc
#endif
//
// FIXME(rxwei): Scoped imports are not yet supported in parseable module
// interfaces, so `@_exported import` won't work. When that becomes supported,
// switch to `@_exported import` by removing `import Darwin.C/Glibc` above and
// uncommenting the following lines. In the meantime, consider using indirect
// wrappers for each function so that random libc symbols won't be leaked to
// users' code completion.
//
// #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
// @_exported import func Darwin.C.sin
// @_exported import func Darwin.C.cos
// @_exported import func Darwin.C.tan
// @_exported import func Darwin.C.sinf
// @_exported import func Darwin.C.cosf
// @_exported import func Darwin.C.tanf
// @_exported import func Darwin.C.sinh
// @_exported import func Darwin.C.cosh
// @_exported import func Darwin.C.tanh
// @_exported import func Darwin.C.sinhf
// @_exported import func Darwin.C.coshf
// @_exported import func Darwin.C.tanhf
// @_exported import func Darwin.C.log
// @_exported import func Darwin.C.logf
// @_exported import func Darwin.C.exp
// @_exported import func Darwin.C.expf
// @_exported import func Darwin.C.pow
// @_exported import func Darwin.C.powf
// #else
// @_exported import func Glibc.sin
// @_exported import func Glibc.cos
// @_exported import func Glibc.tan
// @_exported import func Glibc.sinf
// @_exported import func Glibc.cosf
// @_exported import func Glibc.tanf
// @_exported import func Glibc.sinh
// @_exported import func Glibc.cosh
// @_exported import func Glibc.tanh
// @_exported import func Glibc.sinhf
// @_exported import func Glibc.coshf
// @_exported import func Glibc.tanhf
// @_exported import func Glibc.log
// @_exported import func Glibc.logf
// @_exported import func Glibc.exp
// @_exported import func Glibc.expf
// @_exported import func Glibc.pow
// @_exported import func Glibc.powf
// #endif

public extension Tensor where Scalar : SignedNumeric {
  /// Computes the negation of the specified tensor element-wise.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpNegate(_:)
    where Scalar : TensorFlowFloatingPoint
  )
  static prefix func - (rhs: Tensor) -> Tensor {
    return Raw.neg(rhs)
  }
}

/// Computes the absolute value of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpAbs(_:) where T : TensorFlowFloatingPoint)
public func abs<T : SignedNumeric>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.abs(x)
}

/// Computes the natural logarithm of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpLog(_:) where T : TensorFlowFloatingPoint)
public func log<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.log(x)
}

/// Computes `sin` of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpSin(_:) where T : TensorFlowFloatingPoint)
public func sin<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.sin(x)
}

/// Computes `cos` of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpCos(_:) where T : TensorFlowFloatingPoint)
public func cos<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.cos(x)
}

/// Computes `tan` of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpTan(_:) where T : TensorFlowFloatingPoint)
public func tan<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.tan(x)
}

/// Computes `sinh` of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpSinh(_:) where T : TensorFlowFloatingPoint)
public func sinh<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.sinh(x)
}

/// Computes `cosh` of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpCosh(_:) where T : TensorFlowFloatingPoint)
public func cosh<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.cosh(x)
}

/// Computes `tanh` of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpTanh(_:) where T : TensorFlowFloatingPoint)
public func tanh<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.tanh(x)
}

/// Computes the square root of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpSqrt(_:) where T : TensorFlowFloatingPoint)
public func sqrt<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.sqrt(x)
}

/// Computes the inverse square root of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpRsqrt(_:) where T : TensorFlowFloatingPoint)
public func rsqrt<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.rsqrt(x)
}

/// Computes `exp` of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpExp(_:) where T : TensorFlowFloatingPoint)
public func exp<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.exp(x)
}

/// Computes the ceiling of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpCeil(_:) where T : TensorFlowFloatingPoint)
public func ceil<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.ceil(x)
}

/// Computes the floor of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpFloor(_:) where T : TensorFlowFloatingPoint)
public func floor<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.floor(x)
}

/// Computes the power of the first tensor to the second tensor.
@inlinable @inline(__always)
@differentiable(vjp: _vjpPow(_:_:) where T : TensorFlowFloatingPoint)
public func pow<T>(_ lhs: Tensor<T>, _ rhs: Tensor<T>) -> Tensor<T>
  where T : FloatingPoint {
  return Raw.pow(lhs, rhs)
}

/// Computes the power of the scalar to the tensor, broadcasting the scalar.
@inlinable @inline(__always)
// @differentiable(where T : TensorFlowFloatingPoint)
public func pow<T>(_ lhs: T, _ rhs: Tensor<T>) -> Tensor<T>
  where T : FloatingPoint {
  return pow(Tensor(lhs), rhs)
}

/// Computes the power of the tensor to the scalar, broadcasting the scalar.
@inlinable @inline(__always)
// @differentiable(where T : TensorFlowFloatingPoint)
public func pow<T>(_ lhs: Tensor<T>, _ rhs: T) -> Tensor<T>
  where T : FloatingPoint {
  return pow(lhs, Tensor(rhs))
}

/// Computes the element-wise maximum of two tensors.
/// - Note: `max` supports broadcasting.
@inlinable @inline(__always)
@differentiable(vjp: _vjpMax(_:_:) where T : TensorFlowFloatingPoint)
public func max<T>(_ lhs: Tensor<T>, _ rhs: Tensor<T>) -> Tensor<T>
  where T : Numeric & Comparable {
  return Raw.maximum(lhs, rhs)
}

/// Computes the element-wise maximum of the scalar and the tensor, broadcasting
/// the scalar.
@inlinable @inline(__always)
//@differentiable(where T : TensorFlowFloatingPoint)
public func max<T>(_ lhs: T, _ rhs: Tensor<T>) -> Tensor<T>
  where T : Numeric & Comparable {
  return max(Tensor(lhs), rhs)
}

/// Computes the element-wise maximum of the scalar and the tensor, broadcasting
/// the scalar.
@inlinable @inline(__always)
// @differentiable(where T : TensorFlowFloatingPoint)
public func max<T>(_ lhs: Tensor<T>, _ rhs: T) -> Tensor<T>
  where T : Numeric & Comparable {
  return max(lhs, Tensor(rhs))
}

/// Computes the element-wise minimum of two tensors.
/// - Note: `min` supports broadcasting.
@inlinable @inline(__always)
@differentiable(vjp: _vjpMin(_:_:) where T : TensorFlowFloatingPoint)
public func min<T>(_ lhs: Tensor<T>, _ rhs: Tensor<T>) -> Tensor<T>
  where T : Numeric & Comparable {
  return Raw.minimum(lhs, rhs)
}

/// Computes the element-wise minimum of the scalar and the tensor, broadcasting
/// the scalar.
@inlinable @inline(__always)
// @differentiable(where T : TensorFlowFloatingPoint)
public func min<T>(_ lhs: T, _ rhs: Tensor<T>) -> Tensor<T>
  where T : Numeric & Comparable {
  return min(Tensor(lhs), rhs)
}

/// Computes the element-wise minimum of the scalar and the tensor, broadcasting
/// the scalar.
@inlinable @inline(__always)
// @differentiable(where T : TensorFlowFloatingPoint)
public func min<T>(_ lhs: Tensor<T>, _ rhs: T) -> Tensor<T>
  where T : Numeric & Comparable {
  return min(lhs, Tensor(rhs))
}

/// Computes the square of the tensor.
public extension Tensor where Scalar : Numeric {
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpSquared()
    where Scalar : TensorFlowFloatingPoint
  )
  func squared() -> Tensor {
    return Raw.square(self)
  }
}

/// Computes the log-softmax of the specified tensor element-wise.
@inlinable @inline(__always)
@differentiable(vjp: _vjpLogSoftmax(_:) where T : TensorFlowFloatingPoint)
public func logSoftmax<T : FloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
  return Raw.logSoftmax(logits: x)
}

//===----------------------------------------------------------------------===//
// Selection
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar == Bool {
  /// Returns a new tensor containing elements from either `left` or `right`,
  /// depending on the elements of `self`.
  ///
  /// `self` acts as a mask that chooses, based on the value at each scalar,
  ///  whether the corresponding scalar in the output should be taken from
  /// `left` (if `true`) or `right` (if `false`).
  ///
  /// - Precondition: `left` and `right` must have the same shape. If
  ///   `left` and `right` are scalar, then `self` must also be scalar. If
  ///   `left` and `right` have rank greater than or equal to 1, then `self`
  ///   must be either have the same shape as `left` or be a 1-D `Tensor` such
  ///   that `self.scalarCount == left[0]`.
  @available(*, deprecated, message: "Use '.replacing(with:mask:)' instead")
  @inlinable
  func selecting<T>(_ left: Tensor<T>, _ right: Tensor<T>) -> Tensor<T> {
    return left.replacing(with: right, where: self)
  }
}

public extension Tensor {
  /// Replaces elements of this tensor with `other` in the lanes where `mask` is
  /// `true`.
  ///
  /// - Precondition: `self` and `other` must have the same shape. If
  ///   `self` and `other` are scalar, then `mask` must also be scalar. If
  ///   `self` and `other` have rank greater than or equal to `1`, then `mask`
  ///   must be either have the same shape as `self` or be a 1-D `Tensor` such
  ///   that `mask.scalarCount == self.shape[0]`.
  @inlinable
  @differentiable(wrt: (self, other),
                  vjp: _vjpReplacing where Scalar : TensorFlowFloatingPoint)
  func replacing(with other: Tensor,
                 where mask: Tensor<Bool>) -> Tensor {
    return Raw.select(condition: mask, t: self, e: other)
  }
}

public extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  internal func _vjpReplacing(with other: Tensor, where mask: Tensor<Bool>)
    -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return (replacing(with: other, where: mask), { v in
      let zeros = Tensor(zeros: v.shape)
      return (v.replacing(with: zeros, where: mask),
              zeros.replacing(with: v, where: mask))
    })
  }
}

//===----------------------------------------------------------------------===//
// Reduction
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar == Bool {
  /// Returns `true` if all scalars are equal to `true`. Otherwise, returns
  /// `false`.
  // NOTE: This overload is necessary, otherwise `all()` would refer
  // to the variadic method `all(squeezingAxes:)` with zero indices.
  @inlinable @inline(__always)
  func all() -> Bool {
    let axes = Tensor<Int32>(rangeFrom: 0, to: Int32(rank), stride: 1)
    return _TFGetScalarOrDie(Raw.all(self, reductionIndices: axes).handle)
  }

  /// Returns `true` if any scalars are equal to `true`. Otherwise, returns
  /// `false`.
  // NOTE: This overload is necessary, otherwise `any()` would refer
  // to the variadic method `any(squeezingAxes:)` with zero indices.
  @inlinable @inline(__always)
  func any() -> Bool {
    let axes = Tensor<Int32>(rangeFrom: 0, to: Int32(rank), stride: 1)
    return _TFGetScalarOrDie(Raw.any(self, reductionIndices: axes).handle)
  }

  /// Performs a logical AND operation along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func all(squeezingAxes axes: Int...) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.all(self, reductionIndices: Tensor<Int32>(axes), keepDims: false)
  }

  /// Performs a logical AND operation along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func any(squeezingAxes axes: Int...) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.any(self, reductionIndices: Tensor<Int32>(axes), keepDims: false)
  }

  /// Performs a logical AND operation along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func all(alongAxes axes: Int...) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.all(self, reductionIndices: Tensor<Int32>(axes), keepDims: true)
  }

  /// Performs a logical OR operation along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func any(alongAxes axes: Int...) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.any(self, reductionIndices: Tensor<Int32>(axes), keepDims: true)
  }
}

public extension Tensor where Scalar : Numeric & Comparable {
  // NOTE: This overload is necessary, otherwise `min()` would refer
  // to the variadic method `min(squeezingAxes:)` with zero indices.
  @inlinable @inline(__always)
  func min() -> Tensor {
    let axes = Tensor<Int32>(rangeFrom: 0, to: Int32(rank), stride: 1)
    return Raw.min(self, reductionIndices: axes)
  }

  // NOTE: This overload is necessary, otherwise `max()` would refer
  // to the variadic method `max(squeezingAxes:)` with zero indices.
  @inlinable @inline(__always)
  func max() -> Tensor {
    let axes = Tensor<Int32>(rangeFrom: 0, to: Int32(rank), stride: 1)
    return Raw.max(self, reductionIndices: axes)
  }

  /// Returns the maximum values along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func max(squeezingAxes axes: [Int]) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.max(self, reductionIndices: Tensor<Int32>(axes), keepDims: false)
  }

  /// Returns the maximum values along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func max(squeezingAxes axes: Int...) -> Tensor {
    return max(squeezingAxes: axes)
  }

  /// Returns the minimum values along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func min(squeezingAxes axes: [Int]) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.min(self, reductionIndices: Tensor<Int32>(axes), keepDims: false)
  }

  /// Returns the minimum values along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func min(squeezingAxes axes: Int...) -> Tensor {
    return min(squeezingAxes: axes)
  }

  /// Returns the indices of the maximum values along the specified axes. The
  /// reduced dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func argmax(squeezingAxis axis: Int) -> Tensor<Int32> {
    return Raw.argMax(self, dimension: Tensor<Int32>(Int32(axis)))
  }

  /// Returns the indices of the minimum values along the specified axes. The
  /// reduced dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func argmin(squeezingAxis axis: Int) -> Tensor<Int32> {
    return Raw.argMin(self, dimension: Tensor<Int32>(Int32(axis)))
  }

  /// Returns the minimum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func min(alongAxes axes: [Int]) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.min(self, reductionIndices: Tensor<Int32>(axes), keepDims: true)
  }

  /// Returns the minimum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func min(alongAxes axes: Int...) -> Tensor {
    return min(alongAxes: axes)
  }

  /// Returns the minimum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func max(alongAxes axes: [Int]) -> Tensor {
    let axes = axes.map(Int32.init)
    return Raw.max(self, reductionIndices: Tensor<Int32>(axes), keepDims: true)
  }

  /// Returns the minimum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func max(alongAxes axes: Int...) -> Tensor {
    return max(alongAxes: axes)
  }

  /// Returns the index of the maximum value of the flattened scalars.
  @inlinable @inline(__always)
  func argmax() -> Tensor<Int32> {
    return flattened().argmax(squeezingAxis: 0)
  }

  /// Returns the index of the minimum value of the flattened scalars.
  @inlinable @inline(__always)
  func argmin() -> Tensor<Int32> {
    return flattened().argmin(squeezingAxis: 0)
  }
}

// MARK: - Numeric reduction

public extension Tensor where Scalar : Numeric {
  // MARK: - Sum

  /// Returns the sum along the specified axes. The reduced dimensions are
  /// removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpSum(squeezingAxes:)
    where Scalar : TensorFlowFloatingPoint
  )
  func sum(squeezingAxes axes: Tensor<Int32>) -> Tensor {
    return Raw.sum(self, reductionIndices: Tensor<Int32>(axes), keepDims: false)
  }

  /// Returns the sum along the specified axes. The reduced dimensions are
  /// removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func sum(squeezingAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return sum(squeezingAxes: Tensor<Int32>(axes))
  }

  /// Returns the sum along the specified axes. The reduced dimensions are
  /// removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func sum(squeezingAxes axes: Int...) -> Tensor {
    return sum(squeezingAxes: axes)
  }

  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func sum() -> Tensor {
    return flattened().sum(squeezingAxes: 0)
  }

  /// Returns the sum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpSum(squeezingAxes:)
    where Scalar : TensorFlowFloatingPoint
  )
  func sum(alongAxes axes: Tensor<Int32>) -> Tensor {
    return Raw.sum(self, reductionIndices: axes, keepDims: true)
  }

  /// Returns the sum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func sum(alongAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return sum(alongAxes: Tensor<Int32>(axes))
  }

  /// Returns the sum along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func sum(alongAxes axes: Int...) -> Tensor {
    return sum(alongAxes: axes)
  }

  // MARK: - Product

  /// Returns the product along the specified axes. The reduced dimensions are
  /// removed.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  // TODO: Make this @differentiable.
  @inlinable @inline(__always)
  func product(squeezingAxes axes: Tensor<Int32>) -> Tensor {
    return Raw.prod(self, reductionIndices: axes, keepDims: false)
  }

  /// Returns the product along the specified axes. The reduced dimensions are
  /// removed.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  func product(squeezingAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return product(squeezingAxes: Tensor<Int32>(axes))
  }

  /// Returns the product along the specified axes. The reduced dimensions are
  /// removed.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  func product(squeezingAxes axes: Int...) -> Tensor {
    return product(squeezingAxes: axes)
  }

  @inlinable @inline(__always)
  func product() -> Tensor {
    return flattened().product(squeezingAxes: 0)
  }

  /// Returns the product along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func product(alongAxes axes: Tensor<Int32>) -> Tensor {
    return Raw.prod(self, reductionIndices: axes, keepDims: true)
  }

  /// Returns the product along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func product(alongAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return product(alongAxes: Tensor<Int32>(axes))
  }

  /// Returns the product along the specified axes. The reduced dimensions are
  /// retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  func product(alongAxes axes: Int...) -> Tensor {
    return product(alongAxes: axes)
  }

  // MARK: - Mean

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpMean(squeezingAxes:)
    where Scalar : TensorFlowFloatingPoint
  )
  func mean(squeezingAxes axes: Tensor<Int32>) -> Tensor {
    return Raw.mean(self, reductionIndices: axes, keepDims: false)
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func mean(squeezingAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return mean(squeezingAxes: Tensor<Int32>(axes))
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are removed.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank...rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func mean(squeezingAxes axes: Int...) -> Tensor {
    return mean(squeezingAxes: axes)
  }

  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func mean() -> Tensor {
    return flattened().mean(squeezingAxes: [0])
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(
    wrt: self, vjp: _vjpMean(alongAxes:)
    where Scalar : TensorFlowFloatingPoint
  )
  func mean(alongAxes axes: Tensor<Int32>) -> Tensor {
    return Raw.mean(self, reductionIndices: axes, keepDims: true)
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func mean(alongAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return mean(alongAxes: Tensor<Int32>(axes))
  }

  /// Returns the arithmetic mean along the specified axes. The reduced
  /// dimensions are retained with value 1.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func mean(alongAxes axes: Int...) -> Tensor {
    return mean(alongAxes: axes)
  }

  // MARK: - Variance

  /// Returns the variance along the specified axes. The reduced dimensions are
  /// removed. Does not apply Bessel's correction.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func variance(squeezingAxes axes: Tensor<Int32>) -> Tensor {
    let squaredDiff = (self - mean(alongAxes: axes)).squared()
    return squaredDiff.mean(squeezingAxes: axes)
  }

  /// Returns the variance along the specified axes. The reduced dimensions are
  /// removed. Does not apply Bessel's correction.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func variance(squeezingAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return variance(squeezingAxes: Tensor<Int32>(axes))
  }

  /// Returns the variance along the specified axes. The reduced dimensions are
  /// retained with value 1. Does not apply Bessel's correction.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func variance(squeezingAxes axes: Int...) -> Tensor {
    return variance(squeezingAxes: axes)
  }

  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  @inlinable @inline(__always)
  func variance() -> Tensor {
    let mean = self.mean()
    let squaredDiff = (self - mean).squared()
    return squaredDiff.mean()
  }

  /// Returns the variance along the specified axes. The reduced dimensions are
  /// retained with value 1. Does not apply Bessel's correction.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func variance(alongAxes axes: Tensor<Int32>) -> Tensor {
    let squaredDiff = (self - mean(alongAxes: axes)).squared()
    return squaredDiff.mean(alongAxes: axes)
  }

  /// Returns the variance along the specified axes. The reduced dimensions are
  /// retained with value 1. Does not apply Bessel's correction.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func variance(alongAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return variance(alongAxes: Tensor<Int32>(axes))
  }

  /// Returns the variance along the specified axes. The reduced dimensions are
  /// retained with value 1. Does not apply Bessel's correction.
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func variance(alongAxes axes: Int...) -> Tensor {
    return variance(alongAxes: axes)
  }
}

// TODO: Consider making the return type be generic over `FloatingPoint` types
// so that `self`'s scalar type can be any `Numeric` type.
public extension Tensor where Scalar : TensorFlowFloatingPoint {
  /// Returns the standard deviation of the elements along the specified axes.
  /// The reduced dimensions are retained with value `1`. Does not apply
  /// Bessel's correction.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self)
  func standardDeviation(squeezingAxes axes: Tensor<Int32>) -> Tensor {
    return sqrt(variance(squeezingAxes: axes))
  }

  /// Returns the standard deviation of the elements along the specified axes.
  /// The reduced dimensions are retained with value `1`. Does not apply
  /// Bessel's correction.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self)
  func standardDeviation(squeezingAxes axes: [Int]) -> Tensor {
    return sqrt(variance(squeezingAxes: axes))
  }

  /// Returns the standard deviation of the elements along the specified axes.
  /// The reduced dimensions are retained with value `1`. Does not apply
  /// Bessel's correction.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @differentiable(wrt: self)
  func standardDeviation(squeezingAxes axes: Int...) -> Tensor {
    return standardDeviation(squeezingAxes: axes)
  }

  /// Returns the standard deviation of the elements along the specified axes.
  /// The reduced dimensions are retained with value `1`. Does not apply
  /// Bessel's correction.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @differentiable(wrt: self)
  func standardDeviation() -> Tensor {
    // Reduce along all dimensions.
    return standardDeviation(squeezingAxes: Array(0..<shape.rank))
  }

  /// Returns the standard deviation of the elements along the specified axes.
  /// The reduced dimensions are retained with value `1`. Does not apply
  /// Bessel's correction.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @differentiable(wrt: self)
  func standardDeviation(alongAxes axes: Tensor<Int32>) -> Tensor {
    return sqrt(variance(alongAxes: axes))
  }

  /// Returns the standard deviation of the elements along the specified axes.
  /// The reduced dimensions are retained with value `1`. Does not apply
  /// Bessel's correction.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @differentiable(wrt: self)
  func standardDeviation(alongAxes axes: [Int]) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    let axes = {axes.map(Int32.init)}()
    return standardDeviation(alongAxes: Tensor<Int32>(axes))
  }

  /// Returns the standard deviation of the elements along the specified axes.
  /// The reduced dimensions are retained with value `1`. Does not apply
  /// Bessel's correction.
  ///
  /// - Parameter axes: The dimensions to reduce.
  /// - Precondition: Each value in `axes` must be in the range `-rank..<rank`.
  @inlinable @inline(__always)
  @differentiable(wrt: self)
  func standardDeviation(alongAxes axes: Int...) -> Tensor {
    return sqrt(variance(alongAxes: axes))
  }
}

//===----------------------------------------------------------------------===//
// Tensor properties
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// The rank of the tensor, represented as a `Tensor<Int32>`.
  @inlinable
  var rankTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return Raw.rank(self)
    }
  }

  /// The dimensions of the tensor, represented as a `Tensor<Int32>`.
  @inlinable
  var shapeTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return Raw.shape(self)
    }
  }

  /// The number of scalars in the tensor, represented as a `Tensor<Int32>`.
  @inlinable
  var scalarCountTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return Raw.size(self)
    }
  }
}

//===----------------------------------------------------------------------===//
// Broadcasting
//===----------------------------------------------------------------------===//

public extension Tensor {
  @inlinable @inline(__always)
  func broadcast(toShape shape: Tensor<Int32>) -> Tensor {
    return Raw.broadcastTo(self, shape: shape)
  }

  @inlinable @inline(__always)
  func broadcast(to shape: TensorShape) -> Tensor {
    return broadcast(toShape: Tensor<Int32>(shape.dimensions.map(Int32.init)))
  }

  /// Broadcast to the same shape as the specified `Tensor`.
  /// - Precondition: The specified shape must be compatible for broadcasting.
  @inlinable @inline(__always)
  func broadcast<OtherScalar>(like other: Tensor<OtherScalar>) -> Tensor {
    return broadcast(toShape: other.shapeTensor)
  }
}

public extension Tensor where Scalar : Numeric {
  @inlinable
  func unbroadcast(toShape otherShape: Tensor<Int32>) -> Tensor {
    let rankDiff = (rankTensor - otherShape.scalarCountTensor).rankLifted()
    let ones: Tensor<Int32> = Raw.fill(dims: rankDiff, value: Tensor<Int32>(1))
    let paddedShape = ones ++ otherShape
    let nonEqualIndices = paddedShape .!= shapeTensor
    let broadcastIndices = Raw.where_(nonEqualIndices).flattened()
    let unbroadcasted: Tensor = Raw.sum(
      self, reductionIndices: Tensor<Int32>(broadcastIndices), keepDims: false)
    return Raw.reshape(unbroadcasted, shape: otherShape)
  }

  @inlinable @inline(__always)
  func unbroadcast<OtherScalar>(like other: Tensor<OtherScalar>) -> Tensor {
    return unbroadcast(toShape: other.shapeTensor)
  }

  @inlinable @inline(__always)
  func unbroadcast(to shape: TensorShape) -> Tensor {
    return unbroadcast(toShape: Tensor<Int32>(shape.dimensions.map(Int32.init)))
  }

  @inlinable @inline(__always)
  static func .= (lhs: inout Tensor, rhs: Tensor) {
    lhs = rhs.broadcast(like: lhs)
  }
}

//===----------------------------------------------------------------------===//
// Padding
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  /// Returns a padded tensor according to the specified padding sizes.
  @inlinable
  func padded(
    forSizes sizes: [(before: Int, after: Int)],
    with value: Scalar = 0
  ) -> Tensor {
    let paddings = Tensor<Int32>(
      shape: [sizes.count, 2],
      scalars: sizes.flatMap { [Int32($0.before), Int32($0.after)] }
    )
    return Raw.padV2(self, paddings: paddings, constantValues: Tensor(value))
  }
}

//===----------------------------------------------------------------------===//
// Indexing and slicing
//===----------------------------------------------------------------------===//

// TODO: Negative indexing and strides syntax.

public extension Tensor {
  /// Extracts a slice from the tensor defined by lower and upper bounds for
  /// each dimension.
  ///
  /// - Parameter lowerBounds: The lower bounds at each dimension.
  /// - Parameter upperBounds: The upper bounds at each dimension.
  @inlinable
  @differentiable(wrt: self)
  func slice(lowerBounds: [Int], upperBounds: [Int]) -> Tensor {
    // TODO: Precondition `lowerBounds.count == upperBounds.count`,
    // preferably in graph.
    // TODO: Differentiating control flow is not supported yet, thus the thunks.
    let lowerBoundsTensor = Tensor<Int32>({lowerBounds.map(Int32.init)}())
    let upperBoundsTensor = Tensor<Int32>({upperBounds.map(Int32.init)}())
    return slice(
      lowerBounds: lowerBoundsTensor,
      sizes: upperBoundsTensor - lowerBoundsTensor)
  }

  @inlinable
  @differentiable(wrt: self, vjp: _vjpSlice)
  func slice(lowerBounds: Tensor<Int32>, sizes: Tensor<Int32>) -> Tensor {
    return Raw.slice(self, begin: lowerBounds, size: sizes)
  }

  @inlinable
  internal func _vjpSlice(
    lowerBounds: Tensor<Int32>,
    sizes: Tensor<Int32>
  ) -> (Tensor, (Tensor) -> Tensor) {
    let value = slice(lowerBounds: lowerBounds, sizes: sizes)
    let afterPaddings = shapeTensor - value.shapeTensor - lowerBounds
    return (value, { [after = afterPaddings] v in
      let beforePaddings = lowerBounds.expandingShape(at: 1)
      let afterPaddings = after.expandingShape(at: 1)
      let paddings = Tensor<Int32>(
        concatenating: [beforePaddings, afterPaddings], alongAxis: 1)
      return Raw.pad(v, paddings: paddings)
    })
  }
}

public enum TensorRange : TensorRangeExpression {
  case ellipsis
  case newAxis
  case squeezeAxis
  case index(Int)
  case range(Range<Int>, stride: Int)
  case closedRange(ClosedRange<Int>, stride: Int)
  case partialRangeFrom(PartialRangeFrom<Int>, stride: Int)
  case partialRangeUpTo(PartialRangeUpTo<Int>, stride: Int)
  case partialRangeThrough(PartialRangeThrough<Int>, stride: Int)

  public var tensorRange: TensorRange { return self }
}

extension TensorRange : Equatable {
  public static func == (lhs: TensorRange, rhs: TensorRange) -> Bool {
    switch (lhs, rhs) {
    case (.ellipsis, .ellipsis),
         (.newAxis, .newAxis),
         (.squeezeAxis, .squeezeAxis):
      return true
    case (let .index(i1), let .index(i2)): return i1 == i2
    case (let .range(r1, s1), let .range(r2, s2)): return r1 == r2 && s1 == s2
    case (let .closedRange(r1, s1), let .closedRange(r2, s2)):
      return r1 == r2 && s1 == s2
    case (let .partialRangeFrom(r1, s1), let .partialRangeFrom(r2, s2)):
      return r1.lowerBound == r2.lowerBound && s1 == s2
    case (let .partialRangeUpTo(r1, s1), let .partialRangeUpTo(r2, s2)):
      return r1.upperBound == r2.upperBound && s1 == s2
    case (let .partialRangeThrough(r1, s1), let .partialRangeThrough(r2, s2)):
      return r1.upperBound == r2.upperBound && s1 == s2
    default: return false
    }
  }
}

public protocol TensorRangeExpression {
  var tensorRange: TensorRange { get }
}

// TODO: Cannot extend non-nominal type 'UnboundedRange'.
// extension UnboundedRange : TensorRangeExpression {
//   public var tensorRange: TensorRange { return .ellipsis }
// }

extension Int : TensorRangeExpression {
  public var tensorRange: TensorRange { return .index(self) }
}

extension Range : TensorRangeExpression where Bound == Int {
  public var tensorRange: TensorRange {
    return .range(self, stride: 1)
  }
}

extension ClosedRange : TensorRangeExpression where Bound == Int {
  public var tensorRange: TensorRange {
    return .closedRange(self, stride: 1)
  }
}

extension PartialRangeFrom : TensorRangeExpression where Bound == Int {
  public var tensorRange: TensorRange {
    return .partialRangeFrom(self, stride: 1)
  }
}

extension PartialRangeUpTo : TensorRangeExpression where Bound == Int {
  public var tensorRange: TensorRange {
    return .partialRangeUpTo(self, stride: 1)
  }
}

extension PartialRangeThrough : TensorRangeExpression where Bound == Int {
  public var tensorRange: TensorRange {
    return .partialRangeThrough(self, stride: 1)
  }
}

infix operator .. : StridedRangeFormationPrecedence
precedencegroup StridedRangeFormationPrecedence {
  associativity: left
  higherThan: CastingPrecedence
  lowerThan: RangeFormationPrecedence
}

public extension Range where Bound == Int {
  static func .. (range: Range, stride: Int) -> TensorRange {
    return .range(range, stride: stride)
  }
}

public extension ClosedRange where Bound == Int {
  static func .. (range: ClosedRange, stride: Int) -> TensorRange {
    return .closedRange(range, stride: stride)
  }
}

public extension PartialRangeFrom where Bound == Int {
  static func .. (range: PartialRangeFrom, stride: Int) -> TensorRange {
    return .partialRangeFrom(range, stride: stride)
  }
}

public extension PartialRangeUpTo where Bound == Int {
  static func .. (range: PartialRangeUpTo, stride: Int) -> TensorRange {
    return .partialRangeUpTo(range, stride: stride)
  }
}

public extension PartialRangeThrough where Bound == Int {
  static func .. (range: PartialRangeThrough, stride: Int) -> TensorRange {
    return .partialRangeThrough(range, stride: stride)
  }
}

public extension Tensor {
  @_fixed_layout @usableFromInline
  internal struct IndexPath {
    @usableFromInline
    let begin, end, strides: Tensor<Int32>

    @usableFromInline
    let beginMask, endMask, ellipsisMask, newAxisMask, squeezeAxisMask: Int64

    @inlinable
    public init(
      begin: Tensor<Int32>, end: Tensor<Int32>, strides: Tensor<Int32>,
      beginMask: Int64, endMask: Int64, ellipsisMask: Int64, newAxisMask: Int64,
      squeezeAxisMask: Int64
    ) {
      self.begin = begin
      self.end = end
      self.strides = strides
      self.beginMask = beginMask
      self.endMask = endMask
      self.ellipsisMask = ellipsisMask
      self.newAxisMask = newAxisMask
      self.squeezeAxisMask = squeezeAxisMask
    }
  }

  @inlinable
  @differentiable(wrt: self, vjp: _vjpSubscript)
  internal subscript(_ indexPath: IndexPath) -> Tensor {
    get {
      return Raw.stridedSlice(
        self, begin: indexPath.begin, end: indexPath.end,
        strides: indexPath.strides, beginMask: indexPath.beginMask,
        endMask: indexPath.endMask, ellipsisMask: indexPath.ellipsisMask, 
        newAxisMask: indexPath.newAxisMask,
        shrinkAxisMask: indexPath.squeezeAxisMask)
    }
    set {
      self = Raw.tensorStridedSliceUpdate(
        self, begin: indexPath.begin, end: indexPath.end,
        strides: indexPath.strides, value: newValue,
        beginMask: indexPath.beginMask, endMask: indexPath.endMask,
        ellipsisMask: indexPath.ellipsisMask,
        newAxisMask: indexPath.newAxisMask,
        shrinkAxisMask: indexPath.squeezeAxisMask)
    }
  }

  @inlinable
  // TODO: @differentiable(wrt: self)
  subscript(_ ranges: TensorRangeExpression...) -> Tensor {
    get {
      return self[IndexPath(ranges.map { $0.tensorRange })]
    }
    set {
      self[IndexPath(ranges.map { $0.tensorRange })] = newValue
    }
  }

  @usableFromInline
  internal func _vjpSubscript(
    _ indexPath: IndexPath
  ) -> (Tensor, (Tensor) -> Tensor) {
    return (self[indexPath], { [shape = shapeTensor] v in
      Raw.stridedSliceGrad(
        shape: shape, begin: indexPath.begin, end: indexPath.end,
        strides: indexPath.strides, dy: v, beginMask: indexPath.beginMask,
        endMask: indexPath.endMask, ellipsisMask: indexPath.ellipsisMask,
        newAxisMask: indexPath.newAxisMask,
        shrinkAxisMask: indexPath.squeezeAxisMask)
    })
  }
}

internal extension Tensor.IndexPath {
  @inlinable
  init(_ ranges: [TensorRange]) {
    precondition(!ranges.isEmpty, "The tensor range collection cannot be empty.")
    precondition(ranges.count { $0 == TensorRange.ellipsis } < 2,
                 "Only one ellipsis is allowed per tensor range collection.")

    var begin = [Int32](repeating: 0, count: ranges.count)
    var end = [Int32](repeating: 0, count: ranges.count)
    var strides = [Int32](repeating: 1, count: ranges.count)
    var beginMask: Int64 = 0
    var endMask: Int64 = 0
    var ellipsisMask: Int64 = 0
    var newAxisMask: Int64 = 0
    var squeezeAxisMask: Int64 = 0
    for (i, index) in ranges.enumerated() {
      switch index {
      case .ellipsis: ellipsisMask |= 1 << i
      case .newAxis: newAxisMask |= 1 << i
      case .squeezeAxis: squeezeAxisMask |= 1 << i
      case .index(let index):
        begin[i] = Int32(index)
        end[i] = Int32(index) + 1
        squeezeAxisMask |= 1 << i
      case .range(let range, let stride):
        begin[i] = Int32(range.lowerBound)
        end[i] = Int32(range.upperBound)
        strides[i] = Int32(stride)
      case .closedRange(let range, let stride):
        begin[i] = Int32(range.lowerBound)
        switch Int32(range.upperBound) {
        case -1: endMask |= 1 << i
        case let u: end[i] = u + 1
        }
        strides[i] = Int32(stride)
      case .partialRangeFrom(let range, let stride):
        begin[i] = Int32(range.lowerBound)
        strides[i] = Int32(stride)
        endMask |= 1 << i
      case .partialRangeUpTo(let range, let stride):
        end[i] = Int32(range.upperBound)
        strides[i] = Int32(stride)
        beginMask |= 1 << i
      case .partialRangeThrough(let range, let stride):
        end[i] = Int32(range.upperBound) + 1
        strides[i] = Int32(stride)
        beginMask |= 1 << i
      }
    }

    self.begin = Tensor<Int32>(begin)
    self.end = Tensor<Int32>(end)
    self.strides = Tensor<Int32>(strides)
    self.beginMask = beginMask
    self.endMask = endMask
    self.ellipsisMask = ellipsisMask
    self.newAxisMask = newAxisMask
    self.squeezeAxisMask = squeezeAxisMask
  }
}
