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
//     result = #tfop("Add", "tt:t", lhs, rhs)
//     result = #tfop("Const", ":t", dtype: Float.self, value$tensor: 4.0)
//
// The first two parameters to this syntax are the TensorFlow op name as a
// string, and then a constraint string - which specifies information about the
// operands and result type of the op.  The inputs are specified as additional
// arguments that follow.
//
// The constraint string is specified as two colon separated lists:
// "<OPERANDS>:<RESULTS>".  Here are the codes that are recognized for operands
// so far:
//
//    t: the next operand is a TensorHandle, and is an "input" to the TF node.
//    s: the next operand is a standard library integer or FP scalar.
//
// The codes for the results are currently:
//
//    t: the result is a TensorHandle<T>, where the T is the same type as one
//       of the tensor input operands, or the type of the last dtype specified.
//


// Python PEP 465 makes a compelling argument that matrix multiplication should
// not be spelled with the standard * operator, so we need a new one.  We'll use
// this operator, though it is defensible to use a variety of other ones as well.
infix operator ⊗ : MultiplicationPrecedence

// TODO:
// - Consider explicit broadcasting for elementwise binary ops when
//   scalarization and rank getter are implemented.

extension Tensor : TensorProtocol {
  public typealias BoolTensor = Tensor<Bool>
}

extension Tensor1D : TensorProtocol {
  public typealias BoolTensor = Tensor1D<Bool>
}

extension Tensor2D : TensorProtocol {
  public typealias BoolTensor = Tensor2D<Bool>
}

extension Tensor3D : TensorProtocol {
  public typealias BoolTensor = Tensor3D<Bool>
}

extension Tensor4D : TensorProtocol {
  public typealias BoolTensor = Tensor4D<Bool>
}

//===----------------------------------------------------------------------===//
// Elementwise binary arithmetics
//===----------------------------------------------------------------------===//

extension TensorProtocol /*: Numeric*/ where Scalar : Numeric {
  @_inlineable @inline(__always)
  // @differentiable(gradient: _adjointAdd(_:_:primal:seed:))
  public static func +(lhs: Self, rhs: Self) -> Self {
    return Self(handle: #tfop("Add", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  // @differentiable(gradient: _adjointSubtract(_:_:primal:seed:))
  public static func -(lhs: Self, rhs: Self) -> Self {
    return Self(handle: #tfop("Sub", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  // @differentiable(gradient: _adjointMultiply(_:_:primal:seed:))
  public static func *(lhs: Self, rhs: Self) -> Self {
    return Self(handle: #tfop("Mul", "tt:t", lhs.handle, rhs.handle))
  }
}

public extension TensorProtocol where Scalar : Numeric {
  @_inlineable @inline(__always)
  // @differentiable(gradient: _adjointAdd(_:_:primal:seed:))
  static func +(lhs: Self, rhs: Scalar) -> Self {
    return lhs + Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  // @differentiable(gradient: _adjointAdd(_:_:primal:seed:))
  static func +(lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) + rhs
  }

  @_inlineable @inline(__always)
  static func +=(lhs: inout Self, rhs: Self) {
    lhs = lhs + rhs
  }

  @_inlineable @inline(__always)
  static func +=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs + Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  // @differentiable(gradient: _adjointNegate(_:primal:seed:))
  static prefix func -(rhs: Self) -> Self {
    return Self(handle: #tfop("Neg", "t:t", rhs.handle))
  }

  @_inlineable @inline(__always)
  static func -(lhs: Self, rhs: Scalar) -> Self {
    return lhs - Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func -(lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) - rhs
  }

  @_inlineable @inline(__always)
  static func -=(lhs: inout Self, rhs: Self) {
    lhs = lhs - rhs
  }

  @_inlineable @inline(__always)
  static func -=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs - rhs
  }

  @_inlineable @inline(__always)
  static func *(lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) * rhs
  }

  @_inlineable @inline(__always)
  static func *(lhs: Self, rhs: Scalar) -> Self {
    return lhs * Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func /(lhs: Self, rhs: Self) -> Self {
    return Self(handle: #tfop("Div", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func /(lhs: Self, rhs: Scalar) -> Self {
    return lhs / Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func /(lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) / rhs
  }

  @_inlineable @inline(__always)
  static func /=(lhs: inout Self, rhs: Self) {
    lhs = lhs / rhs
  }

  @_inlineable @inline(__always)
  static func /=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs / rhs
  }

  @_inlineable @inline(__always)
  static func %(lhs: Self, rhs: Self) -> Self {
    return Self(handle: #tfop("Mod", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func %(lhs: Self, rhs: Scalar) -> Self {
    return lhs % Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func %(lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) % rhs
  }

  @_inlineable @inline(__always)
  static func %=(lhs: inout Self, rhs: Self) {
    lhs = lhs % rhs
  }

  @_inlineable @inline(__always)
  static func %=(lhs: inout Self, rhs: Scalar) {
    lhs = lhs % rhs
  }
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : Numeric {
  @_inlineable @inline(__always)
  func dot(_ other: Self) -> Self {
    return Self(handle: #tfop("MatMul", "tt:t", self.handle, other.handle))
  }

  @_inlineable @inline(__always)
  static func ⊗ (lhs: Self, rhs: Self) -> Self {
    return lhs.dot(rhs)
  }

  @_inlineable @inline(__always)
  static func ⊗ (lhs: Scalar, rhs: Self) -> Self {
    return Self(handle: _TFMakeScalarTensor(lhs)) ⊗ rhs
  }

  @_inlineable @inline(__always)
  static func ⊗ (lhs: Self, rhs: Scalar) -> Self {
    return lhs ⊗ Self(handle: _TFMakeScalarTensor(rhs))
  }
}

//===----------------------------------------------------------------------===//
// Elementwise binary comparison
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar : Comparable {
  @_inlineable @inline(__always)
  static func < (lhs: Self, rhs: Self) -> BoolTensor {
    return BoolTensor(handle: #tfop("Less", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func < (lhs: Self, rhs: Scalar) -> BoolTensor {
    return lhs < Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func < (lhs: Scalar, rhs: Self) -> BoolTensor {
    return Self(handle: _TFMakeScalarTensor(lhs)) < rhs
  }

  @_inlineable @inline(__always)
  static func <= (lhs: Self, rhs: Self) -> BoolTensor {
    return BoolTensor(handle:
      #tfop("LessEqual", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func <= (lhs: Self, rhs: Scalar) -> BoolTensor {
    return lhs <= Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func <= (lhs: Scalar, rhs: Self) -> BoolTensor {
    return Self(handle: _TFMakeScalarTensor(lhs)) <= rhs
  }

  @_inlineable @inline(__always)
  static func > (lhs: Self, rhs: Self) -> BoolTensor {
    return BoolTensor(handle: #tfop("Greater", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func > (lhs: Self, rhs: Scalar) -> BoolTensor {
    return lhs > Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func > (lhs: Scalar, rhs: Self) -> BoolTensor {
    return Self(handle: _TFMakeScalarTensor(lhs)) > rhs
  }

  @_inlineable @inline(__always)
  static func >= (lhs: Self, rhs: Self) -> BoolTensor {
    return BoolTensor(handle:
      #tfop("GreaterEqual", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func >= (lhs: Self, rhs: Scalar) -> BoolTensor {
    return lhs >= Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func >= (lhs: Scalar, rhs: Self) -> BoolTensor {
    return Self(handle: _TFMakeScalarTensor(lhs)) >= rhs
  }
}

public extension TensorProtocol where Scalar : Equatable {
  @_inlineable @inline(__always)
  static func == (lhs: Self, rhs: Self) -> BoolTensor {
    return BoolTensor(handle: #tfop("Equal", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func == (lhs: Self, rhs: Scalar) -> BoolTensor {
    return lhs == Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func == (lhs: Scalar, rhs: Self) -> BoolTensor {
    return Self(handle: _TFMakeScalarTensor(lhs)) == rhs
  }

  @_inlineable @inline(__always)
  static func != (lhs: Self, rhs: Self) -> BoolTensor {
    return BoolTensor(handle: #tfop("NotEqual", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable @inline(__always)
  static func != (lhs: Self, rhs: Scalar) -> BoolTensor {
    return lhs != Self(handle: _TFMakeScalarTensor(rhs))
  }

  @_inlineable @inline(__always)
  static func != (lhs: Scalar, rhs: Self) -> BoolTensor {
    return Self(handle: _TFMakeScalarTensor(lhs)) != rhs
  }
}

//===----------------------------------------------------------------------===//
// Transforms
//===----------------------------------------------------------------------===//

public extension TensorProtocol {
  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @_inlineable @inline(__always)
  // @differentiable(
  //   withRespectTo: (self),
  //   gradient: _adjointTransposed(_:_:primal:seed:)
  // )
  func transposed(withPermutations permutations: Tensor<Int>) -> Self {
    return Self(handle:
      #tfop("Transpose", "tt:t", handle, permutations.handle, Tperm: Int.self))
  }

  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @_inlineable @inline(__always)
  func transposed(withPermutations permutations: [Int]) -> Self {
    return transposed(withPermutations: Tensor<Int>(permutations))
  }

  /// Returns a transposed tensor, with dimensions permuted in the specified
  /// order.
  @_inlineable @inline(__always)
  func transposed(withPermutations permutations: Int...) -> Self {
    return transposed(withPermutations: permutations)
  }

  /// Returns a transposed tensor, with dimensions permuted in reverse order.
  @_inlineable @inline(__always)
  func transposed() -> Self {
    let defaultPermutations = (rankTensor - 1) - Tensor<Int32>(
      rangeFrom: Tensor<Int32>(handle: _TFMakeScalarTensor(0)),
      to: rankTensor,
      stride: Tensor<Int32>(handle: _TFMakeScalarTensor(1))
    )
    return transposed(withPermutations: Tensor<Int>(defaultPermutations))
  }
}

//===----------------------------------------------------------------------===//
// Elementwise basic math functions
//===----------------------------------------------------------------------===//

@_inlineable @inline(__always)
public func abs<Scalar: Numeric, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Abs", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointLog(_:primal:seed:))
public func log<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Log", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointSin(_:primal:seed:))
public func sin<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Sin", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointCos(_:primal:seed:))
public func cos<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Cos", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointTan(_:primal:seed:))
public func tan<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Tan", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointSinh(_:primal:seed:))
public func sinh<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Sinh", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointCosh(_:primal:seed:))
public func cosh<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Cosh", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointTanh(_:primal:seed:))
public func tanh<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Tanh", "t:t", x.handle))
}

@_inlineable @inline(__always)
public func sqrt<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Sqrt", "t:t", x.handle))
}

@_inlineable @inline(__always)
public func exp<Scalar: FloatingPoint, T : TensorProtocol>(
  _ x: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Exp", "t:t", x.handle))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointPow(_:_:primal:seed:))
public func pow<Scalar : Numeric, T : TensorProtocol>(
  _ lhs: T, _ rhs: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Pow", "tt:t", lhs.handle, rhs.handle))
}

@_inlineable @inline(__always)
public func pow<Scalar : Numeric, T : TensorProtocol>(
  _ lhs: Scalar, _ rhs: T
) -> T where T.Scalar == Scalar {
  return pow(T(handle: _TFMakeScalarTensor(lhs)), rhs)
}

@_inlineable @inline(__always)
public func pow<Scalar : Numeric, T : TensorProtocol>(
  _ lhs: T, _ rhs: Scalar
) -> T where T.Scalar == Scalar {
  return pow(lhs, T(handle: _TFMakeScalarTensor(rhs)))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointMin(_:_:primal:seed:))
public func min<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: T, _ rhs: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Min", "tt:t", lhs.handle, rhs.handle))
}

@_inlineable @inline(__always)
public func min<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: Scalar, _ rhs: T
) -> T where T.Scalar == Scalar {
  return min(T(handle: _TFMakeScalarTensor(lhs)), rhs)
}

@_inlineable @inline(__always)
public func min<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: T, _ rhs: Scalar
) -> T where T.Scalar == Scalar {
  return min(lhs, T(handle: _TFMakeScalarTensor(rhs)))
}

@_inlineable @inline(__always)
// @differentiable(gradient: _adjointMax(_:_:primal:seed:))
public func max<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: T, _ rhs: T
) -> T where T.Scalar == Scalar {
  return T(handle: #tfop("Max", "tt:t", lhs.handle, rhs.handle))
}

@_inlineable @inline(__always)
public func max<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: Scalar, _ rhs: T
) -> T where T.Scalar == Scalar {
  return max(T(handle: _TFMakeScalarTensor(lhs)), rhs)
}

@_inlineable @inline(__always)
public func max<Scalar : Numeric & Comparable, T : TensorProtocol>(
  _ lhs: T, _ rhs: Scalar
) -> T where T.Scalar == Scalar {
  return max(lhs, T(handle: _TFMakeScalarTensor(rhs)))
}

public extension TensorProtocol {
  @_inlineable @inline(__always)
  func squared() -> Self {
    return Self(handle: #tfop("Square", "t:t", handle))
  }
}

//===----------------------------------------------------------------------===//
// Selection
//===----------------------------------------------------------------------===//

public extension TensorProtocol where Scalar == Bool {
  @_inlineable @inline(__always)
  public func selecting<U, T : TensorProtocol>(_ left: T, _ right: T) -> T
    where T.Scalar == U {
    return T(handle:
      #tfop("Select", "ttt:t", handle, left.handle, right.handle))
  }

  @_inlineable @inline(__always)
  public func selecting<U, T : TensorProtocol>(_ left: U, _ right: T) -> T
  where T.Scalar == U {
    return T(handle:
      #tfop("Select", "ttt:t", handle, _TFMakeScalarTensor(left), right.handle))
  }

  @_inlineable @inline(__always)
  public func selecting<U, T : TensorProtocol>(_ left: T, _ right: U) -> T
    where T.Scalar == U {
    return T(handle:
      #tfop("Select", "ttt:t", handle, left.handle, _TFMakeScalarTensor(right)))
  }
}

//===----------------------------------------------------------------------===//
// Reduction
//===----------------------------------------------------------------------===//

public extension TensorProtocol {
  @_inlineable @inline(__always)
  func mean() -> Scalar {
    return _TFGetScalarOrDie(#tfop("Mean", "tt:t", handle,
                                   Tensor<Int>([] as [Int]).handle))
  }

  @_inlineable @inline(__always)
  func min() -> Scalar {
    return _TFGetScalarOrDie(#tfop("Min", "tt:t", handle,
                                   Tensor<Int>([] as [Int]).handle))
  }

  @_inlineable @inline(__always)
  func max() -> Scalar {
    return _TFGetScalarOrDie(#tfop("Max", "tt:t", handle,
                                   Tensor<Int>([] as [Int]).handle))
  }

  @_inlineable @inline(__always)
  func sum() -> Scalar {
    return _TFGetScalarOrDie(#tfop("Sum", "tt:t", handle,
                                   Tensor<Int>([] as [Int]).handle))
  }

  @inline(never) // make @_inlineable when implemented.
  func argmax() -> Int {
    fatalError("FIXME: implement argmax")
  }

  @inline(never) // make @_inlineable when implemented.
  func argmin() -> Int {
    fatalError("FIXME: implement argmin")
  }
}

public extension Tensor {
  @_inlineable @inline(__always)
  func mean(
    alongAxes axes: [Int],
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Scalar>(handle:
      #tfop("Mean", "tt:t", handle, Tensor<Int>(axes).handle,
            keep_dims: keepingDimensions, Tidx: Int.self))
  }

  @_inlineable @inline(__always)
  func min(
    alongAxes axes: [Int],
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Scalar>(handle:
      #tfop("Min", "tt:t", handle, Tensor<Int>(axes).handle,
            keep_dims: keepingDimensions, Tidx: Int.self))
  }

  @_inlineable @inline(__always)
  func max(
    alongAxes axes: [Int],
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Scalar>(handle:
      #tfop("Max", "tt:t", handle, Tensor<Int>(axes).handle,
            keep_dims: keepingDimensions, Tidx: Int.self))
  }

  @_inlineable @inline(__always)
  func sum(
    alongAxes axes: [Int],
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Scalar>(handle:
      #tfop("Sum", "tt:t", handle, Tensor<Int>(axes).handle,
            keep_dims: keepingDimensions, Tidx: Int.self))
  }
}

//===----------------------------------------------------------------------===//
// Tensor properties
//===----------------------------------------------------------------------===//

public extension TensorProtocol {
  @_inlineable
  var shapeTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return Tensor<Int32>(handle: #tfop("Shape", "t:t", handle))
    }
  }

  @_inlineable
  var rankTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return Tensor<Int32>(handle: #tfop("Rank", "t:t", handle))
    }
  }

  @_inlineable
  var scalarCountTensor: Tensor<Int32> {
    @inline(__always)
    get {
      return Tensor<Int32>(handle: #tfop("Size", "t:t", handle))
    }
  }
}

//===----------------------------------------------------------------------===//
// Indexing and slicing
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Returns the number of elements in a Tensor (equivalent to the first
  /// dimension).
  /// - Note: `count` is distinct from `scalarCount`, which represents the total
  ///   number of scalars.
  @_inlineable
  var count: Int {
    @inline(__always)
    get {
      return Int(shape.dimensions.first ?? 0)
    }
  }

  @_inlineable
  var indices: CountableRange<Int> {
    @inline(__always)
    get {
      return 0..<count
    }
  }

  @_inlineable
  var startIndex: Int {
    @inline(__always)
    get {
      return 0
    }
  }

  @_inlineable
  var endIndex: Int {
    @inline(__always)
    get {
      return count
    }
  }

  /// Access the element tensor specified by an index in the leading dimension.
  /// - Parameter index: index of the element tensor
  subscript(index: Int) -> Tensor {
    get {
      fatalError("FIXME: implement subscript to tensor")
    }
    set {
      fatalError("FIXME: implement subscript to tensor")
    }
  }

  /// Returns the subdimensional tensor at the specified list of indices.
  /// - Parameter indices: list of indices
  /// - TODO: If possible, this should be defined as an op, to be run on the
  /// accelerator.
  subscript(indices: Int...) -> Tensor {
    get {
      fatalError("FIXME: implement subscript to tensor")
    }
    set {
      fatalError("FIXME: implement subscript to tensor")
    }
  }

  /// Returns the subtensor defined by the specified bounds.
  /// - Parameter bounds: contiguous range of indices
  // TODO: begin/end are vectors in general.
  // tfop_slice(tensor, begin, end) -> tensor
  subscript(bounds: Range<Int>) -> Tensor {
    get {
      fatalError("FIXME: implement subscript to tensor")
    }
    set {
      fatalError("FIXME: implement subscript to tensor")
    }
  }
}

//===----------------------------------------------------------------------===//
// Convolution and pooling
//===----------------------------------------------------------------------===//

public enum Padding {
  case same, valid
}

internal extension Padding {
  @_inlineable
  @_versioned
  var cName: String {
    switch self {
    case .same: return "SAME"
    case .valid: return "VALID"
    }
  }
}

public extension Tensor where Scalar : FloatingPoint {
  @_inlineable @inline(__always)
  // @differentiable(
  //   withRespectTo: (self, .0),
  //   gradient: _adjointConvolve2D(input:filter:primal:seed:)
  // )
  func convolved2D(
    withFilter filter: Tensor,
    strides: [Int],
    padding: Padding
  ) -> Tensor {
    return Tensor(handle:
      #tfop("Conv2D", "tt:t", handle, filter.handle, strides: strides,
            padding: padding.cName))
  }

  @_inlineable @inline(__always)
  // @differentiable(
  //   withRespectTo: (self),
  //   gradient:
  //     _adjointMaxPooled2D(input:kernelSize:strides:padding:primal:seed:)
  // )
  func maxPooled(
    kernelSize: Tensor<Int>,
    strides: Tensor<Int>,
    padding: Padding
  ) -> Tensor {
    return Tensor(handle:
      #tfop("MaxPoolV2", "ttt:t", handle, Tensor<Int32>(kernelSize).handle,
            Tensor<Int32>(strides).handle, padding: padding.cName))
  }

  @_inlineable @inline(__always)
  func maxPooled(
    kernelSize: [Int],
    strides: [Int],
    padding: Padding
  ) -> Tensor {
    return maxPooled(kernelSize: Tensor<Int>(kernelSize),
                     strides: Tensor<Int>(strides),
                     padding: padding)
  }

  @_inlineable @inline(__always)
  // @differentiable(
  //   withRespectTo: (self),
  //   gradient:
  //     _adjointAveragePooled2D(input:kernelSize:strides:padding:primal:seed:)
  // )
  func averagePooled(
    kernelSize: [Int],
    strides: [Int],
    padding: Padding
  ) -> Tensor {
    // FIXME: handle attributes (ksize, strides, padding)
    return Tensor(handle:
      #tfop("AvgPool", "t:t", handle, ksize: kernelSize, strides: strides,
            padding: padding.cName))
  }
}
