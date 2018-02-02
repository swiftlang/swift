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
// This file contains core Tensor op definitions.
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
// - Unify Tensor and RankedTensor ops with protocol mechanism.
// - Consider explicit broadcasting for elementwise binary ops when
//   scalarization and rank getter are implemented.
//

//===----------------------------------------------------------------------===//
// Elementwise binary arithmetics
//===----------------------------------------------------------------------===//

extension Tensor /*: Numeric*/ where Unit : Numeric {
  @_inlineable
  // @differentiable(gradient: _adjointAdd(_:_:primal:seed:))
  public static func +(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(#tfop("Add", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  // @differentiable(gradient: _adjointSubtract(_:_:primal:seed:))
  public static func -(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(#tfop("Sub", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  // @differentiable(gradient: _adjointMultiply(_:_:primal:seed:))
  public static func *(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(#tfop("Mul", "tt:t", lhs.handle, rhs.handle))
  }
}

public extension Tensor where Unit : Numeric {
  @_inlineable
  // @differentiable(gradient: _adjointAdd(_:_:primal:seed:))
  static func +(lhs: Tensor, rhs: Unit) -> Tensor {
    return lhs + Tensor(rhs)
  }

  @_inlineable
  // @differentiable(gradient: _adjointAdd(_:_:primal:seed:))
  static func +(lhs: Unit, rhs: Tensor) -> Tensor {
    return Tensor(lhs) + rhs
  }

  @_inlineable
  static func +=(lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs + rhs
  }

  @_inlineable
  static func +=(lhs: inout Tensor, rhs: Unit) {
    lhs = lhs + Tensor(rhs)
  }

  @_inlineable
  // @differentiable(gradient: _adjointNegate(_:primal:seed:))
  static prefix func -(rhs: Tensor) -> Tensor {
    return Tensor(#tfop("Neg", "t:t", rhs.handle))
  }

  @_inlineable
  static func -(lhs: Tensor, rhs: Unit) -> Tensor {
    return lhs - Tensor(rhs)
  }

  @_inlineable
  static func -(lhs: Unit, rhs: Tensor) -> Tensor {
    return Tensor(lhs) - rhs
  }

  @_inlineable
  static func -=(lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs - rhs
  }

  @_inlineable
  static func -=(lhs: inout Tensor, rhs: Unit) {
    lhs = lhs - rhs
  }

  @_inlineable
  static func *(lhs: Unit, rhs: Tensor) -> Tensor {
    return Tensor(lhs) * rhs
  }

  @_inlineable
  static func *(lhs: Tensor, rhs: Unit) -> Tensor {
    return lhs * Tensor(rhs)
  }

  @_inlineable
  static func /(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(#tfop("Div", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  static func /(lhs: Tensor, rhs: Unit) -> Tensor {
    return lhs / Tensor(rhs)
  }

  @_inlineable
  static func /(lhs: Unit, rhs: Tensor) -> Tensor {
    return Tensor(lhs) / rhs
  }

  @_inlineable
  static func /=(lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs / rhs
  }

  @_inlineable
  static func /=(lhs: inout Tensor, rhs: Unit) {
    lhs = lhs / rhs
  }

   @_inlineable
  static func %(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(#tfop("Mod", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  static func %(lhs: Tensor, rhs: Unit) -> Tensor {
    return lhs % Tensor(rhs)
  }

  @_inlineable
  static func %(lhs: Unit, rhs: Tensor) -> Tensor {
    return Tensor(lhs) % rhs
  }

  @_inlineable
  static func %=(lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs % rhs
  }

  @_inlineable
  static func %=(lhs: inout Tensor, rhs: Unit) {
    lhs = lhs % rhs
  }
}

public extension Tensor {
  @_inlineable
  func mean() -> Unit {
    let result = Tensor<Unit>(#tfop("Mean", "tt:t", handle,
                                    Tensor<Int>(emptyWithRank: 1).handle))
    return result.scalar!
  }

  @_inlineable
  func mean(
    alongAxes axes: Int...,
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Unit>(#tfop("Mean", "tt:t", handle, Tensor<Int>(axes).handle,
                              keep_dims: keepingDimensions, Tidx: Int.self))
  }

  @_inlineable
  func min() -> Unit {
    let result = Tensor<Unit>(#tfop("Min", "tt:t", handle,
                                    Tensor<Int>(emptyWithRank: 1).handle))
    return result.scalar!
  }

  @_inlineable
  func min(
    alongAxes axes: Int...,
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Unit>(#tfop("Min", "tt:t", handle, Tensor<Int>(axes).handle,
                              keep_dims: keepingDimensions, Tidx: Int.self))
  }

  @_inlineable
  func max() -> Unit {
    let result = Tensor<Unit>(#tfop("Max", "tt:t", handle,
                                    Tensor<Int>(emptyWithRank: 1).handle))
    return result.scalar!
  }

  @_inlineable
  func max(
    alongAxes axes: Int...,
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Unit>(#tfop("Max", "tt:t", handle, Tensor<Int>(axes).handle,
                              keep_dims: keepingDimensions, Tidx: Int.self))
  }

  @_inlineable
  func sum() -> Unit {
    let result = Tensor<Unit>(#tfop("Sum", "tt:t", handle,
                                    Tensor<Int>(emptyWithRank: 1).handle))
    return result.scalar!
  }

  @_inlineable
  func sum(
    alongAxes axes: Int...,
    keepingDimensions: Bool = false
  ) -> Tensor {
    return Tensor<Unit>(#tfop("Sum", "tt:t", handle, Tensor<Int>(axes).handle,
                              keep_dims: keepingDimensions, Tidx: Int.self))
  }

  @inline(never) // make @_inlineable when implemented.
  func argmax() -> Int {
    fatalError("FIXME: implement argmax")
  }

  @inline(never) // make @_inlineable when implemented.
  func argmin() -> Int {
    fatalError("FIXME: implement argmin")
  }

  @_inlineable
  func squared() -> Tensor {
    return Tensor(#tfop("Square", "t:t", handle))
  }
}

//===----------------------------------------------------------------------===//
// Linear algebra
//===----------------------------------------------------------------------===//

public extension Tensor where Unit : Numeric {
  @_inlineable
  func dot(_ other: Tensor) -> Tensor {
    return Tensor(#tfop("MatMul", "tt:t", self.handle, other.handle))
  }

  @_inlineable
  static func ⊗ (lhs: Tensor, rhs: Tensor) -> Tensor {
    return lhs.dot(rhs)
  }

  @_inlineable
  static func ⊗ (lhs: Unit, rhs: Tensor) -> Tensor {
    return Tensor(lhs) ⊗ rhs
  }

  @_inlineable
  static func ⊗ (lhs: Tensor, rhs: Unit) -> Tensor {
    return lhs ⊗ Tensor(rhs)
  }
}

public extension Tensor1D where Unit : Numeric {
  @_inlineable
  func dot(_ other: Tensor2D<Unit>) -> Tensor1D<Unit> {
    return Tensor1D(underlying: underlyingTensor.dot(other.underlyingTensor))
  }
}

public extension Tensor2D where Unit : Numeric {
  @_inlineable
  static func ⊗ (
    lhs: Tensor1D<Unit>, rhs: Tensor2D<Unit>
  ) -> Tensor1D<Unit> {
    return lhs.dot(rhs)
  }
}

//===----------------------------------------------------------------------===//
// Elementwise binary comparison
//===----------------------------------------------------------------------===//

public extension Tensor where Unit : Comparable {
  @_inlineable
  static func < (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Tensor<Bool>(#tfop("Less", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  static func < (lhs: Tensor, rhs: Unit) -> Tensor<Bool> {
    return lhs < Tensor(rhs)
  }

  @_inlineable
  static func < (lhs: Unit, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) < rhs
  }

  @_inlineable
  static func <= (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Tensor<Bool>(#tfop("LessEqual", "tt:t",
                        lhs.handle, rhs.handle))
  }

  @_inlineable
  static func <= (lhs: Tensor, rhs: Unit) -> Tensor<Bool> {
    return lhs <= Tensor(rhs)
  }

  @_inlineable
  static func <= (lhs: Unit, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) <= rhs
  }

  @_inlineable
  static func > (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Tensor<Bool>(#tfop("Greater", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  static func > (lhs: Tensor, rhs: Unit) -> Tensor<Bool> {
    return lhs > Tensor(rhs)
  }

  @_inlineable
  static func > (lhs: Unit, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) > rhs
  }

  @_inlineable
  static func >= (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Tensor<Bool>(#tfop("GreaterEqual", "tt:t",
                       lhs.handle, rhs.handle))
  }

  @_inlineable
  static func >= (lhs: Tensor, rhs: Unit) -> Tensor<Bool> {
    return lhs >= Tensor(rhs)
  }

  @_inlineable
  static func >= (lhs: Unit, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) >= rhs
  }
}

public extension Tensor where Unit : Equatable {
  @_inlineable
  static func == (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Tensor<Bool>(#tfop("Equal", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  static func == (lhs: Tensor, rhs: Unit) -> Tensor<Bool> {
    return lhs == Tensor(rhs)
  }

  @_inlineable
  static func == (lhs: Unit, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) == rhs
  }

  @_inlineable
  static func != (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Tensor<Bool>(#tfop("NotEqual", "tt:t", lhs.handle, rhs.handle))
  }

  @_inlineable
  static func != (lhs: Tensor, rhs: Unit) -> Tensor<Bool> {
    return lhs != Tensor(rhs)
  }

  @_inlineable
  static func != (lhs: Unit, rhs: Tensor) -> Tensor<Bool> {
    return Tensor(lhs) != rhs
  }
}

//===----------------------------------------------------------------------===//
// Transposition and concatenation
//===----------------------------------------------------------------------===//

public extension Tensor {
  @_inlineable
  var transpose: Tensor {
    return Tensor(#tfop("Transpose", "t:t", handle))
  }

  @inline(never) // make @_inlineable when implemented.
  func concatenated(with other: Tensor) -> Tensor {
    fatalError("FIXME: implement concatenated(with:)")
  }
}

//===----------------------------------------------------------------------===//
// Elementwise basic math functions
//===----------------------------------------------------------------------===//

@_inlineable
public func abs<Unit: Numeric>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Abs", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointLog(_:primal:seed:))
public func log<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Log", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointSin(_:primal:seed:))
public func sin<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Sin", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointCos(_:primal:seed:))
public func cos<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Cos", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointTan(_:primal:seed:))
public func tan<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Tan", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointSinh(_:primal:seed:))
public func sinh<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Sinh", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointCosh(_:primal:seed:))
public func cosh<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Cosh", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointTanh(_:primal:seed:))
public func tanh<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Tanh", "t:t", x.handle))
}

@_inlineable
public func exp<Unit: FloatingPoint>(
  _ x: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Exp", "t:t", x.handle))
}

@_inlineable
// @differentiable(gradient: _adjointPow(_:_:primal:seed:))
public func pow<Unit : Numeric>(
  _ lhs: Tensor<Unit>, _ rhs: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Pow", "tt:t", lhs.handle, rhs.handle))
}

@_inlineable
public func pow<Unit : Numeric>(
  _ lhs: Unit, _ rhs: Tensor<Unit>
) -> Tensor<Unit> {
  return pow(Tensor(lhs), rhs)
}

@_inlineable
public func pow<Unit : Numeric>(
  _ lhs: Tensor<Unit>, _ rhs: Unit
) -> Tensor<Unit> {
  return pow(lhs, Tensor(rhs))
}

@_inlineable
// @differentiable(gradient: _adjointMin(_:_:primal:seed:))
public func min<Unit : Numeric & Comparable>(
  _ lhs: Tensor<Unit>, _ rhs: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Min", "tt:t", lhs.handle, rhs.handle))
}

@_inlineable
public func min<Unit : Numeric & Comparable>(
  _ lhs: Unit, _ rhs: Tensor<Unit>
) -> Tensor<Unit> {
  return min(Tensor(lhs), rhs)
}

@_inlineable
public func min<Unit : Numeric & Comparable>(
  _ lhs: Tensor<Unit>, _ rhs: Unit
) -> Tensor<Unit> {
  return min(lhs, Tensor(rhs))
}

@_inlineable
// @differentiable(gradient: _adjointMax(_:_:primal:seed:))
public func max<Unit : Numeric & Comparable>(
  _ lhs: Tensor<Unit>, _ rhs: Tensor<Unit>
) -> Tensor<Unit> {
  return Tensor(#tfop("Max", "tt:t", lhs.handle, rhs.handle))
}

@_inlineable
public func max<Unit : Numeric & Comparable>(
  _ lhs: Unit, _ rhs: Tensor<Unit>
) -> Tensor<Unit> {
  return max(Tensor(lhs), rhs)
}

@_inlineable
public func max<Unit : Numeric & Comparable>(
  _ lhs: Tensor<Unit>, _ rhs: Unit
) -> Tensor<Unit> {
  return max(lhs, Tensor(rhs))
}

//===----------------------------------------------------------------------===//
// Selection
//===----------------------------------------------------------------------===//

public extension Tensor where Unit == Bool {
  @_inlineable
  public func selecting<T>(_ left: Tensor<T>, _ right: Tensor<T>) -> Tensor<T> {
    return Tensor<T>(#tfop("Select", "ttt:t", handle, left.handle, right.handle))
  }

  @_inlineable
  public func selecting<T>(_ left: T, _ right: Tensor<T>) -> Tensor<T> {
    return selecting(Tensor<T>(left), right)
  }

  @_inlineable
  public func selecting<T>(_ left: Tensor<T>, _ right: T) -> Tensor<T> {
    return selecting(left, Tensor<T>(right))
  }
}

//===----------------------------------------------------------------------===//
// Reduction
//===----------------------------------------------------------------------===//

public extension Tensor2D where Unit : Numeric {
  // Sum tensor along one axis, producing a Tensor1D.
  @_inlineable
  func sum(alongAxis axis: Int) -> Tensor1D<Unit> {
    return Tensor1D<Unit>(underlying:
      underlyingTensor.sum(alongAxes: axis))
  }

  @_inlineable
  func max(alongAxis axis: Int) -> Tensor1D<Unit> {
    return Tensor1D<Unit>(underlying:
      underlyingTensor.max(alongAxes: axis))
  }

  @_inlineable
  func min(alongAxis axis: Int) -> Tensor1D<Unit> {
    return Tensor1D<Unit>(underlying:
      underlyingTensor.min(alongAxes: axis))
  }

  @_inlineable
  func mean(alongAxis axis: Int) -> Tensor1D<Unit> {
    return Tensor1D<Unit>(underlying:
      underlyingTensor.mean(alongAxes: axis))
  }
}

//===----------------------------------------------------------------------===//
// Tensor properties
//===----------------------------------------------------------------------===//

/// Internal getters that return Int32 tensors.
internal extension Tensor {
  @_versioned
  @_inlineable
  var shapeTensorOriginal: Tensor<Int32> {
    @inline(__always)
    get {
      return Tensor<Int32>(#tfop("Shape", "t:t", handle))
    }
  }

  @_versioned
  @_inlineable
  var rankTensorOriginal: Tensor<Int32> {
    @inline(__always)
    get {
      return Tensor<Int32>(#tfop("Rank", "t:t", handle))
    }
  }

  @_versioned
  @_inlineable
  var unitCountTensorOriginal: Tensor<Int32> {
    @inline(__always)
    get {
      return Tensor<Int32>(#tfop("Size", "t:t", handle))
    }
  }
}

public extension Tensor {
  @_inlineable
  var shapeTensor: Tensor<Int> {
    @inline(__always)
    get {
      return Tensor<Int>(shapeTensorOriginal)
    }
  }

  @_inlineable
  var rankTensor: Tensor<Int> {
    @inline(__always)
    get {
      return Tensor<Int>(rankTensorOriginal)
    }
  }

  @_inlineable
  var unitCountTensor: Tensor<Int> {
    @inline(__always)
    get {
      return Tensor<Int>(unitCountTensorOriginal)
    }
  }
}

//===----------------------------------------------------------------------===//
// Slicing and shape transformations
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Returns a subdimensional tensor at the specified list of indices.
  /// - Todo: If possible, this should be defined as an op, to be run on the
  /// accelerator.
  subscript(indices: Int...) -> Tensor {
    fatalError("FIXME: implement subscript to tensor")
  }

  // Slicing out a range of subdimensional tensors.
  // TODO: begin/end are vectors in general.
  // tfop_slice(tensor, begin, end) -> tensor
  subscript(bounds: Range<Int>) -> Tensor {
    fatalError("FIXME: implement subscript to tensor")
  }

  @_inlineable
  @inline(__always)
  func reshaped(_ newShape: Tensor<Int>) -> Tensor {
    return Tensor(#tfop("Reshape", "tt:t", handle, newShape.handle))
  }

  @_inlineable
  func squeezed(alongAxes axes: Tensor<Int>? = nil) -> Tensor {
    // FIXME: handle attributes (squeeze_dims)
    return Tensor(#tfop("Squeeze", "t:t", handle))
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

public extension Tensor where Unit : FloatingPoint {
  @_inlineable
  // @differentiable(
  //   withRespectTo: (self, .0),
  //   gradient: _adjointConvolve2D(input:filter:primal:seed:)
  // )
  func convolved2D(
    withFilter filter: Tensor,
    strides: [Int],
    padding: Padding
  ) -> Tensor {
    return Tensor(#tfop("Conv2D", "tt:t", handle, filter.handle,
                        strides: strides, padding: padding.cName))
  }

  @_inlineable
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
    return Tensor(#tfop("MaxPoolV2", "ttt:t", handle,
                  Tensor<Int32>(kernelSize).handle,
                  Tensor<Int32>(strides).handle, padding: padding.cName))
  }

  @_inlineable
  func maxPooled(
    kernelSize: [Int],
    strides: [Int],
    padding: Padding
  ) -> Tensor {
    return maxPooled(kernelSize: Tensor<Int>(kernelSize),
                     strides: Tensor<Int>(strides),
                     padding: padding)
  }

  @_inlineable
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
    return Tensor(#tfop("AvgPool", "t:t", handle,
                        ksize: kernelSize, strides: strides,
                        padding: padding.cName))
  }
}
