//===-- Tensor.swift ------------------------------------------*- swift -*-===//
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
// This is the core Tensor abstraction, which is conceptually equivalent to a
// NumPy ndarray.  It carries no rank information in its static type, so it can
// be used by model developers who don't want it.
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif
import CTensorFlow

//===----------------------------------------------------------------------===//
// Tensor
//===----------------------------------------------------------------------===//

/// `Tensor` is a multi-dimensional array used for computation. It is a wrapper
/// around a `TensorHandle`.
@_fixed_layout
public struct Tensor<Scalar : TensorFlowScalar> : TensorProtocol {
  /// The underlying `TensorHandle`.
  /// - Note: `handle` is public to allow user defined ops, but should not
  /// normally be used otherwise.
  public let handle: TensorHandle<Scalar>

  @inlinable
  public init(handle: TensorHandle<Scalar>) {
    self.handle = handle
  }
}

//===----------------------------------------------------------------------===//
// Compiler Intrinsics
//===----------------------------------------------------------------------===//
//
// By default, when a `Tensor` value is implicitly passed between host and
// tensor code, the partitioning pass will generate a warning. Users can
// indicate that they are doing something intentionally by using these methods,
// which silences the warning.
//
// TODO: These would be nicer defined as builtins rather than "well known
// functions".

@usableFromInline @inline(never)
@_silgen_name("__tf_to_accel")
func _TFToAccelerator<Scalar>(_ handle: TensorHandle<Scalar>) -> TensorHandle<Scalar> {
  return handle
}

@usableFromInline @inline(never)
@_silgen_name("__tf_to_host")
func _TFToHost<Scalar>(_ handle: TensorHandle<Scalar>)
  -> TensorHandle<Scalar> {
  return handle
}

/// This function converts a `TensorHandle` that is known to have a 0-d value
/// into the scalar that it produces. This is intended for use in op definitions
/// where it is known that the op always returns a 0-d tensor. It is not for use
/// in general code.
@inlinable @inline(__always)
func _TFGetScalarOrDie<Scalar : TensorFlowScalar>(
  _ handle: TensorHandle<Scalar>
) -> Scalar {
  return Scalar._getScalarOrDie(handle)
}

/// This function converts a `TensorHandle` into a scalar if it is 0-d, or
/// returns nil otherwise.
@inlinable @inline(__always)
func _TFGetScalar<Scalar : TensorFlowScalar>(
  _ handle: TensorHandle<Scalar>
) -> Scalar? {
  return Scalar._getScalar(handle)
}

/// This compiler builtin is known by the partitioning pass, which recognizes it
/// and promotes calls to it to being in graph when it can. This signature was
/// designed to align with the requirements of the `Const` TensorFlow operation.
@usableFromInline @inline(never)
@_silgen_name("__tf_tensor_from_scalars")
func _TFTensorFromScalars<Scalar : TensorFlowScalar>(
  _ scalars: [Scalar], shape: [Int]
) -> TensorHandle<Scalar> {
  let contiguousSize = shape.reduce(1, *)
  precondition(scalars.count == contiguousSize,
               "The number of scalars does not match the shape.")
  return TensorHandle(
    shape: shape,
    scalarsInitializer: { addr in
      scalars.withUnsafeBufferPointer { ptr in
        addr.assign(from: ptr.baseAddress!, count: contiguousSize)
      }
    }
  )
}

/// In graph mode, the deabstraction pass transforms this function call to
/// either a "Const" graph_op (if `scalar` is a compile-time constant), or a
/// "tfc.scalarToTensor" graph_op. In the latter case, the partition pass uses
/// it to do scalar promotion, and transforms it away before entering graph
/// lowering. e.g. For user code:
///   let x_scalar = x_tensor.mean()
///   let y_scalar = y_tensor.mean()
///   let z_scalar = x_scalar + y_scalar
///   let z_tensor = Tensor(z_scalar)
///
/// The scalar addition can be promoted into graph, through the
/// "tfc.scalarToTensor" graph_op generated from Tensor(z_scalar). In this
/// example, the _getScalarOrDie() call generated from mean() will be "cancelled
/// out" with "tfc.scalarToTensor", such that we avoid generating scalar on the
/// host, and then converting it back to a graph tensor.
///
/// In eager mode, this function is executed directly.
@usableFromInline @inline(never)
@_silgen_name("__tf_tensor_from_scalar")
func _TFTensorFromScalar<Scalar : TensorFlowScalar>(
  _ scalar: Scalar
) -> TensorHandle<Scalar> {
  return _TFTensorFromScalars([scalar], shape: [])
}

@usableFromInline @inline(never)
@_silgen_name("__tf_tensor_from_scalars_1d")
func _TFTensorFromScalars1D<Scalar : TensorFlowScalar>(_ scalars: [Scalar])
  -> TensorHandle<Scalar> {
  return _TFTensorFromScalars(scalars, shape: [scalars.count])
}

@inlinable @inline(__always)
func _TFHoistable<Scalar>(_ fn: () -> TensorHandle<Scalar>)
  -> TensorHandle<Scalar> {
  return Scalar._hoistableClosure(fn)
}

//===----------------------------------------------------------------------===//
// Memory Transfer Markers
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Mark memory transfer to accelerator.
  /// - Parameters:
  ///   - shape: When sending the tensor to a TF XLA device (including TPU),
  ///   must specify the tensor shape as required by XLA compilation.
  @inlinable @inline(__always)
  func toAccelerator(shape: TensorShape) -> Tensor {
    let tensor = toAccelerator()
    // If the tensor is to be sent from host to TPU, the shape is specified on
    // TF CPU first, before TF CPU sends the tensor to TPU.
    let ret: TensorHandle<Scalar> = #tfop(
      "Identity",
      tensor,
      T$dtype: Scalar.tensorFlowDataType,
      __shapes: [shape],
      __device: "/job:localhost/replica:0/task:0/device:CPU:0")
    return Tensor(handle: ret)
  }

  /// Mark memory transfer to accelerator.
  @inlinable @inline(__always)
  func toAccelerator() -> Tensor {
    return Tensor(handle: _TFToAccelerator(handle))
  }

  /// Mark memory transfer to host.
  /// - Parameters:
  ///   - shape: When sending the tensor to a TF XLA device (including TPU),
  ///   must specify the tensor shape as required by XLA compilation.
  @inlinable @inline(__always)
  func toHost(shape: TensorShape) -> Tensor {
    // If the `self` tensor resides on TPU, the shape is specified on that
    // device first, before outfeeding the tensor to CPU, a required step for
    // sending the tensor to the host.
    let tensor: TensorHandle<Scalar> =
      #tfop("Identity", self, T$dtype: Scalar.tensorFlowDataType,
            __shapes: [shape])
    return Tensor(handle: tensor).toHost()
  }

  /// Mark memory transfer to host.
  @inlinable @inline(__always)
  func toHost() -> Tensor {
    return Tensor(handle: _TFToHost(handle))
  }
}

//===----------------------------------------------------------------------===//
// Initialization
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Creates a tensor from a scalar value.
  @inlinable @inline(__always)
  @differentiable(vjp: _vjpScalarInit where Scalar : TensorFlowFloatingPoint)
  init(_ value: Scalar) {
    self.init(handle: _TFTensorFromScalar(value))
  }
}

internal extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpScalarInit(_ value: Scalar) -> (Tensor, (Tensor) -> Scalar) {
    return (Tensor(value), { $0.scalarized() })
  }
}

public extension Tensor {
  /// Creates a 1D tensor from contiguous scalars.
  ///
  /// - Parameters:
  ///   - vector: The scalar contents of the tensor.
  ///
  @inlinable @inline(__always)
  init(_ vector: [Scalar]) {
    self.init(handle: _TFTensorFromScalars1D(vector))
  }

  /// Creates a 1D tensor from contiguous scalars.
  ///
  /// - Parameters:
  ///   - vector: The scalar contents of the tensor.
  ///
  @inlinable @inline(__always)
  init<C : RandomAccessCollection>(_ vector: C) where C.Element == Scalar {
    let handle = _TFHoistable {
      TensorHandle<Scalar>(
        shape: [vector.count],
        scalarsInitializer: { addr in
          var currentAddr = addr
          for scalar in vector {
            currentAddr.initialize(to: scalar)
            currentAddr = currentAddr.advanced(by: 1)
          }
        }
      )
    }
    self.init(handle: handle)
  }

  /// Creates a tensor with the specified shape and contiguous scalars in
  /// row-major order.
  ///
  /// - Parameters:
  ///   - shape: The shape of the tensor.
  ///   - scalars: The scalar contents of the tensor.
  /// - Precondition: The number of scalars must equal the product of the
  ///   dimensions of the shape.
  ///
  @inlinable @inline(__always)
  init(shape: TensorShape, scalars: [Scalar]) {
    // NOTE: We use `_TFTensorFromScalars` here so the compiler can try to
    // promote constants and avoid copies.
    self.init(handle: _TFTensorFromScalars(scalars, shape: shape.dimensions))
  }

  /// Creates a tensor with the specified shape and contiguous scalars in
  /// row-major order.
  ///
  /// - Parameters:
  ///   - shape: The shape of the tensor.
  ///   - scalars: The scalar contents of the tensor.
  /// - Precondition: The number of scalars must equal the product of the
  ///   dimensions of the shape.
  ///
  @inlinable @inline(__always)
  init(shape: TensorShape, scalars: UnsafeBufferPointer<Scalar>) {
    let handle: TensorHandle<Scalar> = _TFHoistable {
      precondition(scalars.count == shape.contiguousSize)
      return TensorHandle<Scalar>(
        shape: shape.dimensions,
        scalarsInitializer: { addr in
          addr.initialize(from: scalars.baseAddress!,
                          count: shape.contiguousSize)
        }
      )
    }
    self.init(handle: handle)
  }

  /// Creates a tensor with the specified shape and contiguous scalars in
  /// row-major order.
  ///
  /// - Parameters:
  ///   - shape: The shape of the tensor.
  ///   - scalars: The scalar contents of the tensor.
  /// - Precondition: The number of scalars must equal the product of the
  ///   dimensions of the shape.
  ///
  @inlinable @inline(__always)
  init<C : RandomAccessCollection>(shape: TensorShape, scalars: C)
    where C.Element == Scalar {
    let handle: TensorHandle<Scalar> = _TFHoistable {
      precondition(scalars.count == shape.contiguousSize)
      return TensorHandle<Scalar>(
        shape: shape.dimensions,
        scalarsInitializer: { addr in
          var currentAddr = addr
          for scalar in scalars {
            currentAddr.initialize(to: scalar)
            currentAddr = currentAddr.advanced(by: 1)
          }
        }
      )
    }
    self.init(handle: handle)
  }
}

//===----------------------------------------------------------------------===//
// Initialization Syntax
//===----------------------------------------------------------------------===//

// Background story on `TensorElementLiteral` and why it's necessary:
//
// Very importantly, we want users to be able to implicitly convert an array
// literal to a tensor. At first glance, a straightfoward implementation would
// be conforming `Tensor` to `ExpressibleByArrayLiteral` with
// `ExpressibleBy(Float|Int|Bool)Literal` as a base case. However, it is not
// that simple. We have binary operators that take `(Tensor, Scalar)`, `(Scalar,
// Tensor)` as well as `(Tensor, Tensor)`. When `Tensor`s are convertible from
// both a scalar and an array literal, a scalar-tensor binary operator like `+`
// will not type check.
//
// One way to work around it is to define all tensor-tensor operators in a
// protocol extension, and all tensor-scalar and scalar-tensor operators on
// concrete `Tensor`. Protocol extensions are less favorable than concrete
// implementations, so the compiler will prefer the concrete implementation for
// a scalar-tensor operation. However, this would cause enormous code bloat and
// is entirely a hack.
//
// To resolve ambiguity, `Tensor` should not be expressible by scalar literal.
// There's already a lightweight syntax for converting a scalar to a tensor:
// `Tensor(x)`, so there is no strong need for implicit conversion. But we need
// to find a way to give `ExpressibleByArrayLiteral` a base case: what would the
// `ArrayLiteralElement` be if we want to support both `[1,2,3]` and `[[[1,2],
// [1,2]]]`? In the first case the array literal element is an interger, while
// in the second case the array literal itself should be a tensor. Based on this
// observation, we come up with an intermediate type: `TensorElementLiteral` as
// the `ArrayLiteralElement` of `Tensor`. By making `TensorElementLiteral`
// expressible by both array literal and scalar literal, `Tensor` can now be
// converted from an arbitrary-dimensional array literal.
//
// Due to protocol requirements, `TensorElementLiteral` has to be
// public. It is never supposed to be used directly by any user, so the library
// convention is to prepend an underscore to its name, making it
// `_TensorElementLiteral`.
//
// It would be nice to be able to remove this type when we can systematically
// resolve tensor-scalar/scalar-tensor op ambiguity someday, either through an
// improved `Expressible` model, or by introducing an attribute to tell the type
// checker which function to prefer when ambiguity occurs.

/// Represents a literal element for conversion to a `Tensor`.
///
/// - Note: Do not ever use this API directly. This is implicitly created
///   during the conversion from an array literal to a `Tensor`, and is purely
///   for implementation purposes.
@_fixed_layout
public struct _TensorElementLiteral<Scalar> : TensorProtocol
  where Scalar : TensorFlowScalar {

  @usableFromInline let tensor: Tensor<Scalar>

  @inlinable
  public var handle: TensorHandle<Scalar> {
    return tensor.handle
  }

  @inlinable
  public init(handle: TensorHandle<Scalar>) {
    tensor = Tensor(handle: handle)
  }
}

extension _TensorElementLiteral : ExpressibleByBooleanLiteral
  where Scalar : ExpressibleByBooleanLiteral {
  public typealias BooleanLiteralType = Scalar.BooleanLiteralType
  @inlinable @inline(__always)
  public init(booleanLiteral: BooleanLiteralType) {
    tensor = Tensor(Scalar(booleanLiteral: booleanLiteral))
  }
}

extension _TensorElementLiteral : ExpressibleByIntegerLiteral
  where Scalar : ExpressibleByIntegerLiteral {
  public typealias IntegerLiteralType = Scalar.IntegerLiteralType
  @inlinable @inline(__always)
  public init(integerLiteral: IntegerLiteralType) {
    tensor = Tensor(Scalar(integerLiteral: integerLiteral))
  }
}

extension _TensorElementLiteral : ExpressibleByFloatLiteral
  where Scalar : ExpressibleByFloatLiteral {
  public typealias FloatLiteralType = Scalar.FloatLiteralType
  @inlinable @inline(__always)
  public init(floatLiteral: FloatLiteralType) {
    tensor = Tensor(Scalar(floatLiteral: floatLiteral))
  }
}

extension _TensorElementLiteral : ExpressibleByArrayLiteral {
  public typealias ArrayLiteralElement = _TensorElementLiteral<Scalar>
  @inlinable @inline(__always)
  public init(arrayLiteral elements: _TensorElementLiteral<Scalar>...) {
    // FIXME: We cannot use Raw.pack here because _TensorElementLiteral does not
    // conform to tensor group and if we do, several partitioning tests fail.
    tensor = Tensor(handle: #tfop(
      "Pack", elements, T$dtype: Scalar.tensorFlowDataType))
  }
}

extension Tensor : ExpressibleByArrayLiteral {
  /// The type of the elements of an array literal.
  public typealias ArrayLiteralElement = _TensorElementLiteral<Scalar>

  /// Creates a tensor initialized with the given elements.
  /// - Note: This is for conversion from tensor element literals. This is a
  /// separate method because `ShapedArray` initializers need to call it.
  @inlinable @inline(__always)
  internal init(
    _tensorElementLiterals elements: [_TensorElementLiteral<Scalar>]
  ) {
    // FIXME: We cannot use Raw.pack here because _TensorElementLiteral does not
    // conform to tensor group and if we do, several partitioning tests fail.
    self.init(handle: #tfop(
      "Pack", elements, T$dtype: Scalar.tensorFlowDataType))
  }

  /// Creates a tensor initialized with the given elements.
  @inlinable @inline(__always)
  public init(arrayLiteral elements: _TensorElementLiteral<Scalar>...) {
    self.init(_tensorElementLiterals: elements)
  }
}

//===----------------------------------------------------------------------===//
// Scalar Conversion
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Returns `true` if `rank` is equal to 0 and `false` otherwise.
  @inlinable
  var isScalar: Bool {
    @inline(__always)
    get {
      return rank == 0
    }
  }

  /// Returns the single scalar element if `rank` is equal to 0 and `nil`
  /// otherwise.
  @inlinable
  var scalar: Scalar? {
    @inline(__always)
    get {
      return Scalar(self)
    }
  }

  /// Reshape to scalar.
  /// - Precondition: The tensor has exactly one scalar.
  @inlinable
  @differentiable(
    wrt: self,
    vjp: _vjpScalarized where Scalar : TensorFlowFloatingPoint)
  func scalarized() -> Scalar {
    return _TFGetScalarOrDie(Raw.reshape(
      self, shape: Tensor<Int32>(handle: _TFTensorFromScalars1D([]))).handle)
  }
}

internal extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  func _vjpScalarized() -> (Scalar, (Scalar) -> Tensor) {
    return (scalarized(), { v in Tensor(v) })
  }
}

public extension TensorFlowScalar {
  @inlinable @inline(__always)
  init?(_ tensor: Tensor<Self>) {
    guard let scalar = _TFGetScalar(tensor.handle) else {
      return nil
    }
    self = scalar
  }
}

//===----------------------------------------------------------------------===//
// Array Conversion
//===----------------------------------------------------------------------===//

public extension Tensor {
  @inlinable
  var array: ShapedArray<Scalar> {
    @inline(__always)
    get {
      debugLog("Returning a host copy of array.")
      internalConsistencyCheck(toHost().handle.isConcrete)

      // This is considered to be a well known way to produce a copy to the
      // host, so an "implicit copy to host" warning should not be produced.
      return toHost().handle.makeHostCopy()
    }
  }

  @inlinable
  var scalars: [Scalar] {
    return array.scalars
  }
}

//===----------------------------------------------------------------------===//
// Tensor Properties
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// The number of dimensions of the `Tensor`.
  @inlinable
  var rank: Int {
    @inline(__always)
    @_semantics("autodiff.nonvarying")
    get {
      return Int(_TFGetScalarOrDie(rankTensor.handle))
    }
  }

  /// The dimensions of the `Tensor`.
  @inlinable
  var shape: TensorShape {
    @inline(__always)
    @_semantics("autodiff.nonvarying")
    get {
      return TensorShape(shapeTensor.scalars.map(Int.init))
    }
  }

  /// The number of scalars in the `Tensor`.
  @inlinable
  var scalarCount: Int {
    @inline(__always)
    get {
      return Int(_TFGetScalarOrDie(scalarCountTensor.handle))
    }
  }

  /// The rank of the tensor, represented as a `Tensor<Int32>`.
  @inlinable
  var rankTensor: Tensor<Int32> {
    @_semantics("autodiff.nonvarying")
    get {
        return Raw.rank(self)
    }
  }

  /// The dimensions of the tensor, represented as a `Tensor<Int32>`.
  @inlinable
  var shapeTensor: Tensor<Int32> {
    @_semantics("autodiff.nonvarying")
    get {
        return Raw.shape(self)
    }
  }

  /// The number of scalars in the tensor, represented as a `Tensor<Int32>`.
  @inlinable
  var scalarCountTensor: Tensor<Int32> {
    @_semantics("autodiff.nonvarying")
    get {
        return Raw.size(self)
    }
  }
}

//===----------------------------------------------------------------------===//
// Equatable
//===----------------------------------------------------------------------===//

extension Tensor : Equatable where Scalar : Equatable {
  @inlinable
  public static func == (lhs: Tensor, rhs: Tensor) -> Bool {
    let equal = Raw.equal(lhs, rhs)
    let axes = Raw.range(
      start: Tensor<Int32>(0),
      limit: Tensor<Int32>(Int32(equal.rank)),
      delta: Tensor<Int32>(1))
    return Raw.all(equal, reductionIndices: axes).scalarized()
  }

  @inlinable
  public static func != (lhs: Tensor, rhs: Tensor) -> Bool {
    let equal = Raw.equal(lhs, rhs)
    let axes = Raw.range(
      start: Tensor<Int32>(0),
      limit: Tensor<Int32>(Int32(equal.rank)),
      delta: Tensor<Int32>(1))
    return Raw.any(equal, reductionIndices: axes).scalarized()
  }
}

//===----------------------------------------------------------------------===//
// Additive Group
//===----------------------------------------------------------------------===//

extension Tensor : AdditiveArithmetic where Scalar : Numeric {
  /// A scalar zero tensor.
  @inlinable
  public static var zero: Tensor {
    return Tensor(0)
  }

  /// Adds two tensors and produces their sum.
  /// - Note: `+` supports broadcasting.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpAdd(lhs:rhs:) where Scalar : TensorFlowFloatingPoint)
  public static func + (lhs: Tensor, rhs: Tensor) -> Tensor {
    return Raw.add(lhs, rhs)
  }

  /// Subtracts one tensor from another and produces their difference.
  /// - Note: `-` supports broadcasting.
  @inlinable @inline(__always)
  @differentiable(
    vjp: _vjpSubtract(lhs:rhs:) where Scalar : TensorFlowFloatingPoint)
  public static func - (lhs: Tensor, rhs: Tensor) -> Tensor {
    return Raw.sub(lhs, rhs)
  }
}

internal extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpAdd(
    lhs: Tensor,
    rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return (lhs + rhs, {
      [lhsShape = lhs.shapeTensor, rhsShape = rhs.shapeTensor] v in
      if lhsShape == rhsShape {
        return (v, v)
      }
      let (lhsIndices, rhsIndices) = Raw.broadcastGradientArgs(s0: lhsShape, s1: rhsShape)
      let lhs = v.sum(squeezingAxes: lhsIndices).reshaped(toShape: lhsShape)
      let rhs = v.sum(squeezingAxes: rhsIndices).reshaped(toShape: rhsShape)
      return (lhs, rhs)
    })
  }

  @inlinable
  static func _vjpSubtract(
    lhs: Tensor,
    rhs: Tensor
  ) -> (Tensor, (Tensor) -> (Tensor, Tensor)) {
    return (lhs - rhs, {
      [lhsShape = lhs.shapeTensor, rhsShape = rhs.shapeTensor] v in
      if lhsShape == rhsShape {
        return (v, Raw.neg(v))
      }
      let (lhsIndices, rhsIndices) = Raw.broadcastGradientArgs(s0: lhsShape, s1: rhsShape)
      let lhs = v.sum(squeezingAxes: lhsIndices).reshaped(toShape: lhsShape)
      let rhs = Raw.neg(v).sum(squeezingAxes: rhsIndices).reshaped(toShape: rhsShape)
      return (lhs, rhs)
    })
  }
}

//===----------------------------------------------------------------------===//
// Differentiable
//===----------------------------------------------------------------------===//

extension Tensor : Differentiable where Scalar : TensorFlowFloatingPoint {
    public typealias TangentVector = Tensor
    public typealias CotangentVector = Tensor
    public typealias AllDifferentiableVariables = Tensor

    @inlinable
    public func tangentVector(
      from cotangent: CotangentVector
    ) -> TangentVector {
        return cotangent
    }
}
