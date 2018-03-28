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
// Tensor type
//===----------------------------------------------------------------------===//

@_fixed_layout
public struct Tensor<Scalar : AccelerableByTensorFlow> : TensorProtocol {
  public typealias BoolTensor = Tensor<Bool>

  /// A tensor just contains a TensorHandle under the covers.  This is public to
  /// allow user defined ops, but shouldn't normally be used otherwise.
  public let handle: TensorHandle<Scalar>

  @_inlineable
  public init(handle: TensorHandle<Scalar>) {
    self.handle = handle
  }
}

//===----------------------------------------------------------------------===//
// Compiler intrinsics
//===----------------------------------------------------------------------===//
//
// By default, when a tensor value is implicitly passed between host and tensor
// code, the partitioning pass will generate a warning.  Users can indicate that
// they are doing something intentional by using these methods, which silences
// the warning.
//
// TODO: These would be nicer if defined as builtins rather than being "well
// known functions".

@_versioned @inline(never)
@_silgen_name("__tf_send")
@effects(readnone)
func _TFSend<Scalar>(_ handle: TensorHandle<Scalar>) -> TensorHandle<Scalar> {
  return handle
}

@_versioned @inline(never)
@_silgen_name("__tf_receive")
func _TFReceive<Scalar>(_ handle: TensorHandle<Scalar>) -> TensorHandle<Scalar> {
  return handle
}

/// This function converts a TensorHandle that is known to have a 0d value into
/// the scalar that it produces.  This is intended for use in op definitions
/// where it is known that the Op always returns a 0d tensor, it is not for use
/// in general code.
@_versioned @_inlineable @inline(__always)
func _TFGetScalarOrDie<Scalar>(_ handle: TensorHandle<Scalar>) -> Scalar {
  return Scalar._getScalarOrDie(handle)
}

/// This function converts a TensorHandle into a scalar if it is 0d, or returns
/// nil otherwise.
@_versioned @_inlineable @inline(__always)
func _TFGetScalar<Scalar>(_ handle: TensorHandle<Scalar>) -> Scalar? {
  return Scalar._getScalar(handle)
}

/// This compiler builtin is known by the partitioning pass, which recognizes it
/// and promotes calls to it to being in graph when it can.  This signature was
/// designed to align with the requirements of the 'Const' Tensorflow operation.
@_versioned @inline(never)
@_silgen_name("__tf_tensor_from_scalars")
func _TFTensorFromScalars<Scalar>(_ scalars: [Scalar], shape: [Int32])
    -> TensorHandle<Scalar> {
  let contiguousSize = shape.map(Int.init).reduce(1, *)
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

@_versioned @_inlineable @inline(__always)
func _TFMakeScalarTensor<Scalar>(_ scalar: Scalar) -> TensorHandle<Scalar> {
  return Scalar._makeScalarTensor(scalar)
}

@_versioned @inline(never)
@_silgen_name("__tf_tensor_from_scalars_1d")
func _TFTensorFromScalars1D<Scalar>(_ scalars: [Scalar])
  -> TensorHandle<Scalar> {
  return _TFTensorFromScalars(scalars, shape: [Int32(scalars.count)])
}

@_versioned @_inlineable @inline(__always)
func _TFHoistable<Scalar>(_ fn: () -> TensorHandle<Scalar>)
  -> TensorHandle<Scalar> {
  return Scalar._hoistableClosure(fn)
}

//===----------------------------------------------------------------------===//
// Initialization
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  /// Perform an element-wise conversion from another `Tensor`.
  @_inlineable @inline(__always)
  init<OtherScalar : Numeric>(_ other: Tensor<OtherScalar>) {
    self.init(handle: #tfop("Cast", other.handle, DstT: Scalar.self))
  }
}

public extension Tensor {
  /// Creates a tensor from a scalar value.
  @_inlineable @inline(__always)
  init(_ value: Scalar) {
    self.init(handle: _TFMakeScalarTensor(value))
  }

  /// Creates a tensor from an array of tensors (which may themselves be
  /// scalars).
  @_inlineable @inline(__always)
  init<TensorType : TensorProtocol>(_ elements: [TensorType])
    where TensorType.Scalar == Scalar {
    self.init(handle: #tfop("Pack", elements))
  }

  /// Creates a 1D tensor in from contiguous scalars in row-major order.
  ///
  /// - Parameters:
  ///   - vector: The scalar contents of the tensor.
  /// - Precondition: The number of scalars must equal the product of the
  ///   dimensions of the shape.
  ///
  @_inlineable @inline(__always)
  init(_ vector: [Scalar]) {
    self.init(handle: _TFTensorFromScalars1D(vector))
  }

  /// Creates a 1D tensor in from contiguous scalars in row-major order.
  ///
  /// - Parameters:
  ///   - vector: The scalar contents of the tensor.
  /// - Precondition: The number of scalars must equal the product of the
  ///   dimensions of the shape.
  ///
  @_inlineable @inline(__always)
  init<C : RandomAccessCollection>(_ vector: C) where C.Element == Scalar {
    let handle = _TFHoistable {
      TensorHandle<Scalar>(
        shape: [Int32(vector.count)],
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
  @_inlineable @inline(__always)
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
  @_inlineable @inline(__always)
  init(shape: TensorShape, scalars: UnsafeBufferPointer<Scalar>) {
    let handle: TensorHandle<Scalar> = _TFHoistable {
      precondition(scalars.count == shape.contiguousSize)
      return TensorHandle<Scalar>(
        shape: shape.dimensions,
        scalarsInitializer: { addr in
          addr.initialize(from: scalars.baseAddress!,
                          count: Int(shape.contiguousSize))
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
  @_inlineable @inline(__always)
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

  /// Creates a tensor with the specified shape and a single, repeated value.
  ///
  /// - Parameters:
  ///   - shape: The dimensions of the tensor.
  ///   - repeatedValue: The scalar value to repeat.
  ///
  @_inlineable @inline(__always)
  init(shape: TensorShape, repeating repeatedValue: Scalar) {
    self.init(handle: #tfop("Fill", Tensor<Int32>(shape.dimensions),
                            Tensor(repeatedValue)))
  }

  /// Creates a tensor by broadcasting the given scalar to a given rank with
  /// all dimensions being 1.
  @_inlineable @inline(__always)
  init(broadcasting scalar: Scalar, rank: Int32) {
    let shapeTensor = Tensor<Int32>(shape: [rank], repeating: 1)
    self.init(handle: #tfop("Fill", shapeTensor, Tensor(scalar)))
  }

  /// Creates a tensor of shape `[4]` from a 4-tuple.
  /// - Note: This is intended for internal use, for example, to initialize a
  ///   tensor attribute from `convolved2D`'s `strides` argument.
  @_versioned @_inlineable @inline(__always)
  internal init(_ scalars: (Scalar, Scalar, Scalar, Scalar)) {
    self.init([scalars.0, scalars.1, scalars.2, scalars.3])
  }
}

//===----------------------------------------------------------------------===//
// Initialization Syntax
//===----------------------------------------------------------------------===//

extension Tensor : ExpressibleByIntegerLiteral
  where Scalar : ExpressibleByIntegerLiteral &
        _ExpressibleByBuiltinIntegerLiteral {
  public typealias IntegerLiteralType = Scalar
  @_inlineable @inline(__always)
  public init(integerLiteral: Scalar) {
    self.init(integerLiteral)
  }
}

extension Tensor : ExpressibleByFloatLiteral
  where Scalar : BinaryFloatingPoint &
        _ExpressibleByBuiltinFloatLiteral {
  public typealias FloatLiteralType = Scalar
  @_inlineable @inline(__always)
  public init(floatLiteral: Scalar) {
    self.init(floatLiteral)
  }
}

extension Tensor : ExpressibleByBooleanLiteral where Scalar == Bool {
  public typealias BooleanLiteralType = Bool
  @_inlineable @inline(__always)
  public init(booleanLiteral: Bool) {
    self.init(booleanLiteral)
  }
}

extension Tensor : ExpressibleByArrayLiteral {
  /// The type of the elements of an array literal.
  public typealias ArrayLiteralElement = Tensor<Scalar>
  /// Creates a tensor initialized with the given elements.
  @_inlineable @inline(__always)
  public init(arrayLiteral elements: Tensor<Scalar>...) {
    self.init(elements)
  }
}

//===----------------------------------------------------------------------===//
// Properties
//===----------------------------------------------------------------------===//

public extension Tensor {
  @_inlineable
  var shape: TensorShape {
    @inline(__always)
    get {
      return TensorShape(shapeTensor.scalars)
    }
  }

  @_inlineable
  var rank: Int32 {
    @inline(__always)
    get {
      return _TFGetScalarOrDie(rankTensor.handle)
    }
  }

  @_inlineable
  var scalarCount: Int32 {
    @inline(__always)
    get {
      return _TFGetScalarOrDie(scalarCountTensor.handle)
    }
  }
}

//===----------------------------------------------------------------------===//
// Numeric initialization
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  /// Creates a tensor with all scalars set to zero.
  ///
  /// - Parameter shape: The dimensions of the tensor.
  @_inlineable @inline(__always)
  init(zeros shape: TensorShape) {
    self.init(shape: shape, repeating: 0)
  }

  /// Creates a tensor with all scalars set to one.
  ///
  /// - Parameter shape: The dimensions of the tensor.
  @_inlineable @inline(__always)
  init(ones shape: TensorShape) {
    self.init(shape: shape, repeating: 1)
  }

  @inline(never) // make @_inlineable when implemented.
  static func eye(
    rowCount: Int, columnCount: Int? = nil, batchShape: [Int]? = nil
  ) -> Tensor {
    // NOTE: TF doesn't have an "Eye" op. Instead, the `tf.eye` function
    // composes many tensor/linear algebra ops.
    fatalError("FIXME: implement eye")
  }

  /// Creates a 1-D tensor representing a sequence from a starting value to, but
  /// not including, an end value, stepping by the specified amount.
  ///
  /// - Parameters:
  ///   - start: The starting value to use for the sequence. If the sequence
  ///     contains any values, the first one is `start`.
  ///   - end: An end value to limit the sequence. `end` is never an element of
  ///     the resulting sequence.
  ///   - stride: The amount to step by with each iteration. `stride` must be
  ///     positive.
  ///
  @_inlineable @inline(__always)
  init(rangeFrom start: Scalar, to end: Scalar, stride: Scalar) {
    self.init(
      handle: #tfop(
        "Range", Tensor(start), Tensor(end), Tensor(stride), Tidx: Scalar.self
      )
    )
  }

  /// Creates a one-hot tensor at given indices. The locations represented by
  /// `indices` take value `onValue` (`1` by default), while all other locations
  /// take value `offValue` (`0` by default). If the input `indices` is rank
  /// `n`, the new tensor will have rank `n+1`. The new axis is created at
  /// dimension `axis` (by default, the new axis is appended at the end).
  ///
  /// If `indices` is a scalar, the new tensor's shape will be a vector of
  /// length `depth`.
  ///
  /// If `indices` is a vector of length `features`, the output shape will be:
  ///     features x depth, if axis == -1
  ///     depth x features, if axis == 0
  ///
  /// If `indices` is a matrix (batch) with shape `[batch, features]`, the
  /// output shape will be:
  ///     batch x features x depth, if axis == -1
  ///     batch x depth x features, if axis == 1
  ///     depth x batch x features, if axis == 0
  ///
  /// - Parameters:
  ///   - indices: A `Tensor` of indices.
  ///   - depth: A scalar defining the depth of the one hot dimension.
  ///   - onValue: A scalar defining the value at the location referred to by
  ///     some index in `indices`.
  ///   - offValue: A scalar defining the value at a location that is not
  ///     referred to by any index in `indices`.
  ///   - axis: The axis to fill. The default is `-1`, a new inner-most axis.
  ///
  @_inlineable @inline(__always)
  init(oneHotAtIndices indices: Tensor<Int32>, depth: Int32,
       onValue: Scalar = 1, offValue: Scalar = 0, axis: Int = -1) {
    self.init(
      handle: #tfop("OneHot", indices, Tensor<Int32>(depth),
                    Tensor(onValue), Tensor(offValue),
                    axis: axis, T: Scalar.self, TI: Int32.self)
    )
  }
}

//===----------------------------------------------------------------------===//
// Random initialization
//===----------------------------------------------------------------------===//

public extension Float {
  static func randomUniform(state: RandomState? = nil) -> Float {
    let state = state ?? RandomState.global
    return Float(state.generate()) / 0xFFFFFFFF
  }

  private static var boxMullerHelper: Float = randomUniform()

  /// Random value from normal distribution using the Box-Muller method.
  static func randomNormal(mean: Float = 0, stddev: Float = 1,
                           state: RandomState? = nil) -> Float {
    let tmp = randomUniform(state: state)
    let result = sqrtf(-2 * logf(tmp)) * cosf(2 * .pi * boxMullerHelper)
    boxMullerHelper = result
    return result * stddev + mean
  }
}

public extension Double {
  static func randomUniform(state: RandomState? = nil) -> Double {
    let state = state ?? RandomState.global
    return Double(state.generate()) / 0xFFFFFFFF
  }

  private static var boxMullerHelper: Double = randomUniform()

  /// Random value from normal distribution using the Box-Muller method.
  static func randomNormal(mean: Double = 0, stddev: Double = 1,
                           state: RandomState? = nil) -> Double {
    let tmp = randomUniform(state: state)
    let result = sqrt(-2 * log(tmp)) * cos(2 * .pi * boxMullerHelper)
    boxMullerHelper = result
    return result * stddev + mean
  }
}

public extension Tensor where Scalar == Int32 {
  /// Creates a tensor with the specified shape, randomly sampling scalar values
  /// from a discrete uniform distribution.
  ///
  /// - Parameters:
  ///   - shape: The dimensions of the tensor.
  ///   - state: The pseudorandom state in which the random numbers are being
  ///     generated.
  ///
  @_inlineable @inline(__always)
  init(randomStandardUniform shape: TensorShape, state: RandomState? = nil) {
    self = Tensor(
      handle: _TFHoistable {
        let state = state ?? RandomState.global
        return _TFTensorFromScalars(state.generate(Int(shape.contiguousSize)),
                                    shape: shape.dimensions)
      }
    ).toDevice()
  }
}

public extension Tensor where Scalar : FloatingPoint {
  /// Creates a tensor with the specified shape, randomly sampling scalar values
  /// from a uniform distribution between 0 and 1.
  ///
  /// - Parameters:
  ///   - shape: The dimensions of the tensor.
  ///   - state: The pseudorandom state in which the random numbers are being
  ///     generated.
  ///
  @_inlineable @inline(__always)
  init(randomUniform shape: TensorShape, state: RandomState? = nil) {
    self = Tensor(
      Tensor<Int32>(randomStandardUniform: shape, state: state)
    ) / 0xFFFFFFFF
  }

  /// Creates a tensor with the specified shape, randomly sampling scalar values
  /// from a normal distribution.
  ///
  /// - Parameters:
  ///   - shape: The dimensions of the tensor.
  ///   - mean: The mean of the distribution.
  ///   - stddev: The standard deviation of the distribution.
  ///   - state: The pseudorandom state in which the random numbers are being
  ///     generated.
  ///
  @_inlineable @inline(__always)
  init(randomNormal shape: TensorShape, mean: Scalar = 0, stddev: Scalar = 1,
       state: RandomState? = nil) {
    let uniform = Tensor(randomUniform: shape, state: state)
    let boxMullerHelper = Tensor(randomUniform: shape, state: state)
    let result = sqrt(-2 * log(uniform)) * cos(2 * .pi * boxMullerHelper)
    self = result * stddev + mean
  }
}

//===----------------------------------------------------------------------===//
// Shape transformations
//===----------------------------------------------------------------------===//

public extension AccelerableByTensorFlow {
  /// Convert to a tensor with the specified rank, with all dimensions equal to
  /// 1.
  @_inlineable @inline(__always)
  func makeTensor(withRank rank: Int32) -> Tensor<Self> {
    return #tfop("Fill", Tensor<Int32>(ones: TensorShape(rank)), Tensor(self))
  }
}

public extension Tensor where Scalar : Numeric {
  @_inlineable @inline(__always)
  func broadcast(toShape shape: Tensor<Int32>) -> Tensor {
    let zeros: Tensor = #tfop("Fill", shape, Tensor(0))
    return self + zeros
  }

  /// Broadcast to the same shape as the specified Tensor.
  /// - Precondition: The specified shape must be compatible for broadcasting.
  // TODO: This is a temporary workaround for supporting broadcast on numeric
  // tensors. Remove this function once a general working broadcast is
  // implemented.
  @_inlineable @inline(__always)
  func broadcast(to other: Tensor) -> Tensor {
    return broadcast(toShape: other.shapeTensor)
  }

  @_inlineable @inline(__always)
  func broadcast(to shape: TensorShape) -> Tensor {
    let zeros: Tensor = #tfop("Fill", Tensor<Int32>(shape.dimensions), Tensor(0))
    return self + zeros
  }
}

public extension Tensor {
  /// Reshape to the shape of the specified Tensor.
  /// - Precondition: The number of scalars matches the new shape.
  @_inlineable @inline(__always)
  func reshaped<T>(like other: Tensor<T>) -> Tensor {
    return reshaped(toShape: other.shapeTensor)
  }

  /// Reshape to the specified shape.
  /// - Precondition: The number of scalars matches the new shape.
  @_inlineable @inline(__always)
  func reshaped(to newShape: TensorShape) -> Tensor {
    return reshaped(toShape: Tensor<Int32>(newShape.dimensions))
  }

  /// Reshape to the specified Tensor representing a shape.
  /// - Precondition: The number of scalars matches the new shape.
  @_inlineable @inline(__always)
  @differentiable(
    withRespectTo: (self),
    gradient: _adjointReshaped(toShape:partial:seed:)
  )
  func reshaped(toShape newShape: Tensor<Int32>) -> Tensor {
    return #tfop("Reshape", handle, newShape)
  }

  /// Return a copy of the tensor collapsed into a 1-D Tensor, in row-major
  /// order.
  @_inlineable @inline(__always)
  func flattened() -> Tensor {
    return reshaped(to: [-1])
  }

  /// Returns a rank-lifted Tensor with a leading dimension of 1.
  @_inlineable @inline(__always)
  func rankLifted() -> Tensor {
    return expandingShape(at: 0)
  }

  /// Returns a shape-expanded Tensor, with a dimension of 1 inserted at the
  /// specified shape index.
  @_inlineable @inline(__always)
  @differentiable(
    withRespectTo: (self),
    gradient: _adjointExpandingShape(at:partial:seed:)
  )
  func expandingShape(at shapeIndex: Int32) -> Tensor {
    return #tfop("ExpandDims", handle, Tensor<Int32>(shapeIndex),
                 Tdim: Int32.self)
  }

  /// Remove the specified dimensions of size 1 from the shape of a tensor. If
  /// no dimensions are specified, then all dimensions of size 1 will be
  /// removed.
  // NOTE: the gradient for variadic `squeezed` is difficult to express because
  // ExpandDims only expands one axis at a time.
  @_inlineable @inline(__always)
  func squeezingShape(at axes: Int32...) -> Tensor {
    return #tfop("Squeeze", handle, squeeze_dims: axes)
  }

  /// Reshape to scalar.
  /// - Precondition: The tensor has exactly one scalar.
  @_inlineable
  func scalarized() -> Scalar {
    return _TFGetScalarOrDie(reshaped(to: []).handle)
  }
}

//===----------------------------------------------------------------------===//
// Scalar conversion
//===----------------------------------------------------------------------===//

public extension Tensor {
  @_inlineable
  var isScalar: Bool {
    @inline(__always)
    get {
      return rank == 0
    }
  }

  /// Returns the underlying scalar from a 0-ranked Tensor.
  /// - precondition: Tensor is 0-ranked.
  @_inlineable
  var scalar: Scalar? {
    @inline(__always)
    get {
      return Scalar(self)
    }
  }
}

public extension AccelerableByTensorFlow {
  @_inlineable @inline(__always)
  init?(_ tensor: Tensor<Self>) {
    guard let scalar = _TFGetScalar(tensor.handle) else {
      return nil
    }
    self = scalar
  }
}

//===----------------------------------------------------------------------===//
// Automatic differentiation
//===----------------------------------------------------------------------===//

extension Tensor : Differentiable where Scalar : FloatingPoint {
  /// The currency type in the mathematical model of differentiation.
  public typealias DifferentiationCurrency = Scalar

  /// Creates an instance by numerically broadcasting the specified currency
  /// value to be structurally isomorphic to another instance.
  ///
  /// - Parameters:
  ///   - value: The differentiation currency value for initializing the
  ///     instance.
  ///   - other: The other structurally isomorphic instance.
  ///
  @_inlineable @inline(__always)
  public init(numericallyBroadcasting value: Scalar, to other: Tensor) {
    self.init(handle: #tfop("Fill", other.shapeTensor, value))
  }

  @_inlineable @inline(__always)
  public func combiningAsAdjoint(with other: Tensor) -> Tensor {
    return self + other
  }
}

//===----------------------------------------------------------------------===//
// Equality
//===----------------------------------------------------------------------===//

extension Tensor : Equatable where Scalar : Equatable {
  @_inlineable @inline(__always)
  public static func == (lhs: Tensor, rhs: Tensor) -> Bool {
    return lhs.elementsEqual(rhs).all()
  }

  @_inlineable @inline(__always)
  public static func != (lhs: Tensor, rhs: Tensor) -> Bool {
    return lhs.elementsNotEqual(rhs).any()
  }
}

//===----------------------------------------------------------------------===//
// Description and visualization
//===----------------------------------------------------------------------===//

/// String conversion.
extension Tensor : CustomStringConvertible {
  public var description: String {
    return array.description
  }
}

// Xcode Playground display conversion.
extension Tensor : CustomPlaygroundDisplayConvertible {
  public var playgroundDescription: Any {
    return description
  }
}

// Mirror representation, used by debugger/REPL.
extension Tensor : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: [], displayStyle: .struct)
  }
}

//===----------------------------------------------------------------------===//
// Array conversion
//===----------------------------------------------------------------------===//

public extension Tensor {
  @_inlineable
  var array: ShapedArray<Scalar> {
    @inline(__always)
    get {
      debugLog("Returning a host copy of array.")
      // This is considered to be a well known way to produce a copy to the
      // host, so an "implicit copy to host" warning should not be produced.
      return toHost().handle.makeHostCopy()
    }
  }

  @_inlineable
  var scalars: [Scalar] {
    return array.scalars
  }
}
