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

infix operator .== : ComparisonPrecedence

//===----------------------------------------------------------------------===//
// Tensor
//===----------------------------------------------------------------------===//

/// `Tensor` is a multi-dimensional array used for computation.
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
// Initialization
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  /// Perform an element-wise conversion from another `Tensor`.
  @inlinable
  @differentiable(
    vjp: _vjpCast where Scalar : TensorFlowFloatingPoint,
                        OtherScalar: TensorFlowFloatingPoint)
  init<OtherScalar : Numeric>(_ other: Tensor<OtherScalar>) {
    self = Raw.cast(other)
  }
}

internal extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpCast<OtherScalar : TensorFlowFloatingPoint>(
    _ other: Tensor<OtherScalar>
  ) -> (Tensor, (Tensor) -> Tensor<OtherScalar>) {
    return (Tensor(other), { v in Tensor<OtherScalar>(v) })
  }
}

public extension Tensor {
  /// Creates a 0-D tensor from a scalar value.
  @inlinable
  @differentiable(vjp: _vjpScalarInit where Scalar : TensorFlowFloatingPoint)
  init(_ value: Scalar) {
    self.init(shape: [], scalars: [value])
  }
}

internal extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpScalarInit(_ value: Scalar) -> (Tensor, (Tensor) -> Scalar) {
    return (Tensor(value), { $0.scalarized() })
  }
}

public extension Tensor {
  /// Creates a tensor from an array of tensors (which may themselves be
  /// scalars).
  @inlinable
  init(_ elements: [Tensor]) {
    self = Raw.pack(elements)
  }

  /// Creates a 1-D tensor from scalars.
  @inlinable
  init(_ scalars: [Scalar]) {
    self.init(shape: [scalars.count], scalars: scalars)
  }

  /// Creates a 1-D tensor from scalars.
  @inlinable
  init<C : RandomAccessCollection>(_ scalars: C) where C.Element == Scalar {
    let handle = TensorHandle<Scalar>(
      shape: [scalars.count],
      scalarsInitializer: { addr in
        var currentAddr = addr
        for scalar in scalars {
          currentAddr.initialize(to: scalar)
          currentAddr = currentAddr.advanced(by: 1)
        }
      }
    )
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
  @inlinable
  init(shape: TensorShape, scalars: [Scalar]) {
    self = scalars.withUnsafeBufferPointer { buf in
      Tensor(shape: shape, scalars: buf)
    }
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
  @inlinable
  internal init(shape: TensorShape, scalars: UnsafeBufferPointer<Scalar>) {
    precondition(scalars.count == shape.contiguousSize)
    let handle = TensorHandle<Scalar>(
      shape: shape.dimensions,
      scalarsInitializer: { addr in
        addr.initialize(from: scalars.baseAddress!,
                        count: shape.contiguousSize)
      }
    )
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
  @inlinable
  init<C : RandomAccessCollection>(shape: TensorShape, scalars: C)
    where C.Element == Scalar {
    precondition(scalars.count == shape.contiguousSize)
    let handle = TensorHandle<Scalar>(
      shape: shape.dimensions,
      scalarsInitializer: { addr in
        var currentAddr = addr
        for scalar in scalars {
          currentAddr.initialize(to: scalar)
          currentAddr = currentAddr.advanced(by: 1)
        }
      }
    )
    self.init(handle: handle)
  }
}

public extension Tensor {
  /// Creates a tensor with the specified shape and a single, repeated scalar
  /// value.
  ///
  /// - Parameters:
  ///   - shape: The dimensions of the tensor.
  ///   - repeatedValue: The scalar value to repeat.
  @inlinable
  @available(*, deprecated, renamed: "init(repeating:shape:)")
  init(shape: TensorShape, repeating repeatedValue: Scalar) {
    self.init(repeating: repeatedValue, shape: shape)
  }

  /// Creates a tensor with the specified shape and a single, repeated scalar
  /// value.
  ///
  /// - Parameters:
  ///   - repeatedValue: The scalar value to repeat.
  ///   - shape: The dimensions of the tensor.
  @inlinable
  @differentiable(vjp: _vjpInit(repeating:shape:)
                  where Scalar : TensorFlowFloatingPoint)
  init(repeating repeatedValue: Scalar, shape: TensorShape) {
    self = Raw.fill(dims: Tensor<Int32>(shape.dimensions.map(Int32.init)),
                    value: Tensor(repeatedValue))
  }
}

internal extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  static func _vjpInit(
    repeating repeatedValue: Scalar,
    shape: TensorShape
  ) -> (Tensor, (Tensor) -> Scalar) {
    return (Tensor(repeating: repeatedValue, shape: shape),
            { $0.sum().scalarized() })
  }
}

public extension Tensor {
  /// Creates a tensor by broadcasting the given scalar to a given rank with
  /// all dimensions being 1.
  @inlinable
  // @differentiable(where Scalar : TensorFlowFloatingPoint)
  init(broadcasting scalar: Scalar, rank: Int) {
    self = Tensor(scalar).reshaped(to: TensorShape(repeating: 1, count: rank))
  }

  /// Creates a tensor of shape `[4]` from a 4-tuple.
  /// - Note: This is intended for internal use, for example, to initialize a
  ///   tensor attribute from `convolved2D`'s `strides` argument.
  @inlinable
  internal init(_ scalars: (Scalar, Scalar, Scalar, Scalar)) {
    self.init([scalars.0, scalars.1, scalars.2, scalars.3])
  }
}

//===----------------------------------------------------------------------===//
// Initialization syntax
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
  @inlinable
  public init(booleanLiteral: BooleanLiteralType) {
    tensor = Tensor(Scalar(booleanLiteral: booleanLiteral))
  }
}

extension _TensorElementLiteral : ExpressibleByIntegerLiteral
  where Scalar : ExpressibleByIntegerLiteral {
  public typealias IntegerLiteralType = Scalar.IntegerLiteralType
  @inlinable
  public init(integerLiteral: IntegerLiteralType) {
    tensor = Tensor(Scalar(integerLiteral: integerLiteral))
  }
}

extension _TensorElementLiteral : ExpressibleByFloatLiteral
  where Scalar : ExpressibleByFloatLiteral {
  public typealias FloatLiteralType = Scalar.FloatLiteralType
  @inlinable
  public init(floatLiteral: FloatLiteralType) {
    tensor = Tensor(Scalar(floatLiteral: floatLiteral))
  }
}

extension _TensorElementLiteral : ExpressibleByArrayLiteral {
  public typealias ArrayLiteralElement = _TensorElementLiteral<Scalar>
  @inlinable
  public init(arrayLiteral elements: _TensorElementLiteral<Scalar>...) {
    tensor = Raw.pack(elements.map { $0.tensor })
  }
}

extension Tensor : ExpressibleByArrayLiteral {
  /// The type of the elements of an array literal.
  public typealias ArrayLiteralElement = _TensorElementLiteral<Scalar>

  /// Creates a tensor initialized with the given elements.
  /// - Note: This is for conversion from tensor element literals. This is a
  /// separate method because `ShapedArray` initializers need to call it.
  @inlinable
  internal init(
    _tensorElementLiterals elements: [_TensorElementLiteral<Scalar>]
  ) {
    self = Raw.pack(elements.map { $0.tensor })
  }

  /// Creates a tensor initialized with the given elements.
  @inlinable
  public init(arrayLiteral elements: _TensorElementLiteral<Scalar>...) {
    self.init(_tensorElementLiterals: elements)
  }
}

//===----------------------------------------------------------------------===//
// Properties
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// The number of dimensions of the `Tensor`.
  @inlinable
  var rank: Int {
    @_semantics("autodiff.nonvarying")
    get {
      let status = _ExecutionContext.global.status
      let rank = TFE_TensorHandleNumDims(handle._cTensorHandle, status)
      checkOk(status)
      return Int(rank)
    }
  }

  /// The shape of the `Tensor`.
  @inlinable
  var shape: TensorShape {
    @_semantics("autodiff.nonvarying")
    get {
      let status = _ExecutionContext.global.status
      let dims: [Int] = (0..<Int32(rank)).map { i in
        let dim = TFE_TensorHandleDim(self.handle._cTensorHandle, i, status)
        checkOk(status)
        return Int(dim)
      }
      return TensorShape(dims)
    }
  }

  /// The number of scalars in the `Tensor`.
  @inlinable
  var scalarCount: Int {
    @_semantics("autodiff.nonvarying")
    get {
      let status = _ExecutionContext.global.status
      let size = TFE_TensorHandleNumElements(handle._cTensorHandle, status)
      checkOk(status)
      return Int(size)
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
  @inlinable
  init(zeros shape: TensorShape) {
    self.init(repeating: 0, shape: shape)
  }

  /// Creates a tensor with all scalars set to one.
  ///
  /// - Parameter shape: The dimensions of the tensor.
  @inlinable
  init(ones shape: TensorShape) {
    self.init(repeating: 1, shape: shape)
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
  @inlinable
  init(rangeFrom start: Scalar, to end: Scalar, stride: Scalar) {
    self = Raw.range(
      start: Tensor(start),
      limit: Tensor(end),
      delta: Tensor(stride))
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
  @inlinable
  init(oneHotAtIndices indices: Tensor<Int32>, depth: Int,
       onValue: Scalar = 1, offValue: Scalar = 0, axis: Int = -1) {
    self = Raw.oneHot(
      indices: indices,
      depth: Tensor<Int32>(Int32(depth)),
      onValue: Tensor(onValue),
      offValue: Tensor(offValue),
      axis: Int64(axis)
    )
  }
}

//===----------------------------------------------------------------------===//
// Shape transformations
//===----------------------------------------------------------------------===//

public extension TensorFlowScalar {
  /// Convert to a tensor with the specified rank, with all dimensions equal to
  /// 1.
  @inlinable
  func makeTensor(rank: Int) -> Tensor<Self> {
    return Tensor(repeating: self, shape: TensorShape(rank))
  }
}

public extension Tensor {
  /// Reshape to the shape of the specified `Tensor`.
  /// - Precondition: The number of scalars matches the new shape.
  @inlinable
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func reshaped<T>(like other: Tensor<T>) -> Tensor {
    return reshaped(toShape: other.shapeTensor)
  }

  /// Reshape to the specified shape.
  /// - Precondition: The number of scalars matches the new shape.
  @inlinable
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func reshaped(to newShape: TensorShape) -> Tensor {
    // TODO(TF-433): Remove workaround for differentiating `map`.
    return reshaped(
      toShape: Tensor<Int32>({newShape.dimensions.map(Int32.init)}()))
  }

  /// Reshape to the specified `Tensor` representing a shape.
  /// - Precondition: The number of scalars matches the new shape.
  @inlinable
  @differentiable(
    wrt: self, vjp: _vjpReshaped(toShape:)
    where Scalar : TensorFlowFloatingPoint
  )
  func reshaped(toShape newShape: Tensor<Int32>) -> Tensor {
    return Raw.reshape(self, shape: newShape)
  }

  /// Return a copy of the tensor collapsed into a 1-D `Tensor`, in row-major
  /// order.
  @inlinable
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func flattened() -> Tensor {
    return reshaped(to: [-1])
  }

  /// Returns a rank-lifted `Tensor` with a leading dimension of 1.
  @inlinable
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func rankLifted() -> Tensor {
    return expandingShape(at: 0)
  }

  /// Returns a shape-expanded `Tensor`, with a dimension of 1 inserted at the
  /// specified shape indices.
  @inlinable
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func expandingShape(at axes: Int...) -> Tensor {
    return expandingShape(at: axes)
  }
   
  /// Returns a shape-expanded `Tensor`, with a dimension of 1 inserted at the
  /// specified shape indices.
  @inlinable
  @differentiable(
    wrt: self, vjp: _vjpExpandingShape(at:)
    where Scalar : TensorFlowFloatingPoint
  )
  func expandingShape(at axes: [Int]) -> Tensor {
    var res = self
    for i in axes { res = Raw.expandDims(res, dim: Tensor<Int32>(Int32(i))) }
    return res
  }

  /// Remove the specified dimensions of size 1 from the shape of a tensor. If
  /// no dimensions are specified, then all dimensions of size 1 will be
  /// removed.
  @inlinable
  @differentiable(wrt: self where Scalar : TensorFlowFloatingPoint)
  func squeezingShape(at axes: Int...) -> Tensor {
    return squeezingShape(at: axes)
  }

  /// Remove the specified dimensions of size 1 from the shape of a tensor. If
  /// no dimensions are specified, then all dimensions of size 1 will be
  /// removed.
  @inlinable
  @differentiable(
    wrt: self, vjp: _vjpSqueezingShape(at:)
    where Scalar : TensorFlowFloatingPoint
  )
  func squeezingShape(at axes: [Int]) -> Tensor {
    return Raw.squeeze(self, squeezeDims: axes.map(Int32.init))
  }

  /// Reshape to scalar.
  /// - Precondition: The tensor has exactly one scalar.
  @inlinable
  @differentiable(wrt: self,
                  vjp: _vjpScalarized where Scalar : TensorFlowFloatingPoint)
  func scalarized() -> Scalar {
    return reshaped(to: []).scalar!
  }
}

extension Tensor where Scalar : TensorFlowFloatingPoint {
  @inlinable
  func _vjpScalarized() -> (Scalar, (Scalar) -> Tensor) {
    return (scalarized(), { v in Tensor(v) })
  }
}

//===----------------------------------------------------------------------===//
// Scalar conversion
//===----------------------------------------------------------------------===//

public extension Tensor {
  /// Returns `true` if `rank` is equal to 0 and `false` otherwise.
  @inlinable
  var isScalar: Bool {
    return rank == 0
  }

  /// Returns the single scalar element if `rank` is equal to 0 and `nil`
  /// otherwise.
  @inlinable
  var scalar: Scalar? {
    return handle.makeHostCopy().scalar
  }
}

public extension TensorFlowScalar {
  @inlinable
  init?(_ tensor: Tensor<Self>) {
    guard let scalar = tensor.scalar else {
      return nil
    }
    self = scalar
  }
}

//===----------------------------------------------------------------------===//
// Equality
//===----------------------------------------------------------------------===//

extension Tensor : Equatable where Scalar : Equatable {
  @inlinable
  public static func == (lhs: Tensor, rhs: Tensor) -> Bool {
    return (lhs .== rhs).all()
  }

  @inlinable
  public static func != (lhs: Tensor, rhs: Tensor) -> Bool {
    return (lhs .== rhs).any()
  }
}

//===----------------------------------------------------------------------===//
// Description and visualization
//===----------------------------------------------------------------------===//

// String conversion.
extension Tensor : CustomStringConvertible {
  /// A textual representation of the tensor.
  ///
  /// - Note: use `fullDescription` for a non-pretty-printed description showing
  ///   all scalars.
  public var description: String {
    return array.description
  }
}

public extension Tensor {
  /// A textual representation of the tensor. Returns a summarized description
  /// if `summarize` is true and the element count exceeds twice the
  /// `edgeElementCount`.
  ///
  /// - Parameters:
  ///   - lineWidth: The max line width for printing. Used to determine number
  ///     of scalars to print per line.
  ///   - edgeElementCount: The maximum number of elements to print before and
  ///     after summarization via ellipses (`...`).
  ///   - summarizing: If true, summarize description if element count exceeds
  ///     twice `edgeElementCount`.
  func description(
    lineWidth: Int = 80, edgeElementCount: Int = 3, summarizing: Bool = false
  ) -> String {
    return array.description(
      lineWidth: lineWidth, edgeElementCount: edgeElementCount,
      summarizing: summarizing)
  }

  /// A full, non-pretty-printed textual representation of the tensor, showing
  /// all scalars.
  var fullDescription: String {
    return array.fullDescription
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
  @inlinable
  var array: ShapedArray<Scalar> {
    debugLog("Returning a host copy of array.")
    internalConsistencyCheck(handle.isConcrete)
    return handle.makeHostCopy()
  }

  @inlinable
  var scalars: [Scalar] {
    return array.scalars
  }
}

//===----------------------------------------------------------------------===//
// Codable conformance
//===----------------------------------------------------------------------===//

extension Tensor : Codable where Scalar : Codable {
  @inlinable
  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(array)
  }

  @inlinable
  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    let array = try container.decode(ShapedArray<Scalar>.self)
    self.init(array)
  }
}
