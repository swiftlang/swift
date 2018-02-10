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

// NOTE: Pretty much everything here is marked @_inlineable/@_versioned.  This
// causes the body to be serialized into the generated Swift module, which
// allows it to be inlined into the user's code during deabstraction.  This is
// really gross, and will get better one way or the other, through compiler
// changes.

import CTensorFlow

//===----------------------------------------------------------------------===//
// Tensor type
//===----------------------------------------------------------------------===//

public struct Tensor<Scalar : AccelerableByTensorFlow> {
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
@effects(readnone)
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
               "The number of scalars doesn't match the shape.")
  return TensorHandle(
      shape: shape,
      scalarsInitializer: { addr in
        scalars.withUnsafeBufferPointer { ptr in
          addr.assign(from: ptr.baseAddress!, count: contiguousSize)
        }
    })
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

//===----------------------------------------------------------------------===//
// Memory transfer markers
//===----------------------------------------------------------------------===//

/// TODO: Remove when send/receive semantics gets revisited.
public extension Tensor {
  @_inlineable @inline(__always)
  func toDevice() -> Tensor {
    return Tensor(handle: _TFSend(handle))
  }

  @_inlineable @inline(__always)
  func toHost() -> Tensor {
    return Tensor(handle: _TFReceive(handle))
  }
}

//===----------------------------------------------------------------------===//
// Initialization
//===----------------------------------------------------------------------===//

extension Tensor where Scalar : Numeric {
  /// Perform an element conversion from Tensor<U> to Tensor<T>.
  @_inlineable @inline(__always)
  public init<FromType : Numeric>(_ other: Tensor<FromType>) {
    self.init(handle: #tfop("Cast", other.handle, DstT: Scalar.self))
  }
}

public extension Tensor {
  /// Initialize a tensor with a scalar representing a scalar value.
  @_inlineable @inline(__always)
  init(_ value: Scalar) {
    self.init(handle: _TFMakeScalarTensor(value))
  }

  /// Initialize a tensor with an array representing a vector.
  @_inlineable @inline(__always)
  init(_ vector: [Scalar]) {
    self.init(handle: _TFTensorFromScalars1D(vector))
  }

  /// Initialize a tensor with an array of arrays representing a matrix.
  ///
  /// - Precondition: The number of elements in each sub-dimensional array in
  ///   the array must be equal.
  @_inlineable
  init(_ literal: [[Scalar]]) {
    /// Sanity checks.
    let dim0 = literal.count
    let dim1 = literal.first?.count ?? 0
    for subArray in literal {
      precondition(subArray.count == dim1, """
        Each dimension must have an equal number of subdimensions.
        """)
    }
    // We don't want to delegate initialization to `init(shape:scalars:)`
    // because flattening `matrix` to an array is an unnecessary cost.
    let tensorHandle = TensorHandle<Scalar>(
      shape: [Int32(dim0), Int32(dim1)],
      scalarsInitializer: { addr in
        // Copy to TF_Tensor memory, one row at a time.
        for (i, subArray) in literal.enumerated() {
          subArray.withUnsafeBufferPointer { ptr in
            addr.advanced(by: i * dim1)
              .assign(from: ptr.baseAddress!, count: dim1)
          }
        }
      }
    )
    self.init(handle: tensorHandle)
  }

  /// Initialize a tensor with an array of arrays of arrays representing a
  /// 3D tensor.
  ///
  /// - Precondition: The number of elements in each sub-dimensional array in
  ///   the array must be equal.
  /// - TODO: improve description
  @_inlineable
  init(_ literal: [[[Scalar]]]) {
    /// Sanity checks.
    let dim0 = literal.count
    let dim1 = literal.first?.count ?? 0
    let dim2 = literal.first?.first?.count ?? 0
    for subArray in literal {
      precondition(subArray.count == dim1, """
        Each dimension must have an equal number of subdimensions.
        """)
      for subSubArray in subArray {
        precondition(subSubArray.count == dim2, """
          Each dimension must have an equal number of subdimensions.
          """)
      }
    }
    // We don't want to delegate initialization to `init(shape:scalars:)`
    // because flattening `literal` to an array is an unnecessary cost.
    let tensorHandle = TensorHandle<Scalar>(
      shape: [Int32(dim0), Int32(dim1), Int32(dim2)],
      scalarsInitializer: { addr in
        // Copy to TF_Tensor memory, one innermost array at a time.
        for (i, subArray) in literal.enumerated() {
          for (j, subSubArray) in subArray.enumerated() {
            subSubArray.withUnsafeBufferPointer { ptr in
                addr.advanced(by: i * dim1 + j * dim2)
                  .assign(from: ptr.baseAddress!, count: dim2)
            }
          }
        }
      }
    )
    self.init(handle: tensorHandle)
  }

  /// Initialize a tensor with an array of array of arrays of arrays
  /// representing a 4D tensor.
  ///
  /// - Precondition: The number of elements in each sub-dimensional array in
  ///   the array must be equal.
  /// - TODO: improve description
  @_inlineable
  init(_ literal: [[[[Scalar]]]]) {
    /// Sanity checks.
    let dim0 = literal.count
    let dim1 = literal.first?.count ?? 0
    let dim2 = literal.first?.first?.count ?? 0
    let dim3 = literal.first?.first?.first?.count ?? 0
    for subArray in literal {
      precondition(subArray.count == dim1, """
        Each dimension must have an equal number of subdimensions.
        """)
      for subSubArray in subArray {
        precondition(subSubArray.count == dim2, """
          Each dimension must have an equal number of subdimensions.
          """)
        for subSubSubArray in subSubArray {
          precondition(subSubSubArray.count == dim3, """
            Each dimension must have an equal number of subdimensions.
            """)
        }
      }
    }
    // We don't want to delegate initialization to `init(shape:scalars:)`
    // because flattening `literal` to an array is an unnecessary cost.
    let tensorHandle = TensorHandle<Scalar>(
      shape: [Int32(dim0), Int32(dim1), Int32(dim2), Int32(dim3)],
      scalarsInitializer: { addr in
        // Copy to TF_Tensor memory, one innermost array at a time.
        for (i, subArray) in literal.enumerated() {
          for (j, subSubArray) in subArray.enumerated() {
            for (k, subSubSubArray) in subSubArray.enumerated() {
              subSubSubArray.withUnsafeBufferPointer { ptr in
                addr.advanced(by: i * dim1 + j * dim2 + k * dim3)
                  .assign(from: ptr.baseAddress!, count: dim3)
              }
            }
          }
        }
      }
    )
    self.init(handle: tensorHandle)
  }

  /// Initialize a tensor with arbitrary shape.
  /// - Precondition: The number of scalars should be the same as the
  ///   product of all of shape's dimensions.
  @_inlineable @inline(__always)
  init(shape: TensorShape, scalars: [Scalar]) {
    self.init(handle: _TFTensorFromScalars(scalars, shape: shape.dimensions))
  }

  /// Initialize a tensor of a specified shape, filled with a single value.
  @_inlineable @inline(__always)
  init(shape: TensorShape, repeating repeatedValue: Scalar) {
    let valueTensor = Tensor(repeatedValue).handle
    let shapeTensor = Tensor<Int32>(shape.dimensions).handle
    self.init(handle: #tfop("Fill", shapeTensor, valueTensor))
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
  var rank: Int {
    @inline(__always)
    get {
      return Int(rankTensor.scalar!)
    }
  }

  @_inlineable
  var scalarCount: Int {
    @inline(__always)
    get {
      return Int(scalarCountTensor.scalar!)
    }
  }
}

//===----------------------------------------------------------------------===//
// Factory initializers for numeric tensors
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  /// Initialize a tensor with all elements set to zero.
  ///
  /// - Parameter shape: the dimensions of the tensor.
  @_inlineable @inline(__always)
  init(zeros shape: TensorShape) {
    self.init(shape: shape, repeating: 0)
  }

  /// Initialize a tensor with all elements set to one.
  ///
  /// - Parameter shape: the dimensions of the tensor.
  @_inlineable @inline(__always)
  init(ones shape: TensorShape) {
    self.init(shape: shape, repeating: 1)
  }

  @inline(never) // make @_inlineable when implemented.
  static func eye(
    rowCount: Int, columnCount: Int? = nil, batchShape: [Int]? = nil
  ) -> Tensor {
    fatalError("FIXME: implement eye")
  }

  /// Initialize a 1-D tensor representing a sequence from a starting value to,
  /// but not including, an end value, stepping by the specified amount.
  ///
  /// - Parameters:
  ///   - start: The starting value to use for the sequence. If the sequence
  ///     contains any values, the first one is `start`.
  ///   - end: An end value to limit the sequence. `end` is never an element of
  ///     the resulting sequence.
  ///   - stride: The amount to step by with each iteration. `stride` must be
  ///     positive.
  /// - Precondition: `start`, `end`, `stride` must be scalar tensors.
  ///
  @_inlineable @inline(__always)
  init(rangeFrom start: Tensor, to end: Tensor, stride: Tensor) {
    self.init(handle:
      #tfop("Range", start.handle, end.handle, stride.handle,
            Tidx: Scalar.self))
  }

  /// Initialize a 1-D tensor representing a sequence from a starting value to,
  /// but not including, an end value, stepping by the specified amount.
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
    self.init(rangeFrom: Tensor(start), to: Tensor(end), stride: Tensor(stride))
  }
}

//===----------------------------------------------------------------------===//
// Factory methods for floating point tensors
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : FloatingPoint {
  // def tf.random_normal(shape, mean=0.0, stddev=1.0, dtype=dtypes.float32,
  //                      seed=None, name=None):
  @inline(never) // make @_inlineable when implemented.
  static func randomNormal(
    shape: [Int], mean: Double = 0, stddev: Double = 1
  ) -> Tensor {
    fatalError("FIXME: implement randomNormal")
  }
}

//===----------------------------------------------------------------------===//
// Shape transformations
//===----------------------------------------------------------------------===//

public extension AccelerableByTensorFlow {
  /// Broadcast the specified scalar value to be a tensor with the same rank as
  /// the specified other tensor, but with dimension=1 for each rank.
  @inline(never) // make @_inlineable when implemented.
  func broadcast(toRank rank: Int) -> Tensor<Self> {
    fatalError("FIXME: implement scalar broadcast")
  }
}

public extension Tensor {
  /// Returns a rank-lifted Tensor with a leading dimension of 1.
  @_inlineable @inline(__always)
  func rankLifted() -> Tensor {
    return shapePadded(atIndex: 0)
  }

  /// Returns a shape-padded Tensor, inserting a dimension of 1 at a given
  /// index.
  @_inlineable @inline(__always)
  func shapePadded(atIndex index: Int) -> Tensor {
    return Tensor(handle:
      #tfop("ExpandDims", handle, Tensor<Int>(index).handle, Tdim: Int.self))
  }

  /// Broadcast the specified Tensor to a rank greater than or equal to its
  /// one, filling in the new dimensions with size 1.
  // FIXME: this function is ambiguous about how dimensions are filled in. Is
  // it the leading or the trailing dimensions that are filled with 1? Consider
  // removing.
  @inline(never)
  func broadcast(toRank rank: Int) -> Tensor {
    fatalError("FIXME: implement broadcast")
  }

  /// Broadcast to the same shape as the specified Tensor.
  @inline(never) // make @_inlineable when implemented.
  func broadcast(to other: Tensor) -> Tensor {
    fatalError("FIXME: implement broadcast")
  }

  /// Reshape to the specified shape.
  /// - Precondition: The number of scalars matches the new shape.
  @_inlineable @inline(__always)
  func reshaped(_ newShape: [Int]) -> Tensor {
    return reshaped(Tensor<Int>(newShape))
  }

  /// Reshape to the specified Tensor representing a shape.
  /// - Precondition: The number of scalars matches the new shape.
  @_inlineable @inline(__always)
  func reshaped(_ newShape: Tensor<Int>) -> Tensor {
    return Tensor(handle: #tfop("Reshape", handle, newShape.handle))
  }

  /// Remove dimensions of size 1 from the shape of a tensor.
  @_inlineable @inline(__always)
  func squeezed() -> Tensor {
    return Tensor(handle: #tfop("Squeeze", handle))
  }

  /// Concatenates tensors along a dimension.
  // TODO: improve description when implemented.
  @inline(never) // make @_inlineable when implemented.
  func concatenated(with other: Tensor) -> Tensor {
    fatalError("FIXME: implement concatenated(with:)")
  }

  /// Reshape to scalar.
  /// - Precondition: The tensor has exactly one scalar.
  @_inlineable @inline(__always)
  func scalarized() -> Scalar {
#if false // FIXME: The partitioner needs to promote array literals.
    guard let scalar = reshaped([]).scalar else {
      preconditionFailure(
        "Only tensors with exactly one scalar can be scalarized.")
    }
#else
    // FIXME: This is the inefficient implementation. When the partitioner
    // can promote array literals, replace this with the implementation above.
    guard let scalar = array.scalar else {
      preconditionFailure(
        "Only tensors with exactly one scalar can be scalarized.")
    }
#endif
    return scalar
  }
}

//===----------------------------------------------------------------------===//
// Safe data type conversion
//===----------------------------------------------------------------------===//

public extension Tensor where Scalar : Numeric {
  @inline(never)
  init(_ other: Tensor<Bool>) {
    fatalError("FIXME: implement boolean conversion")
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
// Description and visualization
//===----------------------------------------------------------------------===//

/// Make "print(someTensor)" print a pretty form of the tensor.
extension Tensor : CustomStringConvertible {
  public var description: String {
    fatalError("Unimplemented")
  }
}

// Make Tensors show up nicely in the Xcode Playground results sidebar.
extension Tensor : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    fatalError("Unimplemented")
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
      // This is considered to be a well known way to produce a copy to the host,
      // so we never want to produce an "implicit copy to host" warning.
      return toHost().handle.makeHostCopy()
    }
  }

  @_inlineable
  var scalars: [Scalar] {
    return array.scalars
  }
}
