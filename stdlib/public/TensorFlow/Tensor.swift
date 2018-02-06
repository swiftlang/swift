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

public struct Tensor<Unit : AccelerableTensorUnit> {
  /// A tensor just contains a TensorHandle under the covers.  This is public to
  /// allow user defined ops, but shouldn't normally be used otherwise.
  public let handle: TensorHandle<Unit>

  @_inlineable
  public init(_ handle: TensorHandle<Unit>) {
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
func _TFSend<Unit>(_ handle: TensorHandle<Unit>) -> TensorHandle<Unit> {
  return handle
}

@_versioned @inline(never)
@_silgen_name("__tf_receive")
@effects(readnone)
func _TFReceive<Unit>(_ handle: TensorHandle<Unit>) -> TensorHandle<Unit> {
  return handle
}

@_versioned @inline(never)
@_silgen_name("__tf_scalarize")
func _TFScalarize<Unit>(_ handle: TensorHandle<Unit>) -> Unit? {
  return handle.makeHostCopy().scalar
}

/// This compiler builtin is known by the partitioning pass, which recognizes it
/// and promotes calls to it to being in graph when it can.  This signature was
/// designed to align with the requirements of the 'Const' Tensorflow operation.
@_versioned @inline(never)
@_silgen_name("__tf_tensor_from_units")
func _TFTensorFromUnits<Unit>(_ units: [Unit], shape: [Int])
    -> TensorHandle<Unit> {
  let contiguousSize = shape.reduce(1, *)
  precondition(units.count == contiguousSize,
               "The number of units doesn't match the shape.")
  return TensorHandle(
      shape: shape,
      unitsInitializer: { addr in
        units.withUnsafeBufferPointer { ptr in
          addr.assign(from: ptr.baseAddress!, count: contiguousSize)
        }
    })
}

@_versioned @inline(never)
@_silgen_name("__tf_tensor_from_units_1d")
func _TFTensorFromUnits1D<Unit>(_ units: [Unit]) -> TensorHandle<Unit> {
  return _TFTensorFromUnits(units, shape: [units.count])
}

//===----------------------------------------------------------------------===//
// Memory transfer markers
//===----------------------------------------------------------------------===//

/// TODO: Remove when send/receive semantics gets revisited.
public extension Tensor {
  @_inlineable @inline(__always)
  func toDevice() -> Tensor {
    return Tensor(_TFSend(handle))
  }

  @_inlineable @inline(__always)
  func toHost() -> Tensor {
    return Tensor(_TFReceive(handle))
  }
}

//===----------------------------------------------------------------------===//
// Initialization
//===----------------------------------------------------------------------===//

extension Tensor where Unit : Numeric {
  /// Perform an element conversion from Tensor<U> to Tensor<T>.
  @_inlineable @inline(__always)
  public init<FromType : Numeric>(_ other: Tensor<FromType>) {
    self.init(#tfop("Cast", "t:t", other.handle, DstT: Unit.self))
  }
}

public extension Tensor {
  /// Initialize a tensor with a unit representing a scalar value.
  @_inlineable @inline(__always)
  init(_ value: Unit) {
    self.init(#tfop("tfc.scalarToTensor", "s:t", value))
  }

  /// Initialize a tensor with an array representing a vector.
  @_inlineable @inline(__always)
  init(_ vector: [Unit]) {
    self.init(_TFTensorFromUnits1D(vector))
  }

  /// Initialize a tensor with an array of arrays representing a matrix.
  ///
  /// - Precondition: The number of elements in each sub-dimensional array in
  ///   the array must be equal.
  @_inlineable
  init(_ literal: [[Unit]]) {
    /// Sanity checks.
    let dim0 = literal.count
    let dim1 = literal.first?.count ?? 0
    for subArray in literal {
      precondition(subArray.count == dim1, """
        Each dimension must have an equal number of subdimensions.
        """)
    }
    // We don't want to delegate initialization to `init(shape:units:)`
    // because flattening `matrix` to an array is an unnecessary cost.
    let tensorHandle = TensorHandle<Unit>(
      shape: [dim0, dim1],
      unitsInitializer: { addr in
        // Copy to TF_Tensor memory, one row at a time.
        for (i, subArray) in literal.enumerated() {
          subArray.withUnsafeBufferPointer { ptr in
            addr.advanced(by: i * dim1)
              .assign(from: ptr.baseAddress!, count: dim1)
          }
        }
      }
    )
    self.init(tensorHandle)
  }

  /// Initialize a tensor with an array of arrays of arrays representing a
  /// 3D tensor.
  ///
  /// - Precondition: The number of elements in each sub-dimensional array in
  ///   the array must be equal.
  /// - TODO: improve description
  @_inlineable
  init(_ literal: [[[Unit]]]) {
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
    // We don't want to delegate initialization to `init(shape:units:)`
    // because flattening `literal` to an array is an unnecessary cost.
    let tensorHandle = TensorHandle<Unit>(
      shape: [dim0, dim1, dim2],
      unitsInitializer: { addr in
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
    self.init(tensorHandle)
  }

  /// Initialize a tensor with an array of array of arrays of arrays
  /// representing a 4D tensor.
  ///
  /// - Precondition: The number of elements in each sub-dimensional array in
  ///   the array must be equal.
  /// - TODO: improve description
  @_inlineable
  init(_ literal: [[[[Unit]]]]) {
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
    // We don't want to delegate initialization to `init(shape:units:)`
    // because flattening `literal` to an array is an unnecessary cost.
    let tensorHandle = TensorHandle<Unit>(
      shape: [dim0, dim1, dim2, dim3],
      unitsInitializer: { addr in
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
    self.init(tensorHandle)
  }

  /// Initialize a tensor with arbitrary shape.
  /// - Precondition: The number of units should be the same as the
  ///   product of all of shape's dimensions.
  @_inlineable @inline(__always)
  init(shape: [Int], units: [Unit]) {
    self.init(_TFTensorFromUnits(units, shape: shape))
  }

  /// Initialize a tensor of a specified shape, filled with a single value.
  @_inlineable @inline(__always)
  init(shape: [Int], repeating repeatedValue: Unit) {
    let valueTensor = Tensor(repeatedValue).handle
    let shapeTensor = Tensor<Int32>(Tensor<Int>(shape)).handle
    self.init(#tfop("Fill", "tt:t", shapeTensor, valueTensor))
  }
}

//===----------------------------------------------------------------------===//
// Properties
//===----------------------------------------------------------------------===//

public extension Tensor {
  @_inlineable
  var shape: [Int] {
    // TODO: Until we have mandatory deabstraction, we need to force the
    // performance inliner to inline this, even though it is apparently too
    // big to want to do so for performance reasons.
    @inline(__always)
    get {
      return shapeTensor.units
    }
  }

  @_inlineable
  var rank: Int {
    @inline(__always)
    get {
      return rankTensor.scalar!
    }
  }

  @_inlineable
  var unitCount: Int {
    @inline(__always)
    get {
      return unitCountTensor.scalar!
    }
  }
}

//===----------------------------------------------------------------------===//
// Factory initializers for numeric tensors
//===----------------------------------------------------------------------===//

public extension Tensor where Unit : Numeric {
  /// Zero initializer, takes a list of dimensions.
  @_inlineable @inline(__always)
  static func zeros(shape: [Int]) -> Tensor {
    return Tensor(shape: shape, repeating: 0)
  }

  /// Zero initializer, takes variadic dimensions.
  @_inlineable @inline(__always)
  static func zeros(shape: Int...) -> Tensor {
    return zeros(shape: shape)
  }

  /// Ones initializer, takes a list of dimensions.
  @_inlineable @inline(__always)
  static func ones(shape: [Int]) -> Tensor {
    return Tensor(shape: shape, repeating: 1)
  }

  /// Ones initializer, takes variadic dimensions.
  @_inlineable @inline(__always)
  static func ones(shape: Int...) -> Tensor {
    return ones(shape: shape)
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
    self.init(#tfop("Range", "ttt:t", start.handle, end.handle, stride.handle,
                    Tidx: Unit.self))
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
  init(rangeFrom start: Unit, to end: Unit, stride: Unit) {
    self.init(rangeFrom: Tensor(start), to: Tensor(end), stride: Tensor(stride))
  }
}

//===----------------------------------------------------------------------===//
// Factory methods for floating point tensors
//===----------------------------------------------------------------------===//

public extension Tensor where Unit : FloatingPoint {
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

public extension AccelerableTensorUnit {
  /// Broadcast the specified scalar value to be a tensor with the same rank as
  /// the specified other tensor, but with dimension=1 for each rank.
  @inline(never) // make @_inlineable when implemented.
  func broadcast(toRank rank: Int) -> Tensor<Self> {
    fatalError("FIXME: implement scalar broadcast")
  }
}

public extension Tensor {
  @_inlineable @inline(__always)
  func rankLifted(by dimensionCount: Int) -> Tensor {
    return Tensor(#tfop("ExpandDims", "tt:t", handle,
                        Tensor<Int>(dimensionCount).handle, Tdim: Int.self))
  }

  /// Broadcast the specified Tensor to a rank >= its current size, filling in
  /// the new dimensions with rank = 1.
  @inline(never)
  func broadcast(toRank rank: Int) -> Tensor {
    fatalError("FIXME: implement broadcast")
  }

  /// Broadcast self tensor to the same shape as the specified one.
  @inline(never) // make @_inlineable when implemented.
  func broadcast(to other: Tensor) -> Tensor {
    fatalError("FIXME: implement broadcast")
  }

  /// Reshape to the specified shape.
  /// - Precondition: The number of units matches the new shape.
  @_inlineable @inline(__always)
  func reshaped(_ newShape: [Int]) -> Tensor {
    return reshaped(Tensor<Int>(newShape))
  }

  /// Reshape to scalar.
  /// - Precondition: The tensor has exactly one unit.
  @_inlineable @inline(__always)
  func scalarized() -> Unit {
#if false // FIXME: The partitioner needs to promote array literals.
    guard let scalar = reshaped([]).scalar else {
      preconditionFailure(
        "Only tensors with exactly one unit can be scalarized.")
    }
#else
    // FIXME: This is the inefficient implementation. When the partitioner
    // can promote array literals, replace this with the implementation above.
    guard let scalar = array.scalar else {
      preconditionFailure(
        "Only tensors with exactly one unit can be scalarized.")
    }
#endif
    return scalar
  }
}

//===----------------------------------------------------------------------===//
// Safe data type conversion
//===----------------------------------------------------------------------===//

public extension Tensor where Unit : Numeric {
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
  var scalar: Unit? {
    @inline(__always)
    get {
      return Unit(self)
    }
  }
}

public extension AccelerableTensorUnit {
  @_inlineable @inline(__always)
  init?(_ tensor: Tensor<Self>) {
    guard let scalar = _TFScalarize(tensor.handle) else {
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
  var array: ShapedArray<Unit> {
    @inline(__always)
    get {
      debugLog("Returning a host copy of array.")
      // This is considered to be a well known way to produce a copy to the host,
      // so we never want to produce an "implicit copy to host" warning.
      return toHost().handle.makeHostCopy()
    }
  }

  @_inlineable
  var units: [Unit] {
    return array.units
  }
}
