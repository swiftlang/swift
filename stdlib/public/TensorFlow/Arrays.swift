//===-- Arrays.swift ------------------------------------------*- swift -*-===//
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

import Swift
import CTensorFlow

//===----------------------------------------------------------------------===//
// TensorBuffer
//===----------------------------------------------------------------------===//

/// `TensorBuffer` is the internal storage of ShapedArray. This buffer has
/// two modes of storage: 'native' and 'tensorFlow'. In 'native' mode, the
/// buffer object stores a pointer to contiguous scalars; in 'tensorFlow'
/// mode, the buffer object stores a `TF_Tensor*` and bridges to TensorFlow.
/// In either mode, the buffer object owns the memory and will deallocate it
/// on `deinit`.
fileprivate final class TensorBuffer<Scalar> {
  typealias Shape = [Int]

  enum Allocation {
    case native(UnsafeMutablePointer<Scalar>)
    case tensorFlow(CTensor)
  }

  let allocation: Allocation
  private let bufferPointer: UnsafeMutableBufferPointer<Scalar>

  deinit {
    debugLog("De-initializing tensor buffer.")
    switch allocation {
    case let .native(pointer):
      debugLog("Deallocating underlying buffer.")
      pointer.deinitialize(count: count)
      pointer.deallocate()
    case let .tensorFlow(cTensor):
      debugLog("Deleting underlying tensor.")
      TF_DeleteTensor(cTensor)
    }
    debugLog("Returning from deinit of TensorBuffer.")
  }

  init(allocation: Allocation, count: Int) {
    self.allocation = allocation
    // Initialize `bufferPointer`.
    switch allocation {
    case let .native(ptr):
      debugLog("Initializing TensorBuffer with a native buffer.")
      bufferPointer = UnsafeMutableBufferPointer(start: ptr, count: count)
    case let .tensorFlow(cTensor):
      debugLog("Initializing TensorBuffer with a cTensor.")
      let startAddress = TF_TensorData(cTensor)
        .assumingMemoryBound(to: Scalar.self)
      bufferPointer = UnsafeMutableBufferPointer(
        start: startAddress, count: count)
    }
  }

  convenience init(owning startAddress: UnsafeMutablePointer<Scalar>,
                   count: Int) {
    self.init(allocation: .native(startAddress), count: count)
  }
}

/// TF Tensor-specific initializer
extension TensorBuffer where Scalar : AccelerableByTensorFlow {
  /// Initialize a local tensor buffer from a C `TF_Tensor*` value and takes
  /// ownership of the value.
  convenience init(owning cTensor: CTensor, count: Int) {
    debugLog("Initializing TensorBuffer with a cTensor of \(count) elements.")
    let actualCount = (0..<TF_NumDims(cTensor)).reduce(1) { acc, next in
      acc * Int(TF_Dim(cTensor, next))
    }
    assert(actualCount == count)
    self.init(allocation: .tensorFlow(cTensor), count: count)
  }
}

/// Factory methods
extension TensorBuffer {
  static func createUninitialized(count: Int) -> TensorBuffer<Scalar> {
    let pointer = UnsafeMutablePointer<Scalar>.allocate(capacity: count)
    return TensorBuffer(owning: pointer, count: count)
  }
}

/// Properties
extension TensorBuffer {
  var count: Int {
    return bufferPointer.count
  }
}

/// Unsafe address accessor
extension TensorBuffer {
  func withUnsafeMutablePointerToScalars<R>(
    _ body: (UnsafeMutablePointer<Scalar>) throws -> R
  ) rethrows -> R {
    // `baseAddress` is guaranteed non-nil
    return try body(bufferPointer.baseAddress!)
  }

  func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Scalar>) throws -> R
  ) rethrows -> R {
    var bufferPointer = self.bufferPointer
    return try body(&bufferPointer)
  }
}

//===----------------------------------------------------------------------===//
// ShapedArrayProtocol, the protocol that all shaped array types, including
// ShapedArray, Array, Array2D, etc, will conform to.
//===----------------------------------------------------------------------===//

public protocol _ShapedArrayProtocol
  : RandomAccessCollection, MutableCollection {
  associatedtype Scalar
  associatedtype Shape

  /// Number of dimensions in this array.
  var rank: Int { get }
  /// Shape of this array.
  var shape: Shape { get }
  /// The total number of scalars in this array.
  var scalarCount: Int { get }

  /// Initialize an array using specific shape and contiguous scalars in
  /// row-major order.
  /// - Precondition: The number of `scalars` must be equal to the product of
  ///   all dimensions of the shape.
  init(shape: Shape, scalars: [Scalar])

  /// Initialize an array using specific shape and a sequence of scalars in
  /// row-major order.
  /// - Precondition: The number of `scalars` must be equal to the product of
  ///   all dimensions of the shape.
  init<S : Sequence>(shape: Shape, scalars: S) where S.Element == Scalar

  /// Calls a closure with a pointer to the array’s contiguous storage.
  /// - Parameter body: A closure with an UnsafeBufferPointer parameter that
  ///   points to the contiguous storage for the array. If no such storage
  ///   exists, it is created. If body has a return value, that value is also
  ///   used as the return value for the withUnsafeBufferPointer(_:) method. The
  ///   pointer argument is valid only for the duration of the method’s
  ///   execution.
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Scalar>) throws -> R
  ) rethrows -> R

  /// Calls the given closure with a pointer to the array’s mutable contiguous
  /// storage.
  /// - Parameter body: A closure with an UnsafeMutableBufferPointer parameter
  ///   that points to the contiguous storage for the array. If no such storage
  ///   exists, it is created. If body has a return value, that value is also
  ///   used as the return value for the withUnsafeMutableBufferPointer(_:)
  ///   method. The pointer argument is valid only for the duration of the
  ///   method’s execution.
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Scalar>) throws -> R
  ) rethrows -> R
}

public extension _ShapedArrayProtocol {
  /// The scalars of the shaped array in row-major order.
  var scalars: [Scalar] {
    get {
      return withUnsafeBufferPointer(Array.init)
    }
    set {
      precondition(newValue.count == scalarCount, "Scalar count mismatch")
      withUnsafeMutableBufferPointer { ptr in
        ptr.baseAddress!.assign(from: newValue, count: newValue.count)
      }
    }
  }

  /// Returns true if the ShapedArray has rank 0.
  var isScalar: Bool {
    return rank == 0
  }

  /// Returns the underlying scalar from a 0-ranked ShapedArray.
  /// - precondition: ShapedArray is 0-ranked.
  var scalar: Scalar? {
    guard rank == 0 else { return nil }
    return scalars.first
  }
}

public extension _ShapedArrayProtocol
  where Scalar : Equatable, Shape : Equatable {
  static func == <Other>(lhs: Self, rhs: Other) -> Bool
    where Shape == Other.Shape,
          Other : _ShapedArrayProtocol,
          Scalar == Other.Scalar {
    return lhs.shape == rhs.shape &&
      lhs.scalars.elementsEqual(rhs.scalars)
  }
}

public extension _ShapedArrayProtocol
  where Shape : Collection, Shape.Element == Int {
  /// Returns the number of element tensors in a ShapedArray (equivalent to the
  /// first dimension).
  /// - Note: `count` is distinct from `scalarCount`, which represents the total
  ///   number of scalars.
  var count: Int {
    return shape.first ?? 0
  }
}

fileprivate extension _ShapedArrayProtocol
  where Shape : Collection, Shape.Element == Int {
  /// Returns the scalar count for an element in a ShapedArray.
  var scalarCountPerElement: Int {
    return shape.isEmpty ? 0 : shape.dropFirst().reduce(1, *)
  }

  /// Returns the scalar index corresponding to an index in the leading dimension
  /// of a ShapedArray.
  func scalarIndex(fromIndex index: Int) -> Int {
    return scalarCountPerElement * index
  }

  /// Returns the range of scalars corresponding to a range in the leading
  /// dimension of a ShapedArray.
  func scalarSubrange(
    from tensorSubrange: CountableRange<Int>
  ) -> CountableRange<Int> {
    return scalarIndex(fromIndex: tensorSubrange.lowerBound)
      ..< scalarIndex(fromIndex: tensorSubrange.upperBound)
  }
}

/// Common public protocol implementations
fileprivate extension _ShapedArrayProtocol
  where Element : _ShapedArrayProtocol {
  var _description: String {
    if let scalar = scalar {
      return String(describing: scalar)
    }
    return "[\( map({"\($0)"}).joined(separator: ", ") )]"
  }
}

fileprivate extension _ShapedArrayProtocol
  where Shape : Equatable, Scalar : Equatable {
  func _isEqual(to other: Self) -> Bool {
    return shape == other.shape && withUnsafeBufferPointer { selfBuf in
      other.withUnsafeBufferPointer { otherBuf in
        selfBuf.elementsEqual(otherBuf)
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// ShapedArray
//===----------------------------------------------------------------------===//

/// `ShapedArray` is a representation of a multidimensional array. It has a
/// shape, which has type `[Int]` and defines the array dimensions, and uses
/// `TensorBuffer` internally as storage.
public struct ShapedArray<Scalar> : _ShapedArrayProtocol {
  public typealias Shape = [Int]

  /// Contiguous memory storing scalars.
  fileprivate var buffer: TensorBuffer<Scalar>

  /// The shape of this array.
  public private(set) var shape: [Int]

  /// Initialize a shaped array from an existing buffer and a shape.
  fileprivate init(buffer: TensorBuffer<Scalar>, shape: [Int]) {
    precondition(buffer.count == shape.reduce(1, *))
    self.buffer = buffer
    self.shape = shape
    debugLog("Done Init array with buffer: \(self).")
    // debugLog("Done Init array")
  }
}

internal extension ShapedArray {
  mutating func ensureUniquelyReferenced() {
    if isKnownUniquelyReferenced(&buffer) { return }
    let oldBuffer = buffer
    buffer = TensorBuffer.createUninitialized(count: scalarCount)
    oldBuffer.withUnsafeMutablePointerToScalars { oldPtr in
      buffer.withUnsafeMutablePointerToScalars { ptr in
        ptr.initialize(from: oldPtr, count: scalarCount)
      }
    }
  }
}

internal extension ShapedArray where Scalar : AccelerableByTensorFlow {
  @_versioned
  init(owning cTensor: CTensor) {
    // Including \(Scalar.self) into the message would cause non-deterministic
    // crashes.
    debugLog("Initializing ShapedArray from CTensor.")
    shape = (0..<TF_NumDims(cTensor)).map { Int(TF_Dim(cTensor, $0)) }
    if _RuntimeConfig.printsDebugLog {
      // Without this local variable, passing the string directly into
      // debugLog() would not work, because 'self' is captured by the auto
      // closure param in debugLog().
      let shapeStr = "The shape is \(shape)."
      debugLog(shapeStr)
    }
    buffer = TensorBuffer(owning: cTensor, count: shape.reduce(1, *))
    debugLog("Done Init array.")
  }
}

public extension ShapedArray {
  var rank: Int {
    return shape.count
  }

  var scalarCount: Int {
    return buffer.count
  }

  init(_ other: ShapedArray) {
    debugLog("Initializing from another array.")
    self.init(buffer: other.buffer, shape: other.shape)
  }

  init(shape: [Int], scalars: [Scalar]) {
    let scalarCount = shape.reduce(1, *)
    precondition(scalarCount == scalars.count)
    let buffer = TensorBuffer<Scalar>.createUninitialized(count: scalarCount)
    self.init(buffer: buffer, shape: shape)
    buffer.withUnsafeMutablePointerToScalars { ptr in
      scalars.withUnsafeBufferPointer { arrayBuf in
        ptr.initialize(from: arrayBuf.baseAddress!, count: scalarCount)
      }
    }
  }

  init<S : Sequence>(shape: [Int], scalars: S) where S.Element == Scalar {
    let scalarCount = shape.reduce(1, *)
    let buffer = TensorBuffer<Scalar>.createUninitialized(count: scalarCount)
    self.init(buffer: buffer, shape: shape)
    buffer.withUnsafeMutablePointerToScalars { ptr in
      // TODO: Refactor with better pointer initializers in Swift 4.1.
      var i = 0
      for scalar in scalars {
        guard i < scalarCount else { break }
        ptr.advanced(by: i).initialize(to: scalar)
        i += 1
      }
      // If the sequence has fewer elements than the shape needs, this is a
      // precondition failure.
      precondition(i == scalarCount,
                   "The sequence has fewer elements than needed by the shape.")
    }
  }

  /// Initialize a scalar tensor.
  init(_ scalar: Scalar) {
    let buffer = TensorBuffer<Scalar>.createUninitialized(count: 1)
    self.init(buffer: buffer, shape: [])
    buffer.withUnsafeMutablePointerToScalars { ptr in
      ptr.initialize(to: scalar)
    }
  }

  /// Allocate and initialize a tensor to a repeated value.
  /// - Parameter shape: tensor shape
  /// - Parameter repeating: repeated value
  init(shape: Shape, repeating repeatedValue: Scalar) {
    let scalarCount = shape.reduce(1, *)
    let buffer = TensorBuffer<Scalar>.createUninitialized(count: scalarCount)
    self.init(buffer: buffer, shape: shape)
    buffer.withUnsafeMutablePointerToScalars { ptr in
      ptr.initialize(repeating: repeatedValue, count: scalarCount)
    }
  }
}

extension ShapedArray : RandomAccessCollection, MutableCollection {
  public typealias Index = Int
  public typealias Element = ShapedArraySlice<Scalar>
  public typealias SubSequence = ShapedArraySlice<Scalar>

  public var indices: CountableRange<Int> {
    return 0..<count
  }

  public var startIndex: Int {
    return 0
  }

  public var endIndex: Int {
    return count
  }

  /// Access the element tensor specified by an index in the leading dimension.
  /// - Parameter index: index of the element tensor
  public subscript(index: Int) -> Element {
    get {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(index < endIndex, "ShapedArray index is out of range")
      precondition(index >= startIndex,
                   "Negative ShapedArray index is out of range")
      return ShapedArraySlice(base: self, baseIndices: [index])
    }
    set {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(index < endIndex, "ShapedArray index is out of range")
      precondition(index >= startIndex,
                   "Negative ShapedArray index is out of range")
      precondition(shape.dropFirst().elementsEqual(newValue.shape),
                   "Element shape mismatch")
      let scalarIndex = self.scalarIndex(fromIndex: index)
      withUnsafeMutableBufferPointer { destBuffPtr in
        let ptr = destBuffPtr.baseAddress!.advanced(by: scalarIndex)
        newValue.withUnsafeBufferPointer { srcBuffPtr in
          ptr.assign(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
        }
      }
    }
  }

  /// Access the subtensor specified by a contiguous range of indices.
  /// - Parameter bounds: contiguous range of indices
  public subscript(bounds: Range<Int>) -> SubSequence {
    get {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArray indices are out of range")
      return ShapedArraySlice(base: self, bounds: CountableRange(bounds))
    }
    set {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArray indices are out of range")
      let subTensorShape = [bounds.count] + shape.dropFirst()
      precondition(subTensorShape == newValue.shape, "Subtensor shape mismatch")
      let scalarIndex = self.scalarIndex(fromIndex: bounds.lowerBound)
      withUnsafeMutableBufferPointer { destBuffPtr in
        let ptr = destBuffPtr.baseAddress!.advanced(by: scalarIndex)
        newValue.withUnsafeBufferPointer { srcBuffPtr in
          ptr.assign(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
        }
      }
    }
  }
}

public extension ShapedArray {
  func withUnsafeBufferPointer<Result>(
    _ body: (UnsafeBufferPointer<Scalar>) throws -> Result
  ) rethrows -> Result {
    return try buffer.withUnsafeMutableBufferPointer { ptr in
      try body(UnsafeBufferPointer(ptr))
    }
  }

  mutating func withUnsafeMutableBufferPointer<Result>(
    _ body: (inout UnsafeMutableBufferPointer<Scalar>) throws -> Result
  ) rethrows -> Result {
    ensureUniquelyReferenced()
    return try buffer.withUnsafeMutableBufferPointer { ptr in
      try body(&ptr)
    }
  }
}

/// Tensor conversion
extension ShapedArray where Scalar : AccelerableByTensorFlow {
  var byteCount: Int {
    return MemoryLayout<Scalar>.stride * scalarCount
  }

  func makeTensorHandle() -> TensorHandle<Scalar> {
    // This initializer is designed to optimize conversion from TF-allocated
    // `ShapedArray` instances.
    switch buffer.allocation {
    case let .native(bufAddr):
      return TensorHandle<Scalar>(
        shape: shape.map(Int32.init),
        scalarsInitializer: { addr in
          addr.initialize(from: bufAddr, count: scalarCount)
        }
      )

    case let .tensorFlow(cTensor):
      return TensorHandle(copyingFromCTensor: cTensor)
    }
  }
}

/// Tensor conversion
public extension Tensor where Scalar : AccelerableByTensorFlow {
  init(_ other: ShapedArray<Scalar>) {
    self.init(other.makeTensorHandle())
  }
}

/// Equatable conformance
extension ShapedArray : Equatable where Scalar : Equatable {
  static public func == (lhs: ShapedArray, rhs: ShapedArray) -> Bool {
    return lhs._isEqual(to: rhs)
  }
}

extension ShapedArray : CustomStringConvertible {
  /// A textual representation of this shaped array.
  public var description: String {
    return _description
  }
}

//===----------------------------------------------------------------------===//
// ShapedArraySlice
//===----------------------------------------------------------------------===//

/// A contiguous slice of a `ShapedArray` or `ShapedArraySlice` instance.
///
/// `ShapedArraySlice` enables fast, efficient operations on contiguous slices
/// of `ShapedArray` instances. `ShapedArraySlice` instances do not have their
/// own storage. Instead, they provides a view onto the storage of their base
/// `ShapedArray`. `ShapedArraySlice` can represent two different kinds of
/// slices: element tensors and subtensors.
///
/// Element tensors are subdimensional elements of a `ShapedArray`: their rank
/// is one less than that of their base. Element tensor slices are obtained by
/// indexing a `ShapedArray` instance with a singular `Int` index.
///
/// For example:
///
///     let matrix = ShapedArray(shape: [2, 2], scalars: [0, 1, 2, 3])
///     // `matrix` represents [[0, 1], [2, 3]].
///
///     let element = matrix[0]
///     // `element` is a `ShapedArraySlice` with shape [2]. It is an element
///     // tensor, specifically the first element in `matrix`: [0, 1].
///
///     matrix[1] = ShapedArraySlice(shape: [2], scalars: [4, 8])
///     // The second element in `matrix` has been mutated.
///     // `matrix` now represents [[0, 1, 4, 8]].
///
/// Subtensors are a contiguous range of the elements in a `ShapedArray`.
/// The rank of a subtensor is the same as that of its base, but its leading
/// dimension may be smaller. Subtensor slices are obtained by indexing a
/// `ShapedArray` with a `Range<Int>` that represents a range of elements (in
/// the leading dimension). Methods like `prefix(:)` and `suffix(:)` that
/// internally index with a range also produce subtensors.
///
/// For example:
///
///     let zeros = ShapedArray(shape: [3, 2], repeating: 0)
///     var matrix = ShapedArray(shape: [3, 2], scalars: Array(0..<6))
///     // `zeros` represents [[0, 0], [0, 0], [0, 0]].
///     // `matrix` represents [[0, 1], [2, 3], [4, 5]].
///
///     let subtensor = matrix.prefix(2)
///     // `subtensor` is a `ShapedArraySlice` with shape [2, 2]. It is a slice
///     // of the first 2 elements in `matrix` and represents [[0, 1], [2, 3]].
///
///     matrix[0..<2] = zeros.prefix(2)
///     // The first 2 elements in `matrix` have been mutated.
///     // `matrix` now represents [[0, 0], [0, 0], [4, 5]].

public struct ShapedArraySlice<Scalar> : _ShapedArrayProtocol {
  public typealias Shape = [Int]

  /// The underlying `ShapedArray` of a slice.
  private var base: ShapedArray<Scalar>
  /// The subdimensional indices of a slice.
  private var baseIndices: [Int]
  /// The subtensor bounds of a slice.
  private var bounds: CountableRange<Int>?

  /// Initialize a shaped array slice from a shaped array as base, with
  /// specified subdimensional inwards indices and subtensor bounds.
  fileprivate init(
    base: ShapedArray<Scalar>,
    baseIndices indices: [Int] = [],
    bounds: CountableRange<Int>? = nil
  ) {
    precondition(indices.count <= base.rank,
                 "Number of base indices exceeds base rank")
    precondition(zip(base.shape, indices).forAll { $1 >= 0 && $1 < $0 },
                 "Base indices are out of range")
    self.base = base
    self.baseIndices = indices
    self.bounds = bounds
  }
}

public extension ShapedArraySlice {
  /// Indexing depth of this slice, i.e. the difference in rank between the base
  /// and the slice.
  private var indexingDepth: Int {
    return baseIndices.count
  }

  var rank: Int {
    return base.rank - indexingDepth
  }

  var shape: Shape {
    if let bounds = bounds {
      return [bounds.count] + Array(base.shape.dropFirst(indexingDepth + 1))
    }
    return Array(base.shape.dropFirst(indexingDepth))
  }

  var scalarCount: Int {
    return shape.reduce(1, *)
  }
}

/// Slice initializers
public extension ShapedArraySlice {
  init(shape: [Int], scalars: [Scalar]) {
    self.init(base: ShapedArray(shape: shape, scalars: scalars))
  }

  init<S : Sequence>(shape: [Int], scalars: S) where S.Element == Scalar {
    self.init(base: ShapedArray(shape: shape, scalars: scalars))
  }

  /// Initialize a scalar tensor.
  init(_ scalar: Scalar) {
    self.init(base: ShapedArray(scalar))
  }

  /// Allocate and initialize a tensor to a repeated value.
  /// - Parameter shape: tensor shape
  /// - Parameter repeating: repeated value
  init(shape: Shape, repeating repeatedValue: Scalar) {
    self.init(base: ShapedArray(shape: shape, repeating: repeatedValue))
  }
}

private extension ShapedArraySlice {
  /// The range of scalars from the base ShapedArray represented by a
  /// ShapedArraySlice.
  var scalarRange: CountableRange<Int> {
    let trimmedShape = base.shape.dropFirst()
    var (start, end) = baseIndices.enumerated()
      .reduce((0, base.scalarCount)) { (acc, next) in
      let stride = trimmedShape.dropFirst(next.offset).reduce(1, *)
      if next.offset == indexingDepth - 1 {
        let temp = acc.0 + next.element * stride
        return (temp, temp + stride)
      }
      return (acc.0 + next.element * stride, acc.1)
    }
    if let bounds = bounds {
      let stride = trimmedShape.dropFirst(indexingDepth).reduce(1, *)
      let oldStart = start
      start = start + bounds.startIndex * stride
      end = oldStart + bounds.endIndex * stride
    }
    return start..<end
  }
}

public extension ShapedArraySlice {
  func withUnsafeBufferPointer<Result>(
    _ body: (UnsafeBufferPointer<Scalar>) throws -> Result
  ) rethrows -> Result {
    return try base.buffer.withUnsafeMutablePointerToScalars { basePtr in
      let ptr = UnsafeBufferPointer(
        start: basePtr.advanced(by: scalarRange.startIndex),
        count: scalarRange.count)
      return try body(ptr)
    }
  }

  mutating func withUnsafeMutableBufferPointer<Result>(
    _ body: (inout UnsafeMutableBufferPointer<Scalar>) throws -> Result
  ) rethrows -> Result {
    return try base.buffer.withUnsafeMutablePointerToScalars { basePtr in
      var ptr = UnsafeMutableBufferPointer(
        start: basePtr.advanced(by: scalarRange.startIndex),
        count: scalarRange.count)
      return try body(&ptr)
    }
  }
}

extension ShapedArraySlice : RandomAccessCollection, MutableCollection {
  public typealias Index = Int
  public typealias Element = ShapedArraySlice
  public typealias SubSequence = ShapedArraySlice

  public var indices: CountableRange<Int> {
    if let bounds = bounds {
      return bounds
    } else if indexingDepth < base.rank {
      return 0..<base.shape[indexingDepth]
    }
    return 0..<0
  }

  public var startIndex: Int {
    return indices.startIndex
  }

  public var endIndex: Int {
    return indices.endIndex
  }

  /// Access the element tensor specified by an index in the leading dimension.
  /// - Parameter index: index of the element tensor
  public subscript(index: Int) -> Element {
    get {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(index < endIndex, "ShapedArraySlice index is out of range")
      precondition(index >= startIndex,
                   "ShapeArraySlice index is out of range (before startIndex)")
      return ShapedArraySlice(base: base,
                              baseIndices: baseIndices + [index],
                              bounds: bounds)
    }
    set {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(index < endIndex, "ShapedArraySlice index is out of range")
      precondition(index >= startIndex,
                   "ShapeArraySlice index is out of range (before startIndex)")
      precondition(shape.dropFirst().elementsEqual(newValue.shape),
                   "Element shape mismatch")
      let scalarIndex = self.scalarIndex(fromIndex: index)
      withUnsafeMutableBufferPointer { destBuffPtr in
        let ptr = destBuffPtr.baseAddress!.advanced(by: scalarIndex)
        newValue.withUnsafeBufferPointer { srcBuffPtr in
          ptr.assign(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
        }
      }
    }
  }

  /// Access the subtensor specified by a contiguous range of indices.
  /// - Parameter bounds: contiguous range of indices
  public subscript(bounds: Range<Int>) -> SubSequence {
    get {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArraySlice indices are out of range")
      return ShapedArraySlice(base: base,
                              baseIndices: baseIndices,
                              bounds: CountableRange(bounds))
    }
    set {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArraySlice indices are out of range")
      let subTensorShape = [bounds.count] + shape.dropFirst()
      precondition(subTensorShape == newValue.shape, "Subtensor shape mismatch")
      let scalarIndex = self.scalarIndex(fromIndex: bounds.lowerBound)
      withUnsafeMutableBufferPointer { destBuffPtr in
        let ptr = destBuffPtr.baseAddress!.advanced(by: scalarIndex)
        newValue.withUnsafeBufferPointer { srcBuffPtr in
          ptr.assign(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
        }
      }
    }
  }
}

/// Tensor conversion
public extension ShapedArraySlice where Scalar : AccelerableByTensorFlow {
  init(_ other: Tensor<Scalar>) {
    self.init(base: other.array)
  }
}

/// Equatable conformance
extension ShapedArraySlice : Equatable where Scalar : Equatable {
  static public func == (lhs: ShapedArraySlice, rhs: ShapedArraySlice) -> Bool {
    return lhs._isEqual(to: rhs)
  }
}

/// String conversion
extension ShapedArraySlice : CustomStringConvertible {
  /// A textual representation of this shaped array.
  public var description: String {
    return _description
  }
}
