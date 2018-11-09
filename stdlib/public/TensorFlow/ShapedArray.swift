//===-- ShapedArray.swift -------------------------------------*- swift -*-===//
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

/// `TensorBuffer` is the internal storage of `ShapedArray`. This buffer has
/// two modes of storage: 'native' and 'tensorFlow'. In 'native' mode, the
/// buffer object stores a pointer to contiguous scalars; in 'tensorFlow'
/// mode, the buffer object stores a `TF_Tensor*` and bridges to TensorFlow.
/// In either mode, the buffer object owns the memory and will deallocate it
/// on `deinit`.
@_fixed_layout @usableFromInline
internal final class TensorBuffer<Scalar> {
  typealias Shape = [Int]

  /// A reference type wrapping a Swift Array.
  /// - Note: an Array is used as the native storage for `TensorBuffer`. To make
  /// in-place mutation possible when the array is stored in an enum value, the
  /// array must be wrapped in a reference type.
  @_fixed_layout @usableFromInline
  final class BoxedArray {
    var array: [Scalar]

    init(_ array: [Scalar]) {
      self.array = array
    }
  }

  enum Allocation {
    case native(BoxedArray)
    case tensorFlow(CTensor)
  }

  let allocation: Allocation
  let count: Int

  deinit {
    debugLog("De-initializing tensor buffer.")
    switch allocation {
    case .native:
      debugLog("Deallocating underlying buffer.")
    case let .tensorFlow(cTensor):
      debugLog("Deleting underlying tensor.")
      TF_DeleteTensor(cTensor)
    }
    debugLog("Returning from deinit of TensorBuffer.")
  }

  init(allocation: Allocation, count: Int) {
    self.allocation = allocation
    self.count = count
  }
}

/// TF Tensor-specific initializer
extension TensorBuffer where Scalar : _TensorFlowDataTypeCompatible {
  /// Creates a local tensor buffer from a C `TF_Tensor*` value and takes
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
  static func create(
    count: Int,
    withInitializer body: (UnsafeMutableBufferPointer<Scalar>) -> Void
  ) -> TensorBuffer<Scalar> {
    /// Since `Scalar` may be any generic type, it is not possible to construct
    /// an instance of `Scalar` directly for use with the
    /// `Array(repeating:count:)` initializer. The workaround here is to
    /// allocate a dummy `Scalar` pointer of size 1 and to use the pointee value
    /// as the `repeatedValue` of the initializer.
    let dummyPointer = UnsafeMutablePointer<Scalar>.allocate(capacity: 1)
    var array = Array(repeating: dummyPointer.move(), count: count)
    array.withUnsafeMutableBufferPointer { body($0) }
    dummyPointer.deallocate()
    return TensorBuffer(allocation: .native(BoxedArray(array)), count: count)
  }
}

/// Unsafe address accessor
extension TensorBuffer {
  func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Scalar>) throws -> R
  ) rethrows -> R {
    switch allocation {
    case let .native(box):
      return try box.array.withUnsafeMutableBufferPointer { bufferPtr in
        try body(&bufferPtr)
      }
    case let .tensorFlow(cTensor):
      let startAddress = TF_TensorData(cTensor)
        .assumingMemoryBound(to: Scalar.self)
      var bufferPointer = UnsafeMutableBufferPointer(
        start: startAddress, count: count
      )
      return try body(&bufferPointer)
    }
  }

  func withUnsafeBufferPointer<R>(
    _ body: (inout UnsafeBufferPointer<Scalar>) throws -> R
  ) rethrows -> R {
    switch allocation {
    case let .native(box):
      return try box.array.withUnsafeBufferPointer { bufferPtr in
        var bufferPtr = bufferPtr
        return try body(&bufferPtr)
      }
    case let .tensorFlow(cTensor):
      let startAddress = TF_TensorData(cTensor)
        .assumingMemoryBound(to: Scalar.self)
      var bufferPointer = UnsafeBufferPointer(
        start: startAddress, count: count
      )
      return try body(&bufferPointer)
    }
  }
}

//===----------------------------------------------------------------------===//
// ShapedArrayProtocol, the protocol unifying ShapedArray and ShapedArraySlice.
//===----------------------------------------------------------------------===//

public protocol _ShapedArrayProtocol
  : RandomAccessCollection, MutableCollection {
  associatedtype Scalar

  /// The number of dimensions of the array.
  var rank: Int { get }
  /// The dimensions of the array.
  var shape: [Int] { get }
  /// The total number of scalars in the array.
  var scalarCount: Int { get }

  /// Creates an array with the specified shape and contiguous scalars in
  /// row-major order.
  /// - Precondition: The number of scalars must equal the product of the
  ///   dimensions of the shape.
  init(shape: [Int], scalars: [Scalar])

  /// Creates an array with the specified shape and sequence of scalars in
  /// row-major order.
  /// - Precondition: The number of scalars must equal the product of the
  ///   dimensions of the shape.
  init<S : Sequence>(shape: [Int], scalars: S) where S.Element == Scalar

  /// Calls a closure with a pointer to the array’s contiguous storage.
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter that
  ///   points to the contiguous storage for the array. If no such storage
  ///   exists, it is created. If body has a return value, that value is also
  ///   used as the return value for the `withUnsafeBufferPointer(_:)` method.
  ///   The pointer argument is valid only for the duration of the method’s
  ///   execution.
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Scalar>) throws -> R
  ) rethrows -> R

  /// Calls the given closure with a pointer to the array’s mutable contiguous
  /// storage.
  /// - Parameter body: A closure with an `UnsafeMutableBufferPointer` parameter
  ///   that points to the contiguous storage for the array. If no such storage
  ///   exists, it is created. If body has a return value, that value is also
  ///   used as the return value for the `withUnsafeMutableBufferPointer(_:)`
  ///   method. The pointer argument is valid only for the duration of the
  ///   method’s execution.
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Scalar>) throws -> R
  ) rethrows -> R
}

public extension _ShapedArrayProtocol {
  /// The scalars of the array in row-major order.
  var scalars: [Scalar] {
    get {
      return withUnsafeBufferPointer(Array.init)
    }
    set {
      precondition(newValue.count == scalarCount, "Scalar count mismatch.")
      withUnsafeMutableBufferPointer { ptr in
        ptr.baseAddress!.initialize(from: newValue, count: newValue.count)
      }
    }
  }

  /// Returns `true` if the array has rank 0.
  var isScalar: Bool {
    return rank == 0
  }

  /// Returns the single scalar element if the array has rank 0 and `nil`
  /// otherwise.
  var scalar: Scalar? {
    guard rank == 0 else { return nil }
    return scalars.first
  }
}

public extension _ShapedArrayProtocol where Scalar : Equatable {
  static func == <Other>(lhs: Self, rhs: Other) -> Bool
    where Other : _ShapedArrayProtocol, Scalar == Other.Scalar {
    return lhs.shape == rhs.shape && lhs.scalars.elementsEqual(rhs.scalars)
  }
}

public extension _ShapedArrayProtocol {
  /// Returns the number of element arrays in an array (equivalent to the first
  /// dimension).
  /// - Note: `count` is distinct from `scalarCount`, which represents the total
  ///   number of scalars.
  var count: Int {
    return shape.first ?? 0
  }
}

internal extension _ShapedArrayProtocol {
  /// Returns the scalar count for an element of the array.
  var scalarCountPerElement: Int {
    return shape.isEmpty ? 0 : shape.dropFirst().reduce(1, *)
  }

  /// Returns the scalar index corresponding to an index in the leading
  /// dimension of the array.
  func scalarIndex(fromIndex index: Int) -> Int {
    return scalarCountPerElement * index
  }

  /// Returns the range of scalars corresponding to a range in the leading
  /// dimension of the array.
  func scalarSubrange(
    from arraySubrange: Range<Int>
  ) -> Range<Int> {
    return scalarIndex(fromIndex: arraySubrange.lowerBound)
      ..< scalarIndex(fromIndex: arraySubrange.upperBound)
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

fileprivate extension _ShapedArrayProtocol where Scalar : Equatable {
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

/// `ShapedArray` is a multi-dimensional array. It has a shape, which has type
/// `[Int]` and defines the array dimensions, and uses a `TensorBuffer`
/// internally as storage.
@_fixed_layout
public struct ShapedArray<Scalar> : _ShapedArrayProtocol {
  /// Contiguous memory storing scalars.
  internal var buffer: TensorBuffer<Scalar>

  /// The dimensions of the array.
  public private(set) var shape: [Int]

  /// Creates a `ShapedArray` from a `TensorBuffer` and a shape.
  internal init(buffer: TensorBuffer<Scalar>, shape: [Int]) {
    precondition(buffer.count == shape.reduce(1, *),
      "The scalar count of the buffer does not match the shape.")
    self.buffer = buffer
    self.shape = shape
    debugLog("Done initializing ShapedArray from TensorBuffer.")
  }
}

fileprivate extension ShapedArray {
  mutating func ensureUniquelyReferenced() {
    if isKnownUniquelyReferenced(&buffer) { return }
    let oldBuffer = buffer
    debugLog("Unique reference check")
    buffer = TensorBuffer.create(count: scalarCount) { buffPtr in
      let ptr = buffPtr.baseAddress!
      oldBuffer.withUnsafeBufferPointer { oldBuffPtr in
        let oldPtr = oldBuffPtr.baseAddress!
        ptr.initialize(from: oldPtr, count: scalarCount)
      }
    }
  }
}

internal extension ShapedArray where Scalar : _TensorFlowDataTypeCompatible {
  @usableFromInline
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
    debugLog("Done initializing ShapedArray from CTensor.")
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
    debugLog("Initializing from another ShapedArray.")
    self.init(buffer: other.buffer, shape: other.shape)
  }

  init(shape: [Int], scalars: [Scalar]) {
    let scalarCount = shape.reduce(1, *)
    precondition(scalarCount == scalars.count, "Scalar count mismatch.")
    let buffer = TensorBuffer<Scalar>.create(count: scalarCount) { buffPtr in
      let ptr = buffPtr.baseAddress!
      scalars.withUnsafeBufferPointer { arrayBuf in
        ptr.initialize(from: arrayBuf.baseAddress!, count: scalarCount)
      }
    }
    self.init(buffer: buffer, shape: shape)
  }

  init<S : Sequence>(shape: [Int], scalars: S) where S.Element == Scalar {
    let scalarCount = shape.reduce(1, *)
    let buffer = TensorBuffer<Scalar>.create(count: scalarCount) { buffPtr in
      let ptr = buffPtr.baseAddress!
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
    self.init(buffer: buffer, shape: shape)
  }

  /// Creates a `ShapedArray` from a scalar value.
  init(_ scalar: Scalar) {
    let buffer = TensorBuffer<Scalar>.create(count: 1) { buffPtr in
      let ptr = buffPtr.baseAddress!
      ptr.initialize(to: scalar)
    }
    self.init(buffer: buffer, shape: [])
  }

  /// Creates a `ShapedArray` with the specified shape and a single, repeated
  /// value.
  /// - Parameters:
  ///   - shape: The dimensions of the array.
  ///   - repeatedValue: The scalar value to repeat.
  init(shape: [Int], repeating repeatedValue: Scalar) {
    let scalarCount = shape.reduce(1, *)
    let buffer = TensorBuffer<Scalar>.create(count: scalarCount) { buffPtr in
      let ptr = buffPtr.baseAddress!
      ptr.initialize(repeating: repeatedValue, count: scalarCount)
    }
    self.init(buffer: buffer, shape: shape)
  }
}

extension ShapedArray : RandomAccessCollection, MutableCollection {
  public typealias Index = Int
  public typealias Element = ShapedArraySlice<Scalar>
  public typealias SubSequence = ShapedArraySlice<Scalar>

  public var indices: Range<Int> {
    return 0..<count
  }

  public var startIndex: Int {
    return 0
  }

  public var endIndex: Int {
    return count
  }

  /// Access the element array specified by an index in the leading dimension.
  /// - Parameter index: Index of the element array.
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
          ptr.initialize(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
        }
      }
    }
  }

  /// Access the subarray specified by a contiguous range of indices.
  /// - Parameter bounds: Contiguous range of indices.
  public subscript(bounds: Range<Int>) -> SubSequence {
    get {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArray indices are out of range")
      return ShapedArraySlice(base: self, bounds: bounds)
    }
    set {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted.")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArray indices are out of range")
      let subArrayShape = [bounds.count] + shape.dropFirst()
      precondition(subArrayShape == newValue.shape,
                   "Subarray shape mismatch.")
      let scalarIndex = self.scalarIndex(fromIndex: bounds.lowerBound)
      withUnsafeMutableBufferPointer { destBuffPtr in
        let ptr = destBuffPtr.baseAddress!.advanced(by: scalarIndex)
        newValue.withUnsafeBufferPointer { srcBuffPtr in
          ptr.initialize(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
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
extension ShapedArray where Scalar : TensorFlowScalar {
  var byteCount: Int {
    return MemoryLayout<Scalar>.stride * scalarCount
  }

  @usableFromInline
  func makeTensorHandle() -> TensorHandle<Scalar> {
    // This initializer is designed to optimize conversion from TF-allocated
    // `ShapedArray` instances.
    switch buffer.allocation {
    case let .native(box):
      precondition(rank <= Int32.max, """
        Conversion to TensorHandle is undefined when rank exceeds Int32.max.
        """)
      precondition(shape.forAll { $0 <= Int32.max }, """
        Conversion to TensorHandle is undefined when shape dimensions exceed \
        Int32.max.
        """)
      return TensorHandle<Scalar>(
        shape: shape.map(Int32.init),
        scalarsInitializer: { addr in
          addr.initialize(from: box.array, count: scalarCount)
        }
      )
    case let .tensorFlow(cTensor):
      return TensorHandle(copyingFromCTensor: cTensor)
    }
  }
}

/// Tensor conversion
public extension Tensor {
  init(_ array: ShapedArray<Scalar>) {
    self.init(handle: array.makeTensorHandle())
  }
}

/// Array literal conversion
extension ShapedArray : ExpressibleByArrayLiteral
  where Scalar : TensorFlowScalar {
  public typealias ArrayLiteralElement = TensorElementLiteral<Scalar>
  @inlinable @inline(__always)
  public init(arrayLiteral elements: TensorElementLiteral<Scalar>...) {
    self = Tensor<Scalar>(tensorElementLiterals: elements).array
  }
}

/// Equatable conformance
extension ShapedArray : Equatable where Scalar : Equatable {
  public static func == (lhs: ShapedArray, rhs: ShapedArray) -> Bool {
    return lhs._isEqual(to: rhs)
  }
}

/// String conversion
extension ShapedArray : CustomStringConvertible {
  /// A textual representation of this `ShapedArray`.
  public var description: String {
    return _description
  }
}

/// Xcode Playground display conversion.
extension ShapedArray : CustomPlaygroundDisplayConvertible {
  public var playgroundDescription: Any {
    return description
  }
}

/// Mirror representation, used by debugger/REPL.
extension ShapedArray : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: [], displayStyle: .struct)
  }
}

/// Codable conformance.
extension ShapedArray : Codable where Scalar : Codable {
  private enum CodingKeys: String, CodingKey {
    case shape
    case scalars
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    let shape = try container.decode([Int].self, forKey: .shape)
    let scalars = try container.decode([Scalar].self, forKey: .scalars)
    self.init(shape: shape, scalars: scalars)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(shape, forKey: .shape)
    try container.encode(scalars, forKey: .scalars)
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
/// slices: element arrays and subarrays.
///
/// Element arrays are subdimensional elements of a `ShapedArray`: their rank
/// is one less than that of their base. Element array slices are obtained by
/// indexing a `ShapedArray` instance with a singular `Int32` index.
///
/// For example:
///
///     var matrix = ShapedArray(shape: [2, 2], scalars: [0, 1, 2, 3])
///     // `matrix` represents [[0, 1], [2, 3]].
///
///     let element = matrix[0]
///     // `element` is a `ShapedArraySlice` with shape [2]. It is an element
///     // array, specifically the first element in `matrix`: [0, 1].
///
///     matrix[1] = ShapedArraySlice(shape: [2], scalars: [4, 8])
///     // The second element in `matrix` has been mutated.
///     // `matrix` now represents [[0, 1, 4, 8]].
///
/// Subarrays are a contiguous range of the elements in a `ShapedArray`.
/// The rank of a subarray is the same as that of its base, but its leading
/// dimension is the count of the slice range. Subarray slices are obtained by
/// indexing a `ShapedArray` with a `Range<Int32>` that represents a range of
/// elements (in the leading dimension). Methods like `prefix(:)` and
/// `suffix(:)` that internally index with a range also produce subarray.
///
/// For example:
///
///     let zeros = ShapedArray(shape: [3, 2], repeating: 0)
///     var matrix = ShapedArray(shape: [3, 2], scalars: Array(0..<6))
///     // `zeros` represents [[0, 0], [0, 0], [0, 0]].
///     // `matrix` represents [[0, 1], [2, 3], [4, 5]].
///
///     let subarray = matrix.prefix(2)
///     // `subarray` is a `ShapedArraySlice` with shape [2, 2]. It is a slice
///     // of the first 2 elements in `matrix` and represents [[0, 1], [2, 3]].
///
///     matrix[0..<2] = zeros.prefix(2)
///     // The first 2 elements in `matrix` have been mutated.
///     // `matrix` now represents [[0, 0], [0, 0], [4, 5]].

@_fixed_layout
public struct ShapedArraySlice<Scalar> : _ShapedArrayProtocol {
  /// The underlying `ShapedArray` of the slice.
  @usableFromInline internal var base: ShapedArray<Scalar>
  /// The subdimensional indices of a slice.
  @usableFromInline internal var baseIndices: [Int]
  /// The subarray bounds of a slice.
  @usableFromInline internal var bounds: Range<Int>?

  /// Creates a `ShapedArraySlice` from a base `ShapedArray`, with the specified
  /// subdimensional indices and subarray bounds.
  @inlinable
  internal init(
    base: ShapedArray<Scalar>,
    baseIndices indices: [Int] = [],
    bounds: Range<Int>? = nil
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
  internal var indexingDepth: Int {
    return baseIndices.count
  }

  var rank: Int {
    return base.rank - indexingDepth
  }

  var shape: [Int] {
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

  /// Creates a `ShapedArraySlice` from a scalar value.
  init(_ scalar: Scalar) {
    self.init(base: ShapedArray(scalar))
  }

  /// Creates a `ShapedArraySlice` with the specified shape and a single,
  /// repeated value.
  /// - Parameters:
  ///   - shape: The dimensions of the array.
  ///   - repeatedValue: The scalar value to repeat.
  init(shape: [Int], repeating repeatedValue: Scalar) {
    self.init(base: ShapedArray(shape: shape, repeating: repeatedValue))
  }
}

internal extension ShapedArraySlice {
  /// The range of scalars from the base `ShapedArray` represented by a
  /// `ShapedArraySlice`.
  var scalarRange: Range<Int> {
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
    return try base.withUnsafeBufferPointer { baseBuffPtr in
      let basePtr = baseBuffPtr.baseAddress!
      let ptr = UnsafeBufferPointer(
        start: basePtr.advanced(by: scalarRange.startIndex),
        count: scalarRange.count
      )
      return try body(ptr)
    }
  }

  mutating func withUnsafeMutableBufferPointer<Result>(
    _ body: (inout UnsafeMutableBufferPointer<Scalar>) throws -> Result
  ) rethrows -> Result {
    // NOTE: Copying `scalarRange` to a local variable here is necessary for
    // exclusive access.
    let scalarRange = self.scalarRange
    return try base.withUnsafeMutableBufferPointer { baseBuffPtr in
      let basePtr = baseBuffPtr.baseAddress!
      var ptr = UnsafeMutableBufferPointer(
        start: basePtr.advanced(by: scalarRange.startIndex),
        count: scalarRange.count
      )
      return try body(&ptr)
    }
  }
}

extension ShapedArraySlice : RandomAccessCollection, MutableCollection {
  public typealias Index = Int
  public typealias Element = ShapedArraySlice
  public typealias SubSequence = ShapedArraySlice

  public var indices: Range<Int> {
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

  /// Access the element array specified by an index in the leading dimension.
  /// - Parameter index: Index of the element array.
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
          ptr.initialize(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
        }
      }
    }
  }

  /// Access the subarray specified by a contiguous range of indices.
  /// - Parameter bounds: Contiguous range of indices.
  public subscript(bounds: Range<Int>) -> SubSequence {
    get {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArraySlice indices are out of range")
      return ShapedArraySlice(base: base,
                              baseIndices: baseIndices,
                              bounds: bounds)
    }
    set {
      precondition(!isScalar,
                   "Scalar has no elements and cannot be subscripted")
      precondition(
        indices ~= bounds.lowerBound && indices ~= bounds.upperBound - 1,
        "ShapedArraySlice indices are out of range")
      let subArrayShape = [bounds.count] + shape.dropFirst()
      precondition(subArrayShape == newValue.shape, "Subarray shape mismatch")
      let scalarIndex = self.scalarIndex(fromIndex: bounds.lowerBound)
      withUnsafeMutableBufferPointer { destBuffPtr in
        let ptr = destBuffPtr.baseAddress!.advanced(by: scalarIndex)
        newValue.withUnsafeBufferPointer { srcBuffPtr in
          ptr.initialize(from: srcBuffPtr.baseAddress!, count: srcBuffPtr.count)
        }
      }
    }
  }
}

/// Tensor conversion
public extension ShapedArraySlice where Scalar : TensorFlowScalar {
  init(_ tensor: Tensor<Scalar>) {
    self.init(base: tensor.array)
  }
}

/// Array literal conversion
extension ShapedArraySlice : ExpressibleByArrayLiteral
  where Scalar : TensorFlowScalar {
  public typealias ArrayLiteralElement = TensorElementLiteral<Scalar>
  @inlinable @inline(__always)
  public init(arrayLiteral elements: TensorElementLiteral<Scalar>...) {
    self.init(base: Tensor(tensorElementLiterals: elements).array)
  }
}

/// Equatable conformance
extension ShapedArraySlice : Equatable where Scalar : Equatable {
  public static func == (lhs: ShapedArraySlice, rhs: ShapedArraySlice) -> Bool {
    return lhs._isEqual(to: rhs)
  }
}

/// String conversion
extension ShapedArraySlice : CustomStringConvertible {
  /// A textual representation of this `ShapedArraySlice`.
  public var description: String {
    return _description
  }
}

/// Xcode Playground display conversion
extension ShapedArraySlice : CustomPlaygroundDisplayConvertible {
  public var playgroundDescription: Any {
    return description
  }
}

/// Mirror representation, used by debugger/REPL.
extension ShapedArraySlice : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: [], displayStyle: .struct)
  }
}

/// Codable conformance.
extension ShapedArraySlice : Codable where Scalar : Codable {
  private enum CodingKeys : String, CodingKey {
    case shape
    case scalars
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(shape, forKey: .shape)
    try container.encode(scalars, forKey: .scalars)
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    let shape = try container.decode([Int].self, forKey: .shape)
    let scalars = try container.decode([Scalar].self, forKey: .scalars)
    self.init(shape: shape, scalars: scalars)
  }
}
