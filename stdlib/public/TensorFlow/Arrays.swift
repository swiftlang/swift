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
/// buffer object stores a pointer to contiguous units; in 'tensorFlow'
/// mode, the buffer object stores a `TF_Tensor*` and bridges to TensorFlow.
/// In either mode, the buffer object owns the memory and will deallocate it
/// on `deinit`.
fileprivate final class TensorBuffer<Unit> {
  typealias Shape = [Int]

  enum Allocation {
    case native(UnsafeMutablePointer<Unit>)
    case tensorFlow(CTensor)
  }

  let allocation: Allocation
  private let bufferPointer: UnsafeMutableBufferPointer<Unit>

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
        .assumingMemoryBound(to: Unit.self)
      bufferPointer = UnsafeMutableBufferPointer(
        start: startAddress, count: count)
    }
  }

  convenience init(owning startAddress: UnsafeMutablePointer<Unit>,
                   count: Int) {
    self.init(allocation: .native(startAddress), count: count)
  }
}

/// TF Tensor-specific initializer
extension TensorBuffer where Unit : AccelerableTensorUnit {
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
  static func createUninitialized(count: Int) -> TensorBuffer<Unit> {
    let pointer = UnsafeMutablePointer<Unit>.allocate(capacity: count)
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
  func withUnsafeMutablePointerToUnits<R>(
    _ body: (UnsafeMutablePointer<Unit>) throws -> R
  ) rethrows -> R {
    // `baseAddress` is guaranteed non-nil
    return try body(bufferPointer.baseAddress!)
  }

  func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Unit>) throws -> R
  ) rethrows -> R {
    var bufferPointer = self.bufferPointer
    return try body(&bufferPointer)
  }
}

//===----------------------------------------------------------------------===//
// ShapedArrayProtocol, the protocol that all shaped array types, including
// ShapedArray, Array, Array2D, etc, will conform to.
//===----------------------------------------------------------------------===//

public protocol ShapedArrayProtocol {
  associatedtype Unit
  associatedtype Shape

  /// Number of dimensions in this array.
  var rank: Int { get }
  /// Shape of this array.
  var shape: Shape { get }
  /// The total number of units in this array.
  var unitCount: Int { get }

  /// Initialize an array using specific shape and contiguous units in
  /// row-major order.
  /// - Precondition: The number of `units` must be equal to the product of
  /// all dimensions of the shape.
  init(shape: Shape, units: [Unit])

  /// Initialize an array using specific shape and a sequence of units in
  /// row-major order.
  /// - Precondition: The number of `units` must be equal to the product of
  /// all dimensions of the shape.
  init<S : Sequence>(shape: Shape, units: S) where S.Element == Unit

  /// Initialize an array using specific shape and the contiguous view of
  /// another collection that conforms to ShapedArrayProtocol.
  /// - Precondition: The number of `units` must be equal to the product of
  /// all dimensions of the shape.
  init<Other>(shape: [Int], units: ContiguousView<Other>)
    where Unit == Other.Unit

  /// Calls a closure with a pointer to the array’s contiguous storage.
  /// - Parameter body: A closure with an UnsafeBufferPointer parameter that
  /// points to the contiguous storage for the array. If no such storage exists,
  /// it is created. If body has a return value, that value is also used as the
  /// return value for the withUnsafeBufferPointer(_:) method. The pointer
  /// argument is valid only for the duration of the method’s execution.
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Unit>) throws -> R
  ) rethrows -> R

  /// Calls the given closure with a pointer to the array’s mutable contiguous
  /// storage.
  /// - Parameter body: A closure with an UnsafeMutableBufferPointer parameter
  /// that points to the contiguous storage for the array. If no such storage
  /// exists, it is created. If body has a return value, that value is also used
  /// as the return value for the withUnsafeMutableBufferPointer(_:) method. The
  /// pointer argument is valid only for the duration of the method’s execution.
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Unit>) throws -> R
  ) rethrows -> R
}

public extension ShapedArrayProtocol {
  /// A collection view of contiguous units in row-major order.
  var units: ContiguousView<Self> {
    return ContiguousView(base: self)
  }
}

public extension ShapedArrayProtocol where Unit : Equatable, Shape : Equatable {
  static func == <Other>(lhs: Self, rhs: Other) -> Bool
    where Shape == Other.Shape,
          Other : ShapedArrayProtocol,
          Unit == Other.Unit {
    return lhs.shape == rhs.shape &&
      lhs.units.elementsEqual(rhs.units)
  }
}

//===----------------------------------------------------------------------===//
// ShapedArray
//===----------------------------------------------------------------------===//

/// ShapedArray
public struct ShapedArray<Unit> : ShapedArrayProtocol {
  public typealias Shape = [Int]

  /// Contiguous memory storing units.
  fileprivate var buffer: TensorBuffer<Unit>

  /// The shape of this array.
  public private(set) var shape: [Int]

  /// Initialize a shaped array from an existing buffer and a shape.
  fileprivate init(buffer: TensorBuffer<Unit>, shape: [Int]) {
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
    buffer = TensorBuffer.createUninitialized(count: unitCount)
    oldBuffer.withUnsafeMutablePointerToUnits { oldPtr in
      buffer.withUnsafeMutablePointerToUnits { ptr in
        ptr.initialize(from: oldPtr, count: unitCount)
      }
    }
  }
}

internal extension ShapedArray where Unit : AccelerableTensorUnit {
  @_versioned
  init(moving cTensor: CTensor) {
    // Including \(Unit.self) into the message would cause non-deterministic crashes.
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

  var unitCount: Int {
    return buffer.count
  }

  var isScalar: Bool {
    return rank == 0
  }

  var scalar: Unit? {
    guard rank == 0 else { return nil }
    return units.first
  }

  init(_ other: ShapedArray) {
    debugLog("Initializing from another array.")
    self.init(buffer: other.buffer, shape: other.shape)
  }

  init(shape: [Int], units: ContiguousView<ShapedArray>) {
    precondition(units.count == shape.reduce(1, *))
    self.init(buffer: units.base.buffer, shape: shape)
  }

  init(shape: [Int], units: [Unit]) {
    let unitCount = shape.reduce(1, *)
    precondition(unitCount == units.count)
    let buffer = TensorBuffer<Unit>.createUninitialized(count: unitCount)
    self.init(buffer: buffer, shape: shape)
    buffer.withUnsafeMutablePointerToUnits { ptr in
      units.withUnsafeBufferPointer { arrayBuf in
        ptr.initialize(from: arrayBuf.baseAddress!, count: unitCount)
      }
    }
  }

  init<S : Sequence>(shape: [Int], units: S) where S.Element == Unit {
    let unitCount = shape.reduce(1, *)
    let buffer = TensorBuffer<Unit>.createUninitialized(count: unitCount)
    self.init(buffer: buffer, shape: shape)
    buffer.withUnsafeMutablePointerToUnits { ptr in
      // TODO: Refactor with better pointer initializers in Swift 4.1.
      var i = 0
      for unit in units {
        guard i < unitCount else { break }
        ptr.advanced(by: i).initialize(to: unit)
        i += 1
      }
      // If the sequence has fewer elements than the shape needs, this is a
      // precondition failure.
      precondition(i == unitCount,
                   "The sequence has fewer elements than needed by the shape.")
    }
  }

  init<Other>(shape: [Int], units: ContiguousView<Other>)
    where Unit == Other.Unit {
    let count = shape.reduce(1, *)
    precondition(count == units.count)
    let buffer = TensorBuffer<Unit>.createUninitialized(count: count)
    self.init(buffer: buffer, shape: shape)
    buffer.withUnsafeMutablePointerToUnits { ptr in
      units.base.withUnsafeBufferPointer { arrayBuf in
        ptr.initialize(from: arrayBuf.baseAddress!, count: count)
      }
    }
  }

  /// Initialize a scalar tensor.
  init(_ scalar: Unit) {
    let buffer = TensorBuffer<Unit>.createUninitialized(count: 1)
    self.init(buffer: buffer, shape: [])
    buffer.withUnsafeMutablePointerToUnits { ptr in
      ptr.initialize(to: scalar)
    }
  }

  /// Allocate and initialize a tensor to a repeated value.
  /// - parameter shape: tensor shape
  /// - parameter repeating: repeated value
  init(shape: Shape, repeating repeatedValue: Unit) {
    let unitCount = shape.reduce(1, *)
    let buffer = TensorBuffer<Unit>.createUninitialized(count: unitCount)
    self.init(buffer: buffer, shape: shape)
    buffer.withUnsafeMutablePointerToUnits { ptr in
      ptr.initialize(repeating: repeatedValue, count: unitCount)
    }
  }
}

public extension ShapedArray {
  func withUnsafeBufferPointer<Result>(
    _ body: (UnsafeBufferPointer<Unit>) throws -> Result
  ) rethrows -> Result {
    return try buffer.withUnsafeMutableBufferPointer { ptr in
      try body(UnsafeBufferPointer(ptr))
    }
  }

  mutating func withUnsafeMutableBufferPointer<Result>(
    _ body: (inout UnsafeMutableBufferPointer<Unit>) throws -> Result
  ) rethrows -> Result {
    ensureUniquelyReferenced()
    return try buffer.withUnsafeMutableBufferPointer { ptr in
      try body(&ptr)
    }
  }
}

/// Tensor conversion
extension ShapedArray where Unit : AccelerableTensorUnit {
  var byteSize: Int {
    return MemoryLayout<Unit>.stride * unitCount
  }

  func makeTensorHandle() -> TensorHandle<Unit> {
    // This initializer is designed to optimize conversion from TF-allocated
    // `ShapedArray`s.
    switch buffer.allocation {
    case let .native(bufAddr):
      let cTensor = TF_AllocateTensor(Unit.cDataType,
                                      shape.map(Int64.init),
                                      Int32(shape.count),
                                      byteSize)!
      TF_TensorData(cTensor).assumingMemoryBound(to: Unit.self)
        .initialize(from: bufAddr, count: unitCount)
      defer {
        TF_DeleteTensor(cTensor)
      }
      return TensorHandle(copyingFromCTensor: cTensor)
    case let .tensorFlow(cTensor):
      return TensorHandle(copyingFromCTensor: cTensor)
    }
  }
}

/// Tensor conversion
public extension Tensor where Unit : AccelerableTensorUnit {
  init(_ other: ShapedArray<Unit>) {
    self.init(other.makeTensorHandle())
  }
}

//===----------------------------------------------------------------------===//
// ShapedArraySlice
//===----------------------------------------------------------------------===//

/// ShapedArraySlice
public struct ShapedArraySlice<Unit> /*: ShapedArrayProtocol */ {
  public typealias Shape = [Int]

  private var base: ShapedArray<Unit>
  private var baseIndices: [Int]
  private var bounds: CountableRange<Int>?

  /// Initialize a shaped array slice from a shaped array as base, with
  /// specified subdimensional inwards indices and subtensor bounds.
  fileprivate init(
    base: ShapedArray<Unit>,
    baseIndices indices: [Int] = [],
    bounds: CountableRange<Int>? = nil
  ) {
    precondition(indices.count <= base.rank, "Element indices are out of bounds.")
    precondition(zip(base.shape, indices).forAll { $1 >= 0 && $1 < $0 })
    self.base = base
    self.baseIndices = indices
    self.bounds = bounds
  }
}

public extension ShapedArraySlice {
  init(shape: [Int], units: ContiguousView<ShapedArray<Unit>>) {
    self.init(base: ShapedArray(buffer: units.base.buffer, shape: shape))
  }

  init(shape: [Int], units: [Unit]) {
    self.init(base: ShapedArray(shape: shape, units: units))
  }

  init<S : Sequence>(shape: [Int], units: S) where S.Element == Unit {
    self.init(base: ShapedArray(shape: shape, units: units))
  }

  init<Other>(shape: [Int], units: ContiguousView<Other>)
    where Unit == Other.Unit {
    self.init(base: ShapedArray(shape: shape, units: units))
  }

  /// Initialize a scalar tensor.
  init(_ scalar: Unit) {
    self.init(base: ShapedArray(scalar))
  }

  /// Allocate and initialize a tensor to a repeated value.
  /// - parameter shape: tensor shape
  /// - parameter repeating: repeated value
  init(shape: Shape, repeating repeatedValue: Unit) {
    self.init(base: ShapedArray(shape: shape, repeating: repeatedValue))
  }
}

public extension ShapedArraySlice where Unit : AccelerableTensorUnit {
  init(_ other: Tensor<Unit>) {
    self.init(base: other.array)
  }
}

//===----------------------------------------------------------------------===//
// ContiguousView
//===----------------------------------------------------------------------===//

/// A view of a shaped array's contents as a collection of units. It can be
/// created using the `.units` getter on a value that conforms to
/// `ShapedArrayProtocol`.
///
/// For example, one can mutate the units of a shaped array using linear
/// indices:
///
///   let tensor = Tensor([[1, 2], [3, 4]]) // Shape 2 x 2
///   tensor.units[3] = 100
///
public struct ContiguousView<Base : ShapedArrayProtocol> {
  public typealias Element = Base.Unit
  fileprivate var base: Base

  fileprivate init(base: Base) {
    debugLog("Initializing ContiguousView.")
    self.base = base
  }
}

extension ContiguousView : RandomAccessCollection {
  public var count: Int {
    return base.unitCount
  }

  public var startIndex: Int {
    return 0
  }

  public var endIndex: Int {
    return count
  }

  public func index(after i: Int) -> Int {
    return i + 1
  }

  public func index(before i: Int) -> Int {
    return i - 1
  }

  public subscript(index: Int) -> Element {
    get {
      debugLog("Getting element \(index).")
      let ret = base.withUnsafeBufferPointer { $0[index] }
      debugLog("Elem has value \(ret).")
      return ret
    }
    set {
      base.withUnsafeMutableBufferPointer { $0[index] = newValue }
    }
  }
}

extension ContiguousView : Equatable where Element : Equatable {
  public static func == (lhs: ContiguousView, rhs: ContiguousView) -> Bool {
    return lhs.elementsEqual(rhs)
  }
}

/// FIXME: Implement after `ShapedArraySlice` and `subscript` are implemented.
// extension ShapedArray : CustomStringConvertible {
//   public var description: String {
//     if isScalar {
//       return withUnsafeBufferPointer { bufPtr in
//         String(describing: bufPtr[0])
//       }
//     }
//     return "[\( map({"\($0)"}).joined(separator: ", ") )]")
//   }
// }
