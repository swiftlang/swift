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
/// buffer object stores a pointer to contiguous elements; in 'tensorFlow'
/// mode, the buffer object stores a `TF_Tensor*` and bridges to TensorFlow.
/// In either mode, the buffer object owns the memory and will deallocate it
/// on `deinit`.
fileprivate final class TensorBuffer<Element> {
  typealias Shape = [Int]

  enum Allocation {
    case native(UnsafeMutablePointer<Element>)
    case tensorFlow(CTensor)
  }

  let allocation: Allocation
  private let bufferPointer: UnsafeMutableBufferPointer<Element>

  deinit {
    switch allocation {
    case let .native(pointer):
      pointer.deinitialize(count: count)
      pointer.deallocate()
    case let .tensorFlow(cTensor):
      TF_DeleteTensor(cTensor)
    }
  }

  init(allocation: Allocation, count: Int) {
    self.allocation = allocation
    // Initialize `bufferPointer`.
    switch allocation {
    case let .native(ptr):
      bufferPointer = UnsafeMutableBufferPointer(start: ptr, count: count)
    case let .tensorFlow(cTensor):
      let startAddress = TF_TensorData(cTensor)
        .assumingMemoryBound(to: Element.self)
      bufferPointer = UnsafeMutableBufferPointer(
        start: startAddress, count: count)
    }
  }

  convenience init(owning startAddress: UnsafeMutablePointer<Element>,
                   count: Int) {
    self.init(allocation: .native(startAddress), count: count)
  }
}

/// TF Tensor-specific initializer
extension TensorBuffer where Element : TensorElementProtocol {
  /// Initialize a local tensor buffer from a C `TF_Tensor*` value and takes
  /// ownership of the value.
  convenience init(owning cTensor: CTensor, count: Int) {
    let actualCount = (0..<TF_NumDims(cTensor)).reduce(1) { acc, next in
      acc * Int(TF_Dim(cTensor, next))
    }
    assert(actualCount == count)
    self.init(allocation: .tensorFlow(cTensor), count: count)
  }
}

/// Factory methods
extension TensorBuffer {
  static func createUninitialized(count: Int) -> TensorBuffer<Element> {
    let pointer = UnsafeMutablePointer<Element>.allocate(capacity: count)
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
  func withUnsafeMutablePointerToElements<R>(
    _ body: (UnsafeMutablePointer<Element>) throws -> R
  ) rethrows -> R {
    // `baseAddress` is guaranteed non-nil
    return try body(bufferPointer.baseAddress!)
  }

  func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
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
  associatedtype Element
  associatedtype Shape

  /// Number of dimensions in this array.
  var rank: Int { get }
  /// Shape of this array.
  var shape: Shape { get }
  /// The total number of elements in this array.
  var totalElementCount: Int { get }

  /// Initialize an array using specific shape and contiguous elements in
  /// row-major order.
  /// - Precondition: The number of `elements` must be equal to the product of
  /// all dimensions of the shape.
  init(shape: Shape, elements: [Element])

  /// Initialize an array using specific shape and the contiguous view of
  /// another collection that conforms to ShapedArrayProtocol.
  /// - Precondition: The number of `elements` must be equal to the product of
  /// all dimensions of the shape.
  init<Other>(shape: [Int], elements: ContiguousView<Other>)
    where Element == Other.Element

  /// Calls a closure with a pointer to the array’s contiguous storage.
  /// - Parameter body: A closure with an UnsafeBufferPointer parameter that
  /// points to the contiguous storage for the array. If no such storage exists,
  /// it is created. If body has a return value, that value is also used as the
  /// return value for the withUnsafeBufferPointer(_:) method. The pointer
  /// argument is valid only for the duration of the method’s execution.
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R

  /// Calls the given closure with a pointer to the array’s mutable contiguous
  /// storage.
  /// - Parameter body: A closure with an UnsafeMutableBufferPointer parameter
  /// that points to the contiguous storage for the array. If no such storage
  /// exists, it is created. If body has a return value, that value is also used
  /// as the return value for the withUnsafeMutableBufferPointer(_:) method. The
  /// pointer argument is valid only for the duration of the method’s execution.
  mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R
}

public extension ShapedArrayProtocol {
  /// A collection view of contiguous elements in row-major order.
  var contiguousView: ContiguousView<Self> {
    return ContiguousView(base: self)
  }
}

public extension ShapedArrayProtocol where Element : Equatable, Shape : Equatable {
  static func == <Other>(lhs: Self, rhs: Other) -> Bool
    where Shape == Other.Shape,
          Other: ShapedArrayProtocol,
          Element == Other.Element {
    return lhs.shape == rhs.shape &&
      lhs.contiguousView.elementsEqual(rhs.contiguousView)
  }
}

//===----------------------------------------------------------------------===//
// ShapedArray
//===----------------------------------------------------------------------===//

/// ShapedArray
public struct ShapedArray<Element> : ShapedArrayProtocol {
  public typealias Shape = [Int]

  /// Contiguous memory storing elements.
  fileprivate var buffer: TensorBuffer<Element>

  /// The shape of this array.
  public private(set) var shape: [Int]

  /// Initialize a shaped array from an existing buffer and a shape.
  fileprivate init(buffer: TensorBuffer<Element>, shape: [Int]) {
    precondition(buffer.count == shape.reduce(1, *))
    self.buffer = buffer
    self.shape = shape
  }
}

internal extension ShapedArray {
  mutating func ensureUniquelyReferenced() {
    if isKnownUniquelyReferenced(&buffer) { return }
    let oldBuffer = buffer
    buffer = TensorBuffer.createUninitialized(count: totalElementCount)
    oldBuffer.withUnsafeMutablePointerToElements { oldPtr in
      buffer.withUnsafeMutablePointerToElements { ptr in
        ptr.initialize(from: oldPtr, count: totalElementCount)
      }
    }
  }
}

internal extension ShapedArray where Element : TensorElementProtocol {
  @_versioned
  init(moving cTensor: CTensor) {
    shape = (0..<TF_NumDims(cTensor)).map { Int(TF_Dim(cTensor, $0)) }
    buffer = TensorBuffer(owning: cTensor, count: shape.reduce(1, *))
  }
}

public extension ShapedArray {
  var rank: Int {
    return shape.count
  }

  var totalElementCount: Int {
    return buffer.count
  }

  var isScalar: Bool {
    return rank == 0
  }

  var scalar: Element? {
    guard rank == 0 else { return nil }
    return contiguousView.first
  }

  init(_ other: ShapedArray) {
    self.init(buffer: other.buffer, shape: other.shape)
  }

  init(shape: [Int], elements: ContiguousView<ShapedArray>) {
    precondition(elements.count == shape.reduce(1, *))
    self.init(buffer: elements.base.buffer, shape: shape)
  }

  init(shape: [Int], elements: [Element]) {
    let count = shape.reduce(1, *)
    precondition(count == elements.count)
    self.init(buffer: TensorBuffer.createUninitialized(count: count), shape: shape)
    buffer.withUnsafeMutablePointerToElements { ptr in
      elements.withUnsafeBufferPointer { arrayBuf in
        ptr.initialize(from: arrayBuf.baseAddress!, count: count)
      }
    }
  }

  init<Other>(shape: [Int], elements: ContiguousView<Other>)
    where Element == Other.Element {
    let count = shape.reduce(1, *)
    precondition(count == elements.count)
    self.init(buffer: TensorBuffer.createUninitialized(count: count), shape: shape)
    buffer.withUnsafeMutablePointerToElements { ptr in
      elements.base.withUnsafeBufferPointer { arrayBuf in
        ptr.initialize(from: arrayBuf.baseAddress!, count: count)
      }
    }
  }

  /// Initialize a scalar tensor.
  init(_ scalar: Element) {
    self.init(buffer: TensorBuffer.createUninitialized(count: 1),
              shape: [])
    buffer.withUnsafeMutablePointerToElements { ptr in
      ptr.initialize(to: scalar)
    }
  }

  /// Allocate and initialize a tensor to a repeated value.
  /// - parameter shape: tensor shape
  /// - parameter repeating: repeated value
  init(shape: Shape, repeating repeatedValue: Element) {
    let count = shape.reduce(1, *)
    self.init(buffer: TensorBuffer.createUninitialized(count: count), shape: shape)
    buffer.withUnsafeMutablePointerToElements { ptr in
      ptr.initialize(repeating: repeatedValue, count: count)
    }
  }
}

public extension ShapedArray {
  func withUnsafeBufferPointer<Result>(
    _ body: (UnsafeBufferPointer<Element>) throws -> Result
  ) rethrows -> Result {
    return try buffer.withUnsafeMutableBufferPointer { ptr in
      try body(UnsafeBufferPointer(ptr))
    }
  }

  mutating func withUnsafeMutableBufferPointer<Result>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> Result
  ) rethrows -> Result {
    ensureUniquelyReferenced()
    return try buffer.withUnsafeMutableBufferPointer { ptr in
      try body(&ptr)
    }
  }
}


/// Tensor conversion
extension ShapedArray where Element : TensorElementProtocol {
  var byteSize: Int {
    return MemoryLayout<Element>.stride * totalElementCount
  }

  func makeTensorHandle() -> TensorHandle<Element> {
    // This initializer is designed to optimize conversion from TF-allocated
    // `ShapedArray`s.
    switch buffer.allocation {
    case let .native(bufAddr):
      let cTensor = TF_AllocateTensor(Element.cDataType,
                                      shape.map(Int64.init),
                                      Int32(shape.count),
                                      byteSize)!
      TF_TensorData(cTensor).assumingMemoryBound(to: Element.self)
        .initialize(from: bufAddr, count: totalElementCount)
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
public extension Tensor where Element : TensorElementProtocol {
  init(_ other: ShapedArray<Element>) {
    self.init(other.makeTensorHandle())
  }
}

//===----------------------------------------------------------------------===//
// ContiguousView
//===----------------------------------------------------------------------===//

/// A view of a shaped array's contents as a collection of elements. It can be
/// created using the `.contiguousView` getter on a value that conforms to
/// `ShapedArrayProtocol`.
///
/// For example, one can mutate the elements of a shaped array using linear
/// indices:
///
///   let tensor = Tensor([[1, 2], [3, 4]]) // Shape 2 x 2
///   tensor.contiguousView[3] = 100
///
public struct ContiguousView<Base : ShapedArrayProtocol> {
  public typealias Element = Base.Element
  fileprivate var base: Base

  fileprivate init(base: Base) {
    self.base = base
  }
}

extension ContiguousView : RandomAccessCollection {
  public var count: Int {
    return base.totalElementCount
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
      return base.withUnsafeBufferPointer { $0[index] }
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
