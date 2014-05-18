//===--- ArrayBufferType.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The underlying buffer for an ArrayType conforms to ArrayBufferType
protocol ArrayBufferType : MutableCollection {
  /// The type of elements stored in the buffer
  typealias Element

  /// create an empty buffer
  init()

  /// Adopt the storage of x
  init(_ buffer: ContiguousArrayBuffer<Element>)
  
  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  func _uninitializedCopy(subRange: Range<Int>, target: UnsafePointer<Element>)
    -> UnsafePointer<Element>

  /// Convert to an NSArray.
  /// Precondition: isBridgedToObjectiveC(Element.self)
  /// O(1) if the element type is bridged verbatim, O(N) otherwise
  func _asCocoaArray() -> _CocoaArray

  /// Get/set the index'th element
  subscript(index: Int) -> Element { get nonmutating set}

  /// If this buffer is backed by a uniquely-referenced mutable
  /// ContiguousArrayBuffer that can be grown in-place to allow the self
  /// buffer store minimumCapacity elements, returns that buffer.
  /// Otherwise, returns nil.  Note: the result's elementStorage may
  /// not match ours, if we are a SliceBuffer.
  ///
  /// Note: this function must remain mutating; otherwise the buffer
  /// may acquire spurious extra references, which will cause
  /// unnecessary reallocation.
  mutating func requestUniqueMutableBuffer(minimumCapacity: Int)
    -> ContiguousArrayBuffer<Element>?

  /// If this buffer is backed by a ContiguousArrayBuffer, return it.
  /// Otherwise, return nil.  Note: the result's elementStorage may
  /// not match ours, if we are a SliceBuffer.
  func requestNativeBuffer() -> ContiguousArrayBuffer<Element>?
  
  /// Return a SliceBuffer containing the given subRange of values
  /// from this buffer.
  subscript(subRange: Range<Int>) -> SliceBuffer<Element> {get}

  /// Call body(p), where p is a pointer to the underlying contiguous storage
  /// Requires: such contiguous storage exists or the buffer is empty
  func withUnsafePointerToElements<R>(body: (UnsafePointer<Element>)->R) -> R
  
  /// How many elements the buffer stores
  var count: Int {get set}

  /// How many elements the buffer can store without reallocation
  var capacity: Int {get}

  /// An object that keeps the elements stored in this buffer alive
  var owner: AnyObject? {get}
  
  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  var elementStorage: UnsafePointer<Element> {get}

  /// A value that identifies first mutable element, if any.  Two
  /// arrays compare === iff they are both empty, or if their buffers
  /// have the same identity and count.
  var identity: Word {get}
}
