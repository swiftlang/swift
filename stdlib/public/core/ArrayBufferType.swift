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

/// The underlying buffer for an ArrayType conforms to
/// _ArrayBufferType.  This buffer does not provide value semantics.
public protocol _ArrayBufferType : MutableCollectionType {
  /// The type of elements stored in the buffer
  typealias Element

  /// create an empty buffer
  init()

  /// Adopt the storage of x
  init(_ buffer: _ContiguousArrayBuffer<Element>)

  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  func _uninitializedCopy(
    subRange: Range<Int>, target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element>

  /// Get/set the index'th element
  subscript(index: Int) -> Element { get nonmutating set}

  /// If this buffer is backed by a uniquely-referenced mutable
  /// _ContiguousArrayBuffer that can be grown in-place to allow the self
  /// buffer store minimumCapacity elements, returns that buffer.
  /// Otherwise, returns nil.
  ///
  /// - Note: The result's baseAddress may not match ours, if we are a
  ///   _SliceBuffer.
  ///
  /// - Note: This function must remain mutating; otherwise the buffer
  ///   may acquire spurious extra references, which will cause
  ///   unnecessary reallocation.
  mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
    -> _ContiguousArrayBuffer<Element>?

  /// Returns true iff this buffer is backed by a uniquely-referenced mutable
  /// _ContiguousArrayBuffer.
  ///
  /// - Note: This function must remain mutating; otherwise the buffer
  ///   may acquire spurious extra references, which will cause
  ///   unnecessary reallocation.
  mutating func isMutableAndUniquelyReferenced() -> Bool

  /// If this buffer is backed by a `_ContiguousArrayBuffer`
  /// containing the same number of elements as `self`, return it.
  /// Otherwise, return `nil`.
  func requestNativeBuffer() -> _ContiguousArrayBuffer<Element>?

  /// Replace the given `subRange` with the first `newCount` elements of
  /// the given collection.
  ///
  /// - Requires: This buffer is backed by a uniquely-referenced
  /// `_ContiguousArrayBuffer`.
  mutating func replace<C: CollectionType where C.Generator.Element == Element>(
    subRange subRange: Range<Int>, with newCount: Int, elementsOf newValues: C
  )

  /// Return a _SliceBuffer containing the given subRange of values
  /// from this buffer.
  subscript(subRange: Range<Int>) -> _SliceBuffer<Element> {get}

  /// Call `body(p)`, where `p` is an `UnsafeBufferPointer` over the
  /// underlying contiguous storage.  If no such storage exists, it is
  /// created on-demand.
  func withUnsafeBufferPointer<R>(
    @noescape body: (UnsafeBufferPointer<Element>) -> R
  ) -> R

  /// Call `body(p)`, where `p` is an `UnsafeMutableBufferPointer`
  /// over the underlying contiguous storage.
  ///
  /// - Requires: Such contiguous storage exists or the buffer is empty.
  mutating func withUnsafeMutableBufferPointer<R>(
    @noescape body: (UnsafeMutableBufferPointer<Element>) -> R
  ) -> R

  /// How many elements the buffer stores
  var count: Int {get set}

  /// How many elements the buffer can store without reallocation
  var capacity: Int {get}

  /// An object that keeps the elements stored in this buffer alive
  var owner: AnyObject {get}

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, nil.
  var baseAddress: UnsafeMutablePointer<Element> {get}

  /// A value that identifies the storage used by the buffer.  Two
  /// buffers address the same elements when they have the same
  /// identity and count.
  var identity: UnsafePointer<Void> {get}
}
