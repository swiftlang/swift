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

/// Serves as the buffer for an ArrayType.  An ArrayBufferType does
/// not impose value semantics on its elements, and whether its
/// elements are actually being managed by copy-on-write (COW) is, in
/// principle, unknown to the buffer.
protocol ArrayBufferType : MutableCollection {
  /// The type of elements stored in the buffer
  typealias Element

  /// create an empty buffer
  init()

  /// Adopt the storage of x
  init(x: NativeArrayBuffer<Element>)

  /// Copy the given subRange of this buffer into uninitialized memory
  /// starting at target.  Return a pointer past-the-end of the
  /// just-initialized memory.
  func _uninitializedCopy(subRange: Range<Int>, target: UnsafePointer<Element>)
    -> UnsafePointer<Element>

  /// Convert to an NSArray in O(1).  
  /// Precondition: isBridgedToObjectiveC(Element.self)
  func asCocoaArray() -> CocoaArray

  /// Convert to a NativeArrayBuffer storing the same elements.
  /// managedByCopyOnWrite is destined to be removed soon, but it
  /// indicates whether the buffer is to be marked for copying in
  /// copyWithZone.
  func toNativeBuffer(managedByCopyOnWrite: Bool)
    -> NativeArrayBuffer<Element>
  
  /// Return true iff this buffer's storage is uniquely-referenced.
  /// NOTE: this does not mean the buffer is mutable.  Other factors
  /// may need to be considered, such as whether the buffer could be
  /// some immutable Cocoa container.
  mutating func isUniquelyReferenced() -> Bool

  /// Returns true iff this buffer is mutable. NOTE: a true result
  /// does not mean the buffer is uniquely-referenced.
  func isMutable() -> Bool
  
  /// Return a SliceBuffer containing the given subRange of values
  /// from this buffer.
  subscript(subRange: Range<Int>) -> SliceBuffer<Element> {get}
  
  /// How many elements the buffer stores
  var count: Int {get set}

  /// How many elements the buffer can store without reallocation
  var capacity: Int {get}
}
