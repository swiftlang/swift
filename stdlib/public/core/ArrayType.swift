//===--- ArrayType.swift - Protocol for Array-like types ------------------===//
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

@usableFromInline
internal protocol _ArrayProtocol
  : RangeReplaceableCollection, ExpressibleByArrayLiteral
where Indices == Range<Int> {
  /// The number of elements the Array can store without reallocation.
  var capacity: Int { get }

  /// An object that guarantees the lifetime of this array's elements.
  var _owner: AnyObject? { get }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, `nil`.
  var _baseAddressIfContiguous: UnsafeMutablePointer<Element>? { get }

  associatedtype _Buffer: _ArrayBufferProtocol where _Buffer.Element == Element
  init(_ buffer: _Buffer)

  // For testing.
  var _buffer: _Buffer { get }
}

extension _ArrayProtocol {
  @inlinable @inline(__always)
  @_semantics("array.get_count")
  internal func _getCount() -> Int {
    return _buffer.count
  }

  @inlinable @inline(__always)
  @_semantics("array.get_capacity")
  internal func _getCapacity() -> Int {
    return _buffer.capacity
  }

  // Since RangeReplaceableCollection now has a version of filter that is less
  // efficient, we should make the default implementation coming from Sequence
  // preferred.
  @inlinable
  public __consuming func filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> [Element] {
    return try _filter(isIncluded)
  }
}
