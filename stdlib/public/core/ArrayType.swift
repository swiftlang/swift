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

  #if $Embedded
  typealias AnyObject = Builtin.NativeObject
  #endif

  /// An object that guarantees the lifetime of this array's elements.
  var _owner: AnyObject? { get }

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, `nil`.
  var _baseAddressIfContiguous: UnsafeMutablePointer<Element>? { get }

  //===--- basic mutations ------------------------------------------------===//

  /// Reserve enough space to store minimumCapacity elements.
  ///
  /// - Postcondition: `capacity >= minimumCapacity` and the array has
  ///   mutable contiguous storage.
  ///
  /// - Complexity: O(`self.count`).
  override mutating func reserveCapacity(_ minimumCapacity: Int)

  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  ///
  /// - Precondition: `startIndex <= i`, `i <= endIndex`.
  override mutating func insert(_ newElement: __owned Element, at i: Int)

  /// Remove and return the element at the given index.
  ///
  /// - returns: The removed element.
  ///
  /// - Complexity: Worst case O(*n*).
  ///
  /// - Precondition: `count > index`.
  @discardableResult
  override mutating func remove(at index: Int) -> Element

  //===--- implementation detail  -----------------------------------------===//

  associatedtype _Buffer: _ArrayBufferProtocol where _Buffer.Element == Element
  init(_ buffer: _Buffer)

  // For testing.
  var _buffer: _Buffer { get }
}

extension _ArrayProtocol {
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
