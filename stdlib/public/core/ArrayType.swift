//===--- ArrayType.swift - Protocol for Array-like types ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
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
  @_alwaysEmitIntoClient
  public consuming func filter<E: Error>(
    _ isIncluded: (Element) throws(E) -> Bool
  ) throws(E) -> [Element] {
    try _filter(isIncluded)
  }

#if !hasFeature(Embedded)
  // ABI-only entrypoint for the rethrows version of filter, which has been
  // superseded by the typed-throws version. Expressed as "throws", which is
  // ABI-compatible with "rethrows".
  @_spi(SwiftStdlibLegacyABI) @available(swift, obsoleted: 1)
  @abi(
    __consuming func filter(
      _ isIncluded: (Element) throws -> Bool
    ) throws -> [Element]
  )
  @usableFromInline
  internal __consuming func __legacyABI_filter(
    _ isIncluded: (Element) throws -> Bool
  ) throws -> [Element] {
    try filter(isIncluded)
  }
#endif // !hasFeature(Embedded)
}
