//===--- ArrayType.swift - Protocol for Array-like types ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public // @testable
protocol _ArrayType
  : RangeReplaceableCollectionType,
    MutableSliceable,
    ArrayLiteralConvertible
{
  //===--- public interface -----------------------------------------------===//
  /// Construct an array of `count` elements, each initialized to `repeatedValue`.
  init(count: Int, repeatedValue: Generator.Element)

  /// The number of elements the Array stores.
  var count: Int {get}

  /// The number of elements the Array can store without reallocation.
  var capacity: Int {get}

  /// `true` if and only if the Array is empty.
  var isEmpty: Bool {get}

  /// An object that guarantees the lifetime of this array's elements.
  var _owner: AnyObject? {get}

  /// If the elements are stored contiguously, a pointer to the first
  /// element. Otherwise, `nil`.
  var _baseAddressIfContiguous: UnsafeMutablePointer<Element> {get}

  subscript(index: Int) -> Generator.Element {get set}

  //===--- basic mutations ------------------------------------------------===//

  /// Reserve enough space to store minimumCapacity elements.
  ///
  /// - Postcondition: `capacity >= minimumCapacity` and the array has
  ///   mutable contiguous storage.
  ///
  /// - Complexity: O(`self.count`).
  mutating func reserveCapacity(minimumCapacity: Int)

  /// Operator form of `appendContentsOf`.
  func += <
    S: SequenceType where S.Generator.Element == Generator.Element
  >(inout lhs: Self, rhs: S)

  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  ///
  /// - Requires: `atIndex <= count`.
  mutating func insert(newElement: Generator.Element, atIndex i: Int)

  /// Remove and return the element at the given index.
  ///
  /// - returns: The removed element.
  ///
  /// - Complexity: Worst case O(N).
  ///
  /// - Requires: `count > index`.
  mutating func removeAtIndex(index: Int) -> Generator.Element

  //===--- implementation detail  -----------------------------------------===//

  typealias _Buffer : _ArrayBufferType
  init(_ buffer: _Buffer)

  // For testing.
  var _buffer: _Buffer {get}
}

internal struct _ArrayTypeMirror<
  T : _ArrayType where T.Index == Int
> : _MirrorType {
  let _value : T

  init(_ v : T) { _value = v }

  var value: Any { return (_value as Any) }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return nil }

  var count: Int { return _value.count }

  subscript(i: Int) -> (String, _MirrorType) {
    _precondition(i >= 0 && i < count, "_MirrorType access out of bounds")
    return ("[\(i)]", _reflect(_value[_value.startIndex + i]))
  }

  var summary: String {
    if count == 1 { return "1 element" }
    return "\(count) elements"
  }

  var quickLookObject: PlaygroundQuickLook? { return nil }

  var disposition: _MirrorDisposition { return .IndexContainer }
}
