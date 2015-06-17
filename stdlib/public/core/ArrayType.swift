//===--- ArrayType.swift - Protocol for Array-like types ------------------===//
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

public protocol __ArrayType : CollectionType {
  var count: Int {get}

  typealias _Buffer : _ArrayBufferType
  var _buffer: _Buffer {get}

  func _doCopyToNativeArrayBuffer() -> _ContiguousArrayBuffer<Generator.Element>
}

extension __ArrayType {
  public func _copyToNativeArrayBuffer(
  ) -> _ContiguousArrayBuffer<Generator.Element> {
    return _doCopyToNativeArrayBuffer()
  }
}

public // @testable
protocol _ArrayType
  : __ArrayType,
    RangeReplaceableCollectionType,
    MutableSliceable,
    ArrayLiteralConvertible
{
  //===--- public interface -----------------------------------------------===//
  /// Construct an empty Array.
  init()

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
  /// - Complexity: O(`count`).
  mutating func reserveCapacity(minimumCapacity: Int)

  /// Append newElement to the Array in O(1) (amortized).
  mutating func append(newElement: Generator.Element)

  /// Append elements from `sequence` to the Array.
  mutating func extend<
      S : SequenceType
      where S.Generator.Element == Generator.Element
  >(sequence: S)

  /// Operator form of `extend`.
  func += <
    S: SequenceType where S.Generator.Element == Generator.Element
  >(inout lhs: Self, rhs: S)

  /// Remove an element from the end of the Array in O(1).
  ///
  /// - returns: The removed element.
  ///
  /// - Requires: `count > 0`.
  mutating func removeLast() -> Generator.Element

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

  /// Erase all the elements.  If `keepCapacity` is `true`, `capacity`
  /// will not change.
  mutating func removeAll(keepCapacity keepCapacity: Bool)

  //===--- implementation detail  -----------------------------------------===//

  typealias _Buffer : _ArrayBufferType
  init(_ buffer: _Buffer)
}

internal struct _ArrayTypeMirror<T : _ArrayType> : MirrorType {
  let _value : T

  init(_ v : T) { _value = v }

  var value: Any { return (_value as Any) }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return nil }

  var count: Int { return _value.count }

  subscript(i: Int) -> (String, MirrorType) {
    _precondition(i >= 0 && i < count, "MirrorType access out of bounds")
    return ("[\(i)]", reflect(_value[i]))
  }

  var summary: String {
    if count == 1 { return "1 element" }
    return "\(count) elements"
  }

  var quickLookObject: QuickLookObject? { return nil }

  var disposition: MirrorDisposition { return .IndexContainer }
}
