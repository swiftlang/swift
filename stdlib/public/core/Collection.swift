//===----------------------------------------------------------------------===//
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

/// Return the number of elements in x.
///
/// O(1) if T.Index is RandomAccessIndexType; O(N) otherwise.
public func count <T : CollectionType>(x: T) -> T.Index.Distance {
  // FIXME(prext): remove this function when protocol extensions land.
  return x._prext_count()
}

public protocol _CollectionDefaultsType
  : SequenceType, _SequenceDefaultsType {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  typealias Index : ForwardIndexType

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  var startIndex: Index {get}

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  var endIndex: Index {get}

  // The declaration of Element and _subscript here is a trick used to
  // break a cyclic conformance/deduction that Swift can't handle.  We
  // need something other than a CollectionType.Generator.Element that can
  // be used as IndexingGenerator<T>'s Element.  Here we arrange for the
  // CollectionType itself to have an Element type that's deducible from
  // its subscript.  Ideally we'd like to constrain this
  // Element to be the same as CollectionType.Generator.Element (see
  // below), but we have no way of expressing it today.
  typealias _Element
  subscript(_i: Index) -> _Element {get}
}

extension _CollectionDefaultsType {
  /// Returns `true` iff `self` is empty.
  final public var _prext_isEmpty: Bool {
    return startIndex == endIndex
  }

  /// Return a value less than or equal to the number of elements in
  /// `self`, **nondestructively**.
  ///
  /// - complexity: O(N)
  final public func _prext_underestimateCount() -> Int {
    return numericCast(_prext_count())
  }

  /// Return the number of elements.
  ///
  /// - complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
  /// O(N) otherwise.
  final public func _prext_count() -> Index.Distance {
    return distance(startIndex, endIndex)
  }

  /// Customization point for `SequenceType._prext_find()`.
  ///
  /// Define this method if the collection can find an element in less than
  /// O(N) by exploiting collection-specific knowledge.
  ///
  /// Returns: `nil` if a linear search should be attempted instead,
  /// `Optional(nil)` if the element was not found, or
  /// `Optional(Optional(index))` if an element was found.
  ///
  /// - complexity: O(N)
  final public func _customFindEquatableElement(
    element: Generator.Element
  ) -> Index?? {
    return nil
  }
}

/// This protocol is an implementation detail of `CollectionType`; do
/// not use it directly.
///
/// Its requirements are inherited by `CollectionType` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _CollectionGeneratorDefaultsType {
  typealias Index : ForwardIndexType
  typealias _Element
  subscript(position: Index) -> _Element { get }
  var startIndex: Index { get }
  var endIndex: Index { get }
}

extension _CollectionGeneratorDefaultsType {
  final public func generate() -> IndexingGenerator<Self> {
    return IndexingGenerator(self)
  }
}

extension _CollectionDefaultsType {
  final public subscript(_prext_bounds: Range<Index>) -> _prext_Slice<Self> {
    return _prext_Slice(_collection: self, bounds: _prext_bounds)
  }
}

/// A multi-pass *sequence* with addressable positions.
///
/// Positions are represented by an associated `Index` type.  Whereas
/// an arbitrary *sequence* may be consumed as it is traversed, a
/// *collection* is multi-pass: any element may be revisited merely by
/// saving its index.
///
/// The sequence view of the elements is identical to the collection
/// view.  In other words, the following code binds the same series of
/// values to `x` as does `for x in self {}`:
///
///     for i in startIndex..<endIndex {
///       let x = self[i]
///     }
public protocol CollectionType
  : SequenceType, _CollectionDefaultsType,
  _CollectionGeneratorDefaultsType {

  /// Access the element indicated by `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  subscript(position: Index) -> Generator.Element {get}

  /// The *collection* type that represents a sub-range of elements.
  ///
  /// Though it can't currently be enforced by the type system, the
  /// `_prext_SubSlice` type in a concrete implementation of `CollectionType`
  /// should also be `CollectionType`.
  typealias _prext_SubSlice : _CollectionDefaultsType
  // <rdar://problem/20715031> CollectionType.SubSlice should be constrained to CollectionType
  // <rdar://problem/20715697> CollectionType.SubSlice should constrain its Element type

  subscript(_prext_bounds: Range<Index>) -> _prext_SubSlice { get }

  /// Returns `true` iff `self` is empty.
  var _prext_isEmpty: Bool { get }

  /// Return the number of elements.
  ///
  /// - complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
  /// O(N) otherwise.
  func _prext_count() -> Index.Distance

  /// Customization point for `SequenceType._prext_find()`.
  ///
  /// Define this method if the collection can find an element in less than
  /// O(N) by exploiting collection-specific knowledge.
  ///
  /// Returns: `nil` if a linear search should be attempted instead,
  /// `Optional(nil)` if the element was not found, or
  /// `Optional(Optional(index))` if an element was found.
  ///
  /// - complexity: O(N)
  func _customFindEquatableElement(element: Generator.Element) -> Index??
}

// A fast implementation for when you are backed by a contiguous array.
public func ~> <T : protocol<_Sequence_Type, _ArrayType>>(
  source: T, ptr: (_InitializeTo, UnsafeMutablePointer<T.Generator.Element>)) {
  let s = source._baseAddressIfContiguous
  if s != nil {
    let p = UnsafeMutablePointer<T.Element>(ptr.1)
    p.initializeFrom(s, count: source.count)
    _fixLifetime(source._owner)
  } else {
    var p = UnsafeMutablePointer<T.Generator.Element>(ptr.1)
    var g = source.generate()
    while let x? = g.next() {
      p++.initialize(x)
    }
  }
}

// Default implementation of `preprocessingPass` for *collections*.  Do not
// use this operator directly; call `_preprocessingPass(s)` instead
public func ~> <T : CollectionType, R>(
  s: T, args: (_PreprocessingPass, ( (T)->R ))
) -> R? {
  return args.1(s)
}

/// Returns `true` iff `x` is empty.
public func isEmpty<C: CollectionType>(x: C) -> Bool {
  // FIXME(prext): remove this function when protocol extensions land.
  return x._prext_isEmpty
}

/// Returns the first element of `x`, or `nil` if `x` is empty.
public func first<C: CollectionType>(x: C) -> C.Generator.Element? {
  // FIXME(prext): remove this function when protocol extensions land.
  return x._prext_first
}

/// Returns the last element of `x`, or `nil` if `x` is empty.
public func last<C: CollectionType where C.Index: BidirectionalIndexType>(
  x: C
) -> C.Generator.Element? {
  // FIXME(prext): remove this function when protocol extensions land.
  return x._prext_last
}

/// A *collection* that supports subscript assignment.
///
/// For any instance `a` of a type conforming to
/// `MutableCollectionType`, :
///
///     a[i] = x
///     let y = a[i]
///
/// is equivalent to:
///
///     a[i] = x
///     let y = x
///
public protocol MutableCollectionType : CollectionType {
  /// Access the element at `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  subscript(position: Index) -> Generator.Element {get set}
}

/// A *generator* for an arbitrary *collection*.  Provided `C`
/// conforms to the other requirements of *CollectionType*,
/// `IndexingGenerator<C>` can be used as the result of `C`'s
/// `generate()` method.  For example:
///
///      struct MyCollection : CollectionType {
///        struct Index : ForwardIndexType { *implementation hidden* }
///        subscript(i: Index) -> MyElement { *implementation hidden* }
///        func generate() -> **IndexingGenerator<MyCollection>** {
///          return IndexingGenerator(self)
///        }
///      }
public struct IndexingGenerator<
  C: _CollectionGeneratorDefaultsType
> : GeneratorType, SequenceType {
  // Because of <rdar://problem/14396120> we've had to factor
  // _CollectionType out of CollectionType to make it useful.

  /// Create a *generator* over the given collection
  public init(_ seq: C) {
    self._elements = seq
    self._position = seq.startIndex
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - complexity: O(1)
  public func generate() -> IndexingGenerator {
    return self
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: no preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> C._Element? {
    return _position == _elements.endIndex
    ? .None : .Some(_elements[_position++])
  }

  let _elements: C
  var _position: C.Index
}

/// Return the range of `x`'s valid index values.
///
/// The result's `endIndex` is the same as that of `x`.  Because
/// `Range` is half-open, iterating the values of the result produces
/// all valid subscript arguments for `x`, omitting its `endIndex`.
public func indices<
    C : CollectionType>(x: C) -> Range<C.Index> {
  // FIXME(prext): remove this function when protocol extensions land.
  return x._prext_indices
}

/// A *generator* that adapts a *collection* `C` and any *sequence* of
/// its `Index` type to present the collection's elements in a
/// permuted order.
public struct PermutationGenerator<
  C: CollectionType, Indices: SequenceType
  where C.Index == Indices.Generator.Element
> : GeneratorType, SequenceType {
  var seq : C
  var indices : Indices.Generator

  /// The type of element returned by `next()`.
  public typealias Element = C.Generator.Element

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: no preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    var result = indices.next()
    return result != nil ? seq[result!] : .None
  }

  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Generator = PermutationGenerator

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - complexity: O(1)
  public func generate() -> Generator {
    return self
  }

  /// Construct a *generator* over a permutation of `elements` given
  /// by `indices`.
  ///
  /// Requires: `elements[i]` is valid for every `i` in `indices`.
  public init(elements: C, indices: Indices) {
    self.seq = elements
    self.indices = indices.generate()
  }
}

/// This protocol is an implementation detail of `Sliceable`; do
/// not use it directly.
///
/// Its requirements are inherited by `Sliceable` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _Sliceable : CollectionType {}

/// A *collection* from which a sub-range of elements (a "slice")
/// can be efficiently extracted.
public protocol Sliceable : _Sliceable {
  // FIXME: ArraySlice should also be Sliceable but we can't express
  // that constraint (<rdar://problem/14375973> Include associated
  // type information in protocol witness tables) Instead we constrain
  // to _Sliceable; at least error messages will be more informative.

  /// The *collection* type that represents a sub-range of elements.
  ///
  /// Though it can't currently be enforced by the type system, the
  /// `SubSlice` type in a concrete implementation of `Sliceable`
  /// should also be `Sliceable`.
  typealias SubSlice : _Sliceable

  /// Access the elements delimited by the given half-open range of
  /// indices.
  ///
  /// - complexity: O(1) unless bridging from Objective-C requires an
  /// O(N) conversion.
  subscript(bounds: Range<Index>) -> SubSlice {get}
}

/// A *collection* with mutable slices.
///
/// For example,
///
///      x[i..<j] = someExpression
///      x[i..<j].mutatingMethod()
public protocol MutableSliceable : Sliceable, MutableCollectionType {
  subscript(_: Range<Index>) -> SubSlice {get set}
}

/// Return a slice containing all but the first element of `s`.
///
/// Requires: `s` is non-empty.
public func dropFirst<Seq : Sliceable>(s: Seq) -> Seq.SubSlice {
  return s[s.startIndex.successor()..<s.endIndex]
}

/// Return a slice containing all but the last element of `s`.
///
/// Requires: `s` is non-empty.
public func dropLast<
  S: Sliceable
  where S.Index: BidirectionalIndexType
>(s: S) -> S.SubSlice {
  return s[s.startIndex..<s.endIndex.predecessor()]
}

/// Return a slice, up to `maxLength` in length, containing the
/// initial elements of `s`.
///
/// If `maxLength` exceeds `count(s)`, the result contains all
/// the elements of `s`.
///
/// - complexity: O(1)+K when `S.Index` conforms to
/// `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
/// of slicing `s`.
public func prefix<S: Sliceable>(s: S, _ maxLength: Int) -> S.SubSlice {
  return s[
    s.startIndex..<advance(
      s.startIndex, max(0, numericCast(maxLength)), s.endIndex)
  ]
}

/// Return a slice, up to `maxLength` in length, containing the
/// final elements of `s`.
///
/// If `maxLength` exceeds `count(s)`, the result contains all
/// the elements of `s`.
///
/// - complexity: O(1)+K when `S.Index` conforms to
/// `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
/// of slicing `s`.
public func suffix<
  S: Sliceable where S.Index: BidirectionalIndexType
>(s: S, _ maxLength: Int) -> S.SubSlice {
  return s[
    advance(
      s.endIndex, -max(0, numericCast(maxLength)), s.startIndex)..<s.endIndex
  ]
}
