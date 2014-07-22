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

public struct _CountElements {}
internal func _countElements<Args>(a: Args) -> (_CountElements, Args) {
  return (_CountElements(), a)
}
// Default implementation of countElements for Collections
// Do not use this operator directly; call countElements(x) instead
public func ~> <T: _CollectionType>(x:T, _:(_CountElements,()))
  -> T.Index.Distance
{
  return distance(x.startIndex, x.endIndex)
}

/// Return the number of elements in x.  O(1) if T.Index is
/// RandomAccessIndexType; O(N) otherwise.
public func countElements <T: _CollectionType>(x: T) -> T.Index.Distance {
  return x~>_countElements()
}

public protocol _CollectionType : _SequenceType {
  typealias Index : ForwardIndexType

  var startIndex: Index {get}
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
  subscript(i: Index) -> _Element {get}
}

public protocol CollectionType : _CollectionType, SequenceType {
  subscript(i: Index) -> Generator.Element {get}
  
  // Do not use this operator directly; call countElements(x) instead
  func ~> (_:Self, _:(_CountElements, ())) -> Index.Distance
}

// Default implementation of underestimateCount for Collections.  Do not
// use this operator directly; call underestimateCount(s) instead
public func ~> <T: _CollectionType>(x:T,_:(_UnderestimateCount,())) -> Int {
  return numericCast(x~>_countElements())
}

// Default implementation of `preprocessingPass` for Collections.  Do not
// use this operator directly; call `_preprocessingPass(s)` instead
public func ~> <T: _CollectionType, R>(
  s: T, args: (_PreprocessingPass, ( (T)->R ))
) -> R? {
  return args.1(s)
}

/// Returns `true` iff `x` is empty.
public func isEmpty<C: CollectionType>(x: C) -> Bool {
  return x.startIndex == x.endIndex
}

/// Returns the first element of `x`, or `nil` if `x` is empty.
public func first<C: CollectionType>(x: C) -> C.Generator.Element? {
  return isEmpty(x) ? nil : x[x.startIndex]
}

/// Returns the last element of `x`, or `nil` if `x` is empty.
public func last<C: CollectionType where C.Index: BidirectionalIndexType>(
  x: C
) -> C.Generator.Element? {
  return isEmpty(x) ? nil : x[x.endIndex.predecessor()]
}

public protocol MutableCollectionType : CollectionType {
  subscript(i: Index) -> Generator.Element {get set}
}

/// A stream type that could serve for a CollectionType given that
/// it already had an Index.
public struct IndexingGenerator<
  C: _CollectionType
> : GeneratorType, SequenceType {
  // Because of <rdar://problem/14396120> we've had to factor
  // _CollectionType out of CollectionType to make it useful.

  public init(_ seq: C) {
    self._elements = seq
    self._position = seq.startIndex
  }
  
  public func generate() -> IndexingGenerator {
    return self
  }
  
  public mutating func next() -> C._Element? {
    return _position == _elements.endIndex
    ? .None : .Some(_elements[_position++])
  }
  var _elements: C
  var _position: C.Index
}

public func indices<
    Seq : CollectionType>(seq: Seq) -> Range<Seq.Index> {
  return Range(start: seq.startIndex, end: seq.endIndex)
}

public struct PermutationGenerator<
  C: CollectionType, Indices: SequenceType
  where C.Index == Indices.Generator.Element
> : GeneratorType, SequenceType {
  var seq : C
  var indices : Indices.Generator

  public typealias Element = C.Generator.Element

  public mutating func next() -> Element? {
    var result = indices.next()
    return result ? seq[result!] : .None
  }

  // Every GeneratorType is also a single-pass SequenceType
  public typealias Generator = PermutationGenerator
  public func generate() -> Generator {
    return self
  }

  public init(elements seq: C, indices: Indices) {
    self.seq = seq
    self.indices = indices.generate()
  }
}

public protocol _Sliceable : CollectionType {}

public protocol Sliceable : _Sliceable {
  // FIXME: Slice should also be Sliceable but we can't express
  // that constraint (<rdar://problem/14375973> Include associated
  // type information in protocol witness tables) Instead we constrain
  // to _Sliceable; at least error messages will be more informative.
  typealias SubSlice: _Sliceable
  
  /// Returns a `CollectionType` containing the elements delimited by
  /// the given half-open range of indices.  Should be O(1) unless
  /// bridging from Objective-C requires an O(N) conversion.
  subscript(bounds: Range<Index>) -> SubSlice {get}
}

public protocol MutableSliceable : Sliceable, MutableCollectionType {
  subscript(_: Range<Index>) -> SubSlice {get set}
}

/// Return a slice containing all but the first element of `s`.
/// Requires: `s` is non-empty.
public func dropFirst<Seq : Sliceable>(s: Seq) -> Seq.SubSlice {
  return s[s.startIndex.successor()..<s.endIndex]
}

/// Return a slice containing all but the last element of `s`.
/// Requires: `s` is non-empty.
public func dropLast<
  S: Sliceable 
  where S.Index: BidirectionalIndexType
>(s: S) -> S.SubSlice {
  return s[s.startIndex..<s.endIndex.predecessor()]
}

/// Return a slice, up to `maxLength` in length, containing the
/// initial elements of `s`.  O(1)+K when `S.Index` conforms to
/// `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
/// of slicing `s`.
public func prefix<S: Sliceable>(s: S, maxLength: Int) -> S.SubSlice {
  return s[
    s.startIndex..<advance(
      s.startIndex, max(0, numericCast(maxLength)), s.endIndex)
  ]
}

/// Return a slice, up to `maxLength` in length, containing the
/// final elements of `s`. O(1)+K when `S.Index` conforms to
/// `RandomAccessIndexType` and O(N)+K otherwise, where K is the cost
/// of slicing `s`.
public func suffix<
  S: Sliceable where S.Index: BidirectionalIndexType
>(s: S, maxLength: Int) -> S.SubSlice {
  return s[
    advance(
      s.endIndex, -max(0, numericCast(maxLength)), s.startIndex)..<s.endIndex
  ]
}
