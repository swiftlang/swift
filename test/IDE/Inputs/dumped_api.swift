//===--- dump_api.swift ---------------------------------------------------===//
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
// RUN: %target-swift-ide-test -dump-api -source-filename %s > %t.swift
// RUN: diff -du %S/Inputs/dumped_api.swift %t.swift

//===--- Definitions needed only while experimental -----------------------===//
extension _InitializeTo {  }
extension _CopyToNativeArrayBuffer {  }
extension _ContiguousArrayBuffer {
}

//===----------------------------------------------------------------------===//



//===--- Generator --------------------------------------------------------===//
//===----------------------------------------------------------------------===//

public class _AnyGeneratorBase {}

/// An abstract `GeneratorType` base class over `T` elements.
///
/// Use this as a `Sequence`'s associated `Generator` type when you
/// don't want to expose details of the concrete generator, a subclass.
///
/// It is an error to create instances of `AnyGenerator` that are not
/// also instances of an `AnyGenerator` subclass.
///
/// See also:
///
///     struct AnySequence<S: SequenceType>
///     func anyGenerator<G: GeneratorType>(base: G) -> AnyGenerator<G.Element>
///     func anyGenerator<T>(nextImplementation: ()->T?) -> AnyGenerator<T>
public class AnyGenerator<T> : _AnyGeneratorBase, GeneratorType {
  /// Initialize the instance.  May only be called from a subclass
  /// initializer.
  override public init() 

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Note: subclasses must override this method.
  public func next() -> T? 
}

/// Every `GeneratorType` can also be a `SequenceType`.  Note that
/// traversing the sequence consumes the generator.
extension AnyGenerator : SequenceType {
  /// Returns `self`.
  public func generate() -> AnyGenerator 
}

/// Return a `GeneratorType` instance that wraps `base` but whose type
/// depends only on the type of `G.Element`.
///
/// Example:
///
///     func countStrings() -> AnyGenerator<String> {
///       let lazyStrings = lazy(0..<10).map { String($0) }
///
///       // This is a really complicated type of no interest to our
///       // clients.
///       let g: MapSequenceGenerator<RangeGenerator<Int>, String>
///         = lazyStrings.generate()
///       return anyGenerator(g)
///     }
public func anyGenerator<G: GeneratorType>(base: G) -> AnyGenerator<G.Element> 


/// Return a `GeneratorType` instance whose `next` method invokes
/// `nextImplementation` and returns the result.
///
/// Example:
///
///     var x = 7
///     let g = anyGenerator { x < 15 ? x++ : nil }
///     let a = Array(g) // [ 7, 8, 9, 10, 11, 12, 13, 14 ]
public func anyGenerator<T>(nextImplementation: ()->T?) -> AnyGenerator<T> 



//===--- Sequence ---------------------------------------------------------===//
//===----------------------------------------------------------------------===//



// FIXME: can't make this a protocol due to <rdar://20209031>
// FIXME: can't make this a protocol due to <rdar://20209031>

/// A type-erased sequence.
///
/// Forwards operations to an arbitrary underlying sequence having the
/// same `Element` type, hiding the specifics of the underlying
/// `SequenceType`.
///
/// See also: `AnyGenerator<T>`.
public struct AnySequence<T> : SequenceType {

  /// Wrap and forward operations to to `base`
  public init<S: SequenceType where S.Generator.Element == T>(_ base: S) 

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> 

}

public func ~> <Element>(
  source: AnySequence<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) 

public func ~> <Element>(
  source: AnySequence<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> 

public func ~> <Element>(
  source: AnyForwardCollection<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) 

public func ~> <Element>(
  source: AnyForwardCollection<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> 

public func ~> <Element>(
  source: AnyBidirectionalCollection<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) 

public func ~> <Element>(
  source: AnyBidirectionalCollection<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> 

public func ~> <Element>(
  source: AnyRandomAccessCollection<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) 

public func ~> <Element>(
  source: AnyRandomAccessCollection<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> 

//===--- ForwardIndex -----------------------------------------------------===//
//===----------------------------------------------------------------------===//



//===--- BidirectionalIndex -----------------------------------------------===//
//===----------------------------------------------------------------------===//



//===--- RandomAccessIndex -----------------------------------------------===//
//===----------------------------------------------------------------------===//



//===--- All Index Protocols ----------------------------------------------===//
//===----------------------------------------------------------------------===//


/// A wrapper over an underlying `ForwardIndexType` that hides
/// the specific underlying type.
///
/// See also: `AnyForwardCollection`
public struct AnyForwardIndex : ForwardIndexType {
  public typealias Distance = IntMax

  /// Wrap and forward operations to `base`.
  public init<BaseIndex: ForwardIndexType>(_ base: BaseIndex) 

  /// Return the next consecutive value in a discrete sequence of
  /// `AnyForwardIndex` values.
  ///
  /// Requires: `self` has a well-defined successor.
  public func successor() -> AnyForwardIndex 



  //===--- private --------------------------------------------------------===//



}

public func ~> (
  start: AnyForwardIndex, other : (_Distance, AnyForwardIndex)
) -> AnyForwardIndex.Distance 

public func ~> (
  start: AnyForwardIndex, distance : (_Advance, AnyForwardIndex.Distance)
) -> AnyForwardIndex 

public func ~> (
  start: AnyForwardIndex,
  args: (_Advance, (AnyForwardIndex.Distance, AnyForwardIndex))
) -> AnyForwardIndex 

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyForwardIndex`s.
///
/// Requires: the types of indices wrapped by `lhs` and `rhs` are
/// identical.
public func == (lhs: AnyForwardIndex, rhs: AnyForwardIndex) -> Bool 

/// A wrapper over an underlying `BidirectionalIndexType` that hides
/// the specific underlying type.
///
/// See also: `AnyBidirectionalCollection`
public struct AnyBidirectionalIndex : BidirectionalIndexType {
  public typealias Distance = IntMax

  /// Wrap and forward operations to `base`.
  public init<BaseIndex: BidirectionalIndexType>(_ base: BaseIndex) 

  /// Return the next consecutive value in a discrete sequence of
  /// `AnyBidirectionalIndex` values.
  ///
  /// Requires: `self` has a well-defined successor.
  public func successor() -> AnyBidirectionalIndex 

  /// Return the previous consecutive value in a discrete sequence of
  /// `AnyBidirectionalIndex` values.
  ///
  /// Requires: `self` has a well-defined predecessor.
  public func predecessor() -> AnyBidirectionalIndex 


  //===--- private --------------------------------------------------------===//



}

public func ~> (
  start: AnyBidirectionalIndex, other : (_Distance, AnyBidirectionalIndex)
) -> AnyBidirectionalIndex.Distance 

public func ~> (
  start: AnyBidirectionalIndex, distance : (_Advance, AnyBidirectionalIndex.Distance)
) -> AnyBidirectionalIndex 

public func ~> (
  start: AnyBidirectionalIndex,
  args: (_Advance, (AnyBidirectionalIndex.Distance, AnyBidirectionalIndex))
) -> AnyBidirectionalIndex 

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyBidirectionalIndex`s.
///
/// Requires: the types of indices wrapped by `lhs` and `rhs` are
/// identical.
public func == (lhs: AnyBidirectionalIndex, rhs: AnyBidirectionalIndex) -> Bool 

/// A wrapper over an underlying `RandomAccessIndexType` that hides
/// the specific underlying type.
///
/// See also: `AnyRandomAccessCollection`
public struct AnyRandomAccessIndex : RandomAccessIndexType {
  public typealias Distance = IntMax

  /// Wrap and forward operations to `base`.
  public init<BaseIndex: RandomAccessIndexType>(_ base: BaseIndex) 

  /// Return the next consecutive value in a discrete sequence of
  /// `AnyRandomAccessIndex` values.
  ///
  /// Requires: `self` has a well-defined successor.
  public func successor() -> AnyRandomAccessIndex 

  /// Return the previous consecutive value in a discrete sequence of
  /// `AnyRandomAccessIndex` values.
  ///
  /// Requires: `self` has a well-defined predecessor.
  public func predecessor() -> AnyRandomAccessIndex 

  /// Return the minimum number of applications of `successor` or
  /// `predecessor` required to reach `other` from `self`.
  ///
  /// Requires: `self` and `other` wrap instances of the same type.
  public func distanceTo(other: AnyRandomAccessIndex) -> Distance 

  /// Return `self` offset by `n` steps.
  ///
  /// - returns: If `n > 0`, the result of applying `successor` to
  /// `self` `n` times.  If `n < 0`, the result of applying
  /// `predecessor` to `self` `n` times. Otherwise, `self`.
  public func advancedBy(amount: Distance) -> AnyRandomAccessIndex 

  //===--- private --------------------------------------------------------===//



}

public func ~> (
  start: AnyRandomAccessIndex, other : (_Distance, AnyRandomAccessIndex)
) -> AnyRandomAccessIndex.Distance 

public func ~> (
  start: AnyRandomAccessIndex, distance : (_Advance, AnyRandomAccessIndex.Distance)
) -> AnyRandomAccessIndex 

public func ~> (
  start: AnyRandomAccessIndex,
  args: (_Advance, (AnyRandomAccessIndex.Distance, AnyRandomAccessIndex))
) -> AnyRandomAccessIndex 

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyRandomAccessIndex`s.
///
/// Requires: the types of indices wrapped by `lhs` and `rhs` are
/// identical.
public func == (lhs: AnyRandomAccessIndex, rhs: AnyRandomAccessIndex) -> Bool 

//===--- Collections ------------------------------------------------------===//
//===----------------------------------------------------------------------===//


/// A protocol for `AnyForwardCollection<T>`,
/// `AnyBidirectionalCollection<T>`, and
/// `AnyRandomAccessCollection<T>`.
///
/// This protocol can be considered an implementation detail of the
/// `===` and `!==` implementations for these types.
public protocol AnyCollectionType : CollectionType {
  /// Identifies the underlying collection stored by `self`. Instances
  /// copied from one another have the same `underlyingCollectionID`.
  var underlyingCollectionID: ObjectIdentifier {get}
}

/// Return true iff `lhs` and `rhs` store the same underlying collection.
public func === <
  L: AnyCollectionType, R: AnyCollectionType
>(lhs: L, rhs: R) -> Bool 

/// Return false iff `lhs` and `rhs` store the same underlying collection.
public func !== <
  L: AnyCollectionType, R: AnyCollectionType
>(lhs: L, rhs: R) -> Bool 

/// A type-erased wrapper over any collection with at least
/// forward indices.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// See also: `AnyBidirectionalType`, `AnyRandomAccessType`
public struct AnyForwardCollection<Element> : AnyCollectionType {

  /// Create an `AnyForwardCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: ForwardIndexType, C.Generator.Element == Element
  >(_ base: C) 

  /// Create an `AnyForwardCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyForwardCollection<Element>) 
  /// Create an `AnyForwardCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: BidirectionalIndexType, C.Generator.Element == Element
  >(_ base: C) 

  /// Create an `AnyForwardCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyBidirectionalCollection<Element>) 
  /// Create an `AnyForwardCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: RandomAccessIndexType, C.Generator.Element == Element
  >(_ base: C) 

  /// Create an `AnyForwardCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyRandomAccessCollection<Element>) 


  /// Return a *generator* over the elements of this *collection*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> 

  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public var startIndex: AnyForwardIndex 

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: AnyForwardIndex 

  /// Access the element indicated by `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: AnyForwardIndex) -> Element 

  /// Uniquely identifies the stored underlying collection.
  public var underlyingCollectionID: ObjectIdentifier 

}
/// A type-erased wrapper over any collection with at least
/// bidirectional indices.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// See also: `AnyRandomAccessType`, `AnyForwardType`
public struct AnyBidirectionalCollection<Element> : AnyCollectionType {

  /// Create an `AnyBidirectionalCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: BidirectionalIndexType, C.Generator.Element == Element
  >(_ base: C) 

  /// Create an `AnyBidirectionalCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyBidirectionalCollection<Element>) 
  /// Create an `AnyBidirectionalCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: RandomAccessIndexType, C.Generator.Element == Element
  >(_ base: C) 

  /// Create an `AnyBidirectionalCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyRandomAccessCollection<Element>) 

  /// If the indices of the underlying collection stored by `other`
  /// satisfy `BidirectionalIndexType`, create an
  /// `AnyBidirectionalCollection` having the same underlying
  /// collection as `other`.  Otherwise, the result is `nil`.
  ///
  /// Complexity: O(1)
  public init?(_ other: AnyForwardCollection<Element>) 

  /// Return a *generator* over the elements of this *collection*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> 

  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public var startIndex: AnyBidirectionalIndex 

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: AnyBidirectionalIndex 

  /// Access the element indicated by `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: AnyBidirectionalIndex) -> Element 

  /// Uniquely identifies the stored underlying collection.
  public var underlyingCollectionID: ObjectIdentifier 

}
/// A type-erased wrapper over any collection with at least
/// randomaccess indices.
///
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// See also: `AnyForwardType`, `AnyBidirectionalType`
public struct AnyRandomAccessCollection<Element> : AnyCollectionType {

  /// Create an `AnyRandomAccessCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: RandomAccessIndexType, C.Generator.Element == Element
  >(_ base: C) 

  /// Create an `AnyRandomAccessCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyRandomAccessCollection<Element>) 

  /// If the indices of the underlying collection stored by `other`
  /// satisfy `RandomAccessIndexType`, create an
  /// `AnyRandomAccessCollection` having the same underlying
  /// collection as `other`.  Otherwise, the result is `nil`.
  ///
  /// Complexity: O(1)
  public init?(_ other: AnyForwardCollection<Element>) 
  /// If the indices of the underlying collection stored by `other`
  /// satisfy `RandomAccessIndexType`, create an
  /// `AnyRandomAccessCollection` having the same underlying
  /// collection as `other`.  Otherwise, the result is `nil`.
  ///
  /// Complexity: O(1)
  public init?(_ other: AnyBidirectionalCollection<Element>) 

  /// Return a *generator* over the elements of this *collection*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> 

  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public var startIndex: AnyRandomAccessIndex 

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: AnyRandomAccessIndex 

  /// Access the element indicated by `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: AnyRandomAccessIndex) -> Element 

  /// Uniquely identifies the stored underlying collection.
  public var underlyingCollectionID: ObjectIdentifier 

}

