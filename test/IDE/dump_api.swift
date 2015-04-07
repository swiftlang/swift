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
internal typealias _ContiguousArrayStorageBase = AnyObject
extension _InitializeTo { init() {} }
extension _CopyToNativeArrayBuffer { init() {} }
extension _Count { init() {} }
extension _ContiguousArrayBuffer {
  var _storage: _ContiguousArrayStorageBase { return owner }
  init(_ owner: _ContiguousArrayStorageBase) {
    self = unsafeBitCast(owner, _ContiguousArrayBuffer.self)
  }
}

//===----------------------------------------------------------------------===//


@noreturn @inline(never)
internal func _abstract(file: StaticString = __FILE__, line: UWord = __LINE__) {
  fatalError("Method must be overridden", file: file, line: line)
}

//===--- Generator --------------------------------------------------------===//
//===----------------------------------------------------------------------===//

public class _AnyGeneratorBase {}

/// An abstract `GeneratorType` base class over `T` elements.
///
/// Use this as a `Sequence`\ 's associated `Generator` type when you
/// don't want to expose details of the concrete generator, a subclass.
///
/// It is an error to create instances of `AnyGenerator` that are not
/// also instances of an `AnyGenerator` subclass.
///
/// See also::
///
///   struct AnySequence<S: SequenceType>
///   func anyGenerator<G: GeneratorType>(base: G) -> AnyGenerator<G.Element>
///   func anyGenerator<T>(nextImplementation: ()->T?) -> AnyGenerator<T>
public class AnyGenerator<T> : _AnyGeneratorBase, GeneratorType {
  /// Initialize the instance.  May only be called from a subclass
  /// initializer.
  override public init() {
    super.init()
    _debugPrecondition(
      _typeID(self) != unsafeBitCast(AnyGenerator.self, ObjectIdentifier.self),
      "AnyGenerator<T> instances can not be created; create a subclass instance instead."
    )
  }
  
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Note: subclasses must override this method.
  public func next() -> T? {_abstract()}
}

/// Every `GeneratorType` can also be a `SequenceType`.  Note that
/// traversing the sequence consumes the generator.
extension AnyGenerator : SequenceType {
  /// Returns `self`.
  public func generate() -> AnyGenerator { return self }
}

/// Return a `GeneratorType` instance that wraps `base` but whose type
/// depends only on the type of `G.Element`.
///
/// Example::
///
///   func countStrings() -> AnyGenerator<String> {
///     let lazyStrings = lazy(0..<10).map { String($0) }
///
///     // This is a really complicated type of no interest to our
///     // clients.
///     let g: MapSequenceGenerator<RangeGenerator<Int>, String>
///       = lazyStrings.generate()
///     return anyGenerator(g)
///   }
public func anyGenerator<G: GeneratorType>(base: G) -> AnyGenerator<G.Element> {
  return _GeneratorBox(base)
}

internal class _FunctionGenerator<T> : AnyGenerator<T> {
  init(_ nextImplementation: ()->T?) {
    self.nextImplementation = nextImplementation
  }
  override func next() -> T? { return nextImplementation() }
  let nextImplementation: ()->T?
}

/// Return a `GeneratorType` instance whose `next` method invokes
/// `nextImplementation` and returns the result.
///
/// Example::
///
///   var x = 7
///   let g = anyGenerator { x < 15 ? x++ : nil }
///   let a = Array(g) // [ 7, 8, 9, 10, 11, 12, 13, 14 ]
public func anyGenerator<T>(nextImplementation: ()->T?) -> AnyGenerator<T> {
  return _FunctionGenerator(nextImplementation)
}

internal final class _GeneratorBox<
  Base: GeneratorType
> : AnyGenerator<Base.Element> {
  init(_ base: Base) { self.base = base }
  override func next() -> Base.Element? { return base.next() }
  var base: Base
}

internal func _typeID(instance: AnyObject) -> ObjectIdentifier {
  return ObjectIdentifier(instance.dynamicType)
}

//===--- Sequence ---------------------------------------------------------===//
//===----------------------------------------------------------------------===//

internal class _AnySequenceBox {
  // FIXME: can't make _AnySequenceBox generic and return
  // _AnyGenerator<T> here due to <rdar://20211022>
  func generate() -> _AnyGeneratorBase {_abstract()}

  func _underestimateCount() -> Int  {_abstract()}
  // FIXME: can't traffic in UnsafeMutablePointer<T> and
  // _ContiguousArrayBuffer<T> here due to <rdar://20164041>
  func _initializeTo(ptr: UnsafeMutablePointer<Void>) {_abstract()}
  func _copyToNativeArrayBuffer() -> _ContiguousArrayStorageBase {_abstract()}
}

internal class _AnyCollectionBoxBase : _AnySequenceBox {
  init(startIndex: _ForwardIndexBoxType, endIndex: _ForwardIndexBoxType) {
    self.startIndex = startIndex
    self.endIndex = endIndex
  }
  let startIndex: _ForwardIndexBoxType
  let endIndex: _ForwardIndexBoxType
}

// FIXME: can't make this a protocol due to <rdar://20209031>
internal class _SequenceBox<S: SequenceType>
  : _AnySequenceBox {
  typealias Element = S.Generator.Element
  
  override func generate() -> _AnyGeneratorBase {
    return _GeneratorBox(_base.generate())
  }
  override func _underestimateCount() -> Int {
    return Swift.underestimateCount(_base)
  }
  override func _initializeTo(ptr: UnsafeMutablePointer<Void>) {
    _base~>(_InitializeTo(), UnsafeMutablePointer(ptr))
  }
  override func _copyToNativeArrayBuffer() -> _ContiguousArrayStorageBase {
    return (_base~>(_CopyToNativeArrayBuffer(), ()))._storage
  }
  init(_ base: S) {
    self._base = base
  }
  internal var _base: S
}
// FIXME: can't make this a protocol due to <rdar://20209031>
internal class _CollectionBox<S: CollectionType>
  : _AnyCollectionBox<S.Generator.Element> {
  typealias Element = S.Generator.Element
  
  override func generate() -> _AnyGeneratorBase {
    return _GeneratorBox(_base.generate())
  }
  override func _underestimateCount() -> Int {
    return Swift.underestimateCount(_base)
  }
  override func _initializeTo(ptr: UnsafeMutablePointer<Void>) {
    _base~>(_InitializeTo(), UnsafeMutablePointer(ptr))
  }
  override func _copyToNativeArrayBuffer() -> _ContiguousArrayStorageBase {
    return (_base~>(_CopyToNativeArrayBuffer(), ()))._storage
  }
  override func _count() -> IntMax {
    return numericCast(_base~>(_Count(), ()))
  }
  override subscript(position: _ForwardIndexBoxType) -> Element {
    if let i? = position._unbox() as S.Index? {
      return _base[i]
    }
    fatalError("Index type mismatch!")
  }
  init(
    _ base: S,
    startIndex: _ForwardIndexBoxType,
    endIndex: _ForwardIndexBoxType
  ) {
    self._base = base
    super.init(startIndex: startIndex, endIndex: endIndex)
  }
  internal var _base: S
}

/// A type-erased sequence.
/// 
/// Forwards operations to an arbitrary underlying sequence having the
/// same `Element` type, hiding the specifics of the underlying
/// `SequenceType`.
///
/// See also: `AnyGenerator<T>`.
public struct AnySequence<T> : SequenceType {
  typealias Element = T

  /// Wrap and forward operations to to `base`
  public init<S: SequenceType where S.Generator.Element == T>(_ base: S) {
    _box = _SequenceBox(base)
  }
  
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> {
    return unsafeDowncast(_box.generate())
  }
  
  internal let _box: _AnySequenceBox
}

public func ~> <Element>(
  source: AnySequence<Element>, _: (_UnderestimateCount, ())
) -> Int {
  return source._box._underestimateCount()
}

public func ~> <Element>(
  source: AnySequence<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) {
  source._box._initializeTo(UnsafeMutablePointer(ptr.1))
}

public func ~> <Element>(
  source: AnySequence<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> {
  return _ContiguousArrayBuffer(source._box._copyToNativeArrayBuffer())
}

public func ~> <Element>(
  source: AnyForwardCollection<Element>, _: (_UnderestimateCount, ())
) -> Int {
  return source._box._underestimateCount()
}

public func ~> <Element>(
  source: AnyForwardCollection<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) {
  source._box._initializeTo(UnsafeMutablePointer(ptr.1))
}

public func ~> <Element>(
  source: AnyForwardCollection<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> {
  return _ContiguousArrayBuffer(source._box._copyToNativeArrayBuffer())
}

public func ~> <Element>(
  source: AnyForwardCollection<Element>, _: (_Count,())
) -> IntMax {
  return source._box._count()
}
public func ~> <Element>(
  source: AnyBidirectionalCollection<Element>, _: (_UnderestimateCount, ())
) -> Int {
  return source._box._underestimateCount()
}

public func ~> <Element>(
  source: AnyBidirectionalCollection<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) {
  source._box._initializeTo(UnsafeMutablePointer(ptr.1))
}

public func ~> <Element>(
  source: AnyBidirectionalCollection<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> {
  return _ContiguousArrayBuffer(source._box._copyToNativeArrayBuffer())
}

public func ~> <Element>(
  source: AnyBidirectionalCollection<Element>, _: (_Count,())
) -> IntMax {
  return source._box._count()
}
public func ~> <Element>(
  source: AnyRandomAccessCollection<Element>, _: (_UnderestimateCount, ())
) -> Int {
  return source._box._underestimateCount()
}

public func ~> <Element>(
  source: AnyRandomAccessCollection<Element>,
  ptr: (_InitializeTo, UnsafeMutablePointer<Element>)
) {
  source._box._initializeTo(UnsafeMutablePointer(ptr.1))
}

public func ~> <Element>(
  source: AnyRandomAccessCollection<Element>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<Element> {
  return _ContiguousArrayBuffer(source._box._copyToNativeArrayBuffer())
}

public func ~> <Element>(
  source: AnyRandomAccessCollection<Element>, _: (_Count,())
) -> IntMax {
  return source._box._count()
}

//===--- ForwardIndex -----------------------------------------------------===//
//===----------------------------------------------------------------------===//

internal protocol _ForwardIndexBoxType : class {
  var typeID: ObjectIdentifier {get}
  func successor() -> _ForwardIndexBoxType
  func equals(other: _ForwardIndexBoxType) -> Bool
  func _unbox<T: ForwardIndexType>() -> T?
  func _distanceTo(other: _ForwardIndexBoxType) -> AnyForwardIndex.Distance
  // FIXME: Can't return Self from _advancedBy pending <rdar://20181253>
  func _advancedBy(distance: AnyForwardIndex.Distance) -> _ForwardIndexBoxType
  func _advancedBy(
    distance: AnyForwardIndex.Distance,
    _ limit: _ForwardIndexBoxType
  ) -> _ForwardIndexBoxType
}

internal class _ForwardIndexBox<
  BaseIndex: ForwardIndexType
> : _ForwardIndexBoxType {
  required init(_ base: BaseIndex) {
    self.base = base
  }
  
  func successor() -> _ForwardIndexBoxType {
    return self.dynamicType(self.base.successor())
  }
  
  func unsafeUnbox(other: _ForwardIndexBoxType) -> BaseIndex {
    return (unsafeDowncast(other) as _ForwardIndexBox).base
  }
  
  func equals(other: _ForwardIndexBoxType) -> Bool {
    return base == unsafeUnbox(other)
  }

  func _distanceTo(other: _ForwardIndexBoxType) -> AnyForwardIndex.Distance {
    return numericCast(distance(base, unsafeUnbox(other)))
  }
  
  func _advancedBy(n: AnyForwardIndex.Distance) -> _ForwardIndexBoxType {
    return self.dynamicType(advance(base, numericCast(n)))
  }
  
  func _advancedBy(
    n: AnyForwardIndex.Distance,
    _ limit: _ForwardIndexBoxType
  ) -> _ForwardIndexBoxType {
    return self.dynamicType(advance(base, numericCast(n), unsafeUnbox(limit)))
  }

  func _unbox<T: ForwardIndexType>() -> T? {
    if T.self is BaseIndex.Type {
      _sanityCheck(BaseIndex.self is T.Type)
      // This bit cast is really nothing as we have proven they are
      // the same type.
      return unsafeBitCast(base, T.self) 
    }
    return nil
  }

  var typeID: ObjectIdentifier { return _typeID(self) }
  
  internal // private
  let base: BaseIndex
}

//===--- BidirectionalIndex -----------------------------------------------===//
//===----------------------------------------------------------------------===//

internal protocol _BidirectionalIndexBoxType : _ForwardIndexBoxType {
  func predecessor() -> _BidirectionalIndexBoxType
}

internal class _BidirectionalIndexBox<
  BaseIndex: BidirectionalIndexType
> : _ForwardIndexBox<BaseIndex>, _BidirectionalIndexBoxType {
  required init(_ base: BaseIndex) {
    super.init(base)
  }
  
  override func successor() -> _ForwardIndexBoxType {
    return self.dynamicType(self.base.successor())
  }
  
  func predecessor() -> _BidirectionalIndexBoxType {
    return self.dynamicType(self.base.predecessor())
  }
}

//===--- RandomAccessIndex -----------------------------------------------===//
//===----------------------------------------------------------------------===//

internal protocol _RandomAccessIndexBoxType : _BidirectionalIndexBoxType {}

internal final class _RandomAccessIndexBox<
  BaseIndex: RandomAccessIndexType
> : _BidirectionalIndexBox<BaseIndex>, _RandomAccessIndexBoxType {
  required init(_ base: BaseIndex) {
    super.init(base)
  }
}

//===--- All Index Protocols ----------------------------------------------===//
//===----------------------------------------------------------------------===//


/// A wrapper over an underlying `ForwardIndexType` that hides
/// the specific underlying type.
///
/// See also: `AnyForwardCollection`
public struct AnyForwardIndex : ForwardIndexType {
  public typealias Distance = IntMax

  /// Wrap and forward operations to `base`.
  public init<BaseIndex: ForwardIndexType>(_ base: BaseIndex) {
    _box = _ForwardIndexBox(base)
  }
  
  /// Return the next consecutive value in a discrete sequence of
  /// `AnyForwardIndex` values.
  ///
  /// Requires: `self` has a well-defined successor.
  public func successor() -> AnyForwardIndex {
    return AnyForwardIndex(_box.successor())
  }
  
  
  
  //===--- private --------------------------------------------------------===//
  
  internal var _typeID: ObjectIdentifier {
    return _box.typeID
  }
  
  internal init(_ box: _ForwardIndexBoxType) {
    self._box = box
  }
  
  internal let _box: _ForwardIndexBoxType
}

public func ~> (
  start: AnyForwardIndex, other : (_Distance, AnyForwardIndex)
) -> AnyForwardIndex.Distance {
  precondition(
    start._typeID == other.1._typeID,
    "distance: base index types differ.")
  return start._box._distanceTo(other.1._box)
}

public func ~> (
  start: AnyForwardIndex, distance : (_Advance, AnyForwardIndex.Distance)
) -> AnyForwardIndex {
  return AnyForwardIndex(start._box._advancedBy(distance.1))
}

public func ~> (
  start: AnyForwardIndex,
  args: (_Advance, (AnyForwardIndex.Distance, AnyForwardIndex))
) -> AnyForwardIndex {
  precondition(
    start._typeID == args.1.1._typeID, "advance: base index types differ.")
  return AnyForwardIndex(start._box._advancedBy(args.1.0, args.1.1._box))
}

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyForwardIndex`\ s.
///
/// Requires: the types of indices wrapped by `lhs` and `rhs` are
/// identical.
public func == (lhs: AnyForwardIndex, rhs: AnyForwardIndex) -> Bool {
  precondition(lhs._typeID == rhs._typeID, "base index types differ.")
  return lhs._box.equals(rhs._box)
}

/// A wrapper over an underlying `BidirectionalIndexType` that hides
/// the specific underlying type.
///
/// See also: `AnyBidirectionalCollection`
public struct AnyBidirectionalIndex : BidirectionalIndexType {
  public typealias Distance = IntMax

  /// Wrap and forward operations to `base`.
  public init<BaseIndex: BidirectionalIndexType>(_ base: BaseIndex) {
    _box = _BidirectionalIndexBox(base)
  }
  
  /// Return the next consecutive value in a discrete sequence of
  /// `AnyBidirectionalIndex` values.
  ///
  /// Requires: `self` has a well-defined successor.
  public func successor() -> AnyBidirectionalIndex {
    return AnyBidirectionalIndex(_box.successor())
  }
  
  /// Return the previous consecutive value in a discrete sequence of
  /// `AnyBidirectionalIndex` values.
  ///
  /// Requires: `self` has a well-defined predecessor.
  public func predecessor() -> AnyBidirectionalIndex {
    return AnyBidirectionalIndex(_box.predecessor())
  }
  
  
  //===--- private --------------------------------------------------------===//
  
  internal var _typeID: ObjectIdentifier {
    return _box.typeID
  }
  
  internal init(_ box: _ForwardIndexBoxType) {
    self._box = box as! _BidirectionalIndexBoxType
  }
  
  internal let _box: _BidirectionalIndexBoxType
}

public func ~> (
  start: AnyBidirectionalIndex, other : (_Distance, AnyBidirectionalIndex)
) -> AnyBidirectionalIndex.Distance {
  precondition(
    start._typeID == other.1._typeID,
    "distance: base index types differ.")
  return start._box._distanceTo(other.1._box)
}

public func ~> (
  start: AnyBidirectionalIndex, distance : (_Advance, AnyBidirectionalIndex.Distance)
) -> AnyBidirectionalIndex {
  return AnyBidirectionalIndex(start._box._advancedBy(distance.1))
}

public func ~> (
  start: AnyBidirectionalIndex,
  args: (_Advance, (AnyBidirectionalIndex.Distance, AnyBidirectionalIndex))
) -> AnyBidirectionalIndex {
  precondition(
    start._typeID == args.1.1._typeID, "advance: base index types differ.")
  return AnyBidirectionalIndex(start._box._advancedBy(args.1.0, args.1.1._box))
}

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyBidirectionalIndex`\ s.
///
/// Requires: the types of indices wrapped by `lhs` and `rhs` are
/// identical.
public func == (lhs: AnyBidirectionalIndex, rhs: AnyBidirectionalIndex) -> Bool {
  precondition(lhs._typeID == rhs._typeID, "base index types differ.")
  return lhs._box.equals(rhs._box)
}

/// A wrapper over an underlying `RandomAccessIndexType` that hides
/// the specific underlying type.
///
/// See also: `AnyRandomAccessCollection`
public struct AnyRandomAccessIndex : RandomAccessIndexType {
  public typealias Distance = IntMax

  /// Wrap and forward operations to `base`.
  public init<BaseIndex: RandomAccessIndexType>(_ base: BaseIndex) {
    _box = _RandomAccessIndexBox(base)
  }
  
  /// Return the next consecutive value in a discrete sequence of
  /// `AnyRandomAccessIndex` values.
  ///
  /// Requires: `self` has a well-defined successor.
  public func successor() -> AnyRandomAccessIndex {
    return AnyRandomAccessIndex(_box.successor())
  }
  
  /// Return the previous consecutive value in a discrete sequence of
  /// `AnyRandomAccessIndex` values.
  ///
  /// Requires: `self` has a well-defined predecessor.
  public func predecessor() -> AnyRandomAccessIndex {
    return AnyRandomAccessIndex(_box.predecessor())
  }
  
  /// Return the minimum number of applications of `successor` or
  /// `predecessor` required to reach `other` from `self`.
  ///
  /// Requires: `self` and `other` wrap instances of the same type.
  public func distanceTo(other: AnyRandomAccessIndex) -> Distance {
    return _box._distanceTo(other._box)
  }
  
  /// Return `self` offset by `n` steps.
  ///
  /// :returns: If `n > 0`, the result of applying `successor` to
  /// `self` `n` times.  If `n < 0`, the result of applying
  /// `predecessor` to `self` `n` times. Otherwise, `self`.
  public func advancedBy(amount: Distance) -> AnyRandomAccessIndex {
    return AnyRandomAccessIndex(_box._advancedBy(amount))
  }
  
  //===--- private --------------------------------------------------------===//
  
  internal var _typeID: ObjectIdentifier {
    return _box.typeID
  }
  
  internal init(_ box: _ForwardIndexBoxType) {
    self._box = box as! _RandomAccessIndexBoxType
  }
  
  internal let _box: _RandomAccessIndexBoxType
}

public func ~> (
  start: AnyRandomAccessIndex, other : (_Distance, AnyRandomAccessIndex)
) -> AnyRandomAccessIndex.Distance {
  precondition(
    start._typeID == other.1._typeID,
    "distance: base index types differ.")
  return start._box._distanceTo(other.1._box)
}

public func ~> (
  start: AnyRandomAccessIndex, distance : (_Advance, AnyRandomAccessIndex.Distance)
) -> AnyRandomAccessIndex {
  return AnyRandomAccessIndex(start._box._advancedBy(distance.1))
}

public func ~> (
  start: AnyRandomAccessIndex,
  args: (_Advance, (AnyRandomAccessIndex.Distance, AnyRandomAccessIndex))
) -> AnyRandomAccessIndex {
  precondition(
    start._typeID == args.1.1._typeID, "advance: base index types differ.")
  return AnyRandomAccessIndex(start._box._advancedBy(args.1.0, args.1.1._box))
}

/// Return true iff `lhs` and `rhs` wrap equal underlying
/// `AnyRandomAccessIndex`\ s.
///
/// Requires: the types of indices wrapped by `lhs` and `rhs` are
/// identical.
public func == (lhs: AnyRandomAccessIndex, rhs: AnyRandomAccessIndex) -> Bool {
  precondition(lhs._typeID == rhs._typeID, "base index types differ.")
  return lhs._box.equals(rhs._box)
}

//===--- Collections ------------------------------------------------------===//
//===----------------------------------------------------------------------===//

internal class _AnyCollectionBox<Element> : _AnyCollectionBoxBase {
  subscript(_ForwardIndexBoxType) -> Element {_abstract()}
  func _count() -> IntMax {_abstract()}

  // FIXME: should be inherited, but a known bug prevents it since
  // this class is generic.
  override init(
    startIndex: _ForwardIndexBoxType,
    endIndex: _ForwardIndexBoxType
  ) {
    super.init(startIndex: startIndex, endIndex: endIndex)
  }
}

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
>(lhs: L, rhs: R) -> Bool {
  return lhs.underlyingCollectionID == rhs.underlyingCollectionID
}

/// Return false iff `lhs` and `rhs` store the same underlying collection.
public func !== <
  L: AnyCollectionType, R: AnyCollectionType
>(lhs: L, rhs: R) -> Bool {
  return lhs.underlyingCollectionID != rhs.underlyingCollectionID
}

/// A type-erased wrapper over any collection with at least
/// forward indices.
/// 
/// Forwards operations to an arbitrary underlying collection having the
/// same `Element` type, hiding the specifics of the underlying
/// `CollectionType`.
///
/// See also: `AnyBidirectionalType`, `AnyRandomAccessType`
public struct AnyForwardCollection<Element> : AnyCollectionType {
  typealias Box = _AnyCollectionBox<Element>

  /// Create an `AnyForwardCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: ForwardIndexType, C.Generator.Element == Element
  >(_ base: C) {
    self._box = _CollectionBox<C>(
      base,
      startIndex: _ForwardIndexBox(base.startIndex),
      endIndex: _ForwardIndexBox(base.endIndex))
  }

  /// Create an `AnyForwardCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyForwardCollection<Element>) {
    self._box = other._box
  }
  /// Create an `AnyForwardCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: BidirectionalIndexType, C.Generator.Element == Element
  >(_ base: C) {
    self._box = _CollectionBox<C>(
      base,
      startIndex: _BidirectionalIndexBox(base.startIndex),
      endIndex: _BidirectionalIndexBox(base.endIndex))
  }

  /// Create an `AnyForwardCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyBidirectionalCollection<Element>) {
    self._box = other._box
  }
  /// Create an `AnyForwardCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: RandomAccessIndexType, C.Generator.Element == Element
  >(_ base: C) {
    self._box = _CollectionBox<C>(
      base,
      startIndex: _RandomAccessIndexBox(base.startIndex),
      endIndex: _RandomAccessIndexBox(base.endIndex))
  }

  /// Create an `AnyForwardCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyRandomAccessCollection<Element>) {
    self._box = other._box
  }

  
  /// Return a *generator* over the elements of this *collection*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> {
    return unsafeDowncast(_box.generate())
  }
  
  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public var startIndex: AnyForwardIndex {
    return AnyForwardIndex(_box.startIndex)
  }
  
  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: AnyForwardIndex {
    return AnyForwardIndex(_box.endIndex)
  }
  
  /// Access the element indicated by `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: AnyForwardIndex) -> Element {
    return _box[position._box]
  }

  /// Uniquely identifies the stored underlying collection.
  public var underlyingCollectionID: ObjectIdentifier {
    return ObjectIdentifier(_box)
  }
  
  internal let _box: Box
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
  typealias Box = _AnyCollectionBox<Element>

  /// Create an `AnyBidirectionalCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: BidirectionalIndexType, C.Generator.Element == Element
  >(_ base: C) {
    self._box = _CollectionBox<C>(
      base,
      startIndex: _BidirectionalIndexBox(base.startIndex),
      endIndex: _BidirectionalIndexBox(base.endIndex))
  }

  /// Create an `AnyBidirectionalCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyBidirectionalCollection<Element>) {
    self._box = other._box
  }
  /// Create an `AnyBidirectionalCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: RandomAccessIndexType, C.Generator.Element == Element
  >(_ base: C) {
    self._box = _CollectionBox<C>(
      base,
      startIndex: _RandomAccessIndexBox(base.startIndex),
      endIndex: _RandomAccessIndexBox(base.endIndex))
  }

  /// Create an `AnyBidirectionalCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyRandomAccessCollection<Element>) {
    self._box = other._box
  }

  /// If the indices of the underlying collection stored by `other`
  /// satisfy `BidirectionalIndexType`, create an
  /// `AnyBidirectionalCollection` having the same underlying
  /// collection as `other`.  Otherwise, the result is `nil`.
  ///
  /// Complexity: O(1)
  public init?(_ other: AnyForwardCollection<Element>) {
    if !(other._box.startIndex is _BidirectionalIndexBoxType) {
      return nil
    }
    _sanityCheck(other._box.endIndex is _BidirectionalIndexBoxType)
    self._box = other._box
  }
  
  /// Return a *generator* over the elements of this *collection*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> {
    return unsafeDowncast(_box.generate())
  }
  
  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public var startIndex: AnyBidirectionalIndex {
    return AnyBidirectionalIndex(_box.startIndex)
  }
  
  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: AnyBidirectionalIndex {
    return AnyBidirectionalIndex(_box.endIndex)
  }
  
  /// Access the element indicated by `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: AnyBidirectionalIndex) -> Element {
    return _box[position._box]
  }

  /// Uniquely identifies the stored underlying collection.
  public var underlyingCollectionID: ObjectIdentifier {
    return ObjectIdentifier(_box)
  }
  
  internal let _box: Box
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
  typealias Box = _AnyCollectionBox<Element>

  /// Create an `AnyRandomAccessCollection` that stores `base` as its
  /// underlying collection.
  ///
  /// Complexity: O(1)
  public init<
    C: CollectionType
      where C.Index: RandomAccessIndexType, C.Generator.Element == Element
  >(_ base: C) {
    self._box = _CollectionBox<C>(
      base,
      startIndex: _RandomAccessIndexBox(base.startIndex),
      endIndex: _RandomAccessIndexBox(base.endIndex))
  }

  /// Create an `AnyRandomAccessCollection` having the same underlying
  /// collection as `other`.
  ///
  /// Postcondition: the result is `===` to `other`.
  ///
  /// Complexity: O(1)
  public init(_ other: AnyRandomAccessCollection<Element>) {
    self._box = other._box
  }

  /// If the indices of the underlying collection stored by `other`
  /// satisfy `RandomAccessIndexType`, create an
  /// `AnyRandomAccessCollection` having the same underlying
  /// collection as `other`.  Otherwise, the result is `nil`.
  ///
  /// Complexity: O(1)
  public init?(_ other: AnyForwardCollection<Element>) {
    if !(other._box.startIndex is _RandomAccessIndexBoxType) {
      return nil
    }
    _sanityCheck(other._box.endIndex is _RandomAccessIndexBoxType)
    self._box = other._box
  }
  /// If the indices of the underlying collection stored by `other`
  /// satisfy `RandomAccessIndexType`, create an
  /// `AnyRandomAccessCollection` having the same underlying
  /// collection as `other`.  Otherwise, the result is `nil`.
  ///
  /// Complexity: O(1)
  public init?(_ other: AnyBidirectionalCollection<Element>) {
    if !(other._box.startIndex is _RandomAccessIndexBoxType) {
      return nil
    }
    _sanityCheck(other._box.endIndex is _RandomAccessIndexBoxType)
    self._box = other._box
  }
  
  /// Return a *generator* over the elements of this *collection*.
  ///
  /// Complexity: O(1)
  public func generate() -> AnyGenerator<Element> {
    return unsafeDowncast(_box.generate())
  }
  
  /// The position of the first element in a non-empty collection.
  ///
  /// Identical to `endIndex` in an empty collection.
  public var startIndex: AnyRandomAccessIndex {
    return AnyRandomAccessIndex(_box.startIndex)
  }
  
  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: AnyRandomAccessIndex {
    return AnyRandomAccessIndex(_box.endIndex)
  }
  
  /// Access the element indicated by `position`.
  ///
  /// Requires: `position` indicates a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: AnyRandomAccessIndex) -> Element {
    return _box[position._box]
  }

  /// Uniquely identifies the stored underlying collection.
  public var underlyingCollectionID: ObjectIdentifier {
    return ObjectIdentifier(_box)
  }
  
  internal let _box: Box
}

