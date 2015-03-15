//===--- ExistentialCollection.swift --------------------------------------===//
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
// RUN: %target-run-simple-swift

@noreturn
internal func _abstract(file: StaticString = __FILE__, line: UWord = __LINE__) {
  fatalError("Method must be overridden", file: file, line: line)
}

public class Generator<T> : GeneratorType {
  public func next() -> T? {_abstract()}
  static public func make<
    Base: GeneratorType
  >(base: Base) -> Generator<Base.Element> {
    return _Generator(base)
  }
}

internal final class _Generator<Base: GeneratorType> : Generator<Base.Element> {
  init(_ base: Base) { self.base = base }
  override func next() -> Base.Element? { return base.next() }
  var base: Base
}

internal final class _ForwardIndexStorage<
  BaseIndex: ForwardIndexType
> : ForwardIndex._StorageBase {
  internal typealias Super = ForwardIndex._StorageBase
  
  internal init(_ base: BaseIndex) {
    self.base = base
  }
  
  internal override func successor() -> Super {
    return _ForwardIndexStorage(self.base.successor())
  }
  
  internal func unsafeUnbox(other: Super) -> BaseIndex {
    return (unsafeDowncast(other) as _ForwardIndexStorage).base
  }
  
  internal override func equals(other: Super) -> Bool {
    return base == unsafeUnbox(other)
  }

  internal override func _distanceTo(other: Super) -> ForwardIndex.Distance {
    return numericCast(distance(base, unsafeUnbox(other)))
  }
  
  internal override func _advancedBy(n: ForwardIndex.Distance) -> Super {
    return _ForwardIndexStorage(advance(base, numericCast(n)))
    
  }
  internal override func _advancedBy(
    n: ForwardIndex.Distance, _ limit: Super) -> Super {
    return _ForwardIndexStorage(
      advance(base, numericCast(n), unsafeUnbox(limit)))
  }
  
  
  internal // private
  let base: BaseIndex
}

internal class _ForwardIndexStorageBase {
  typealias _Self = _ForwardIndexStorageBase
  typealias Distance = ForwardIndex.Distance
  
  final var typeID: Int {
    return unsafeBitCast(self.dynamicType, Int.self)
  }
  
  internal func successor() -> _Self {_abstract()}
  internal func equals(other: _Self) -> Bool {_abstract()}
  internal func _distanceTo(other: _Self) -> Distance {_abstract()}
  internal func _advancedBy(distance: Distance) -> _Self {_abstract()}
  internal func _advancedBy(
    distance: Distance, _ limit: _Self) -> _Self {_abstract()}
}

public struct ForwardIndex : ForwardIndexType {
  public typealias Distance = IntMax
  typealias _StorageBase = _ForwardIndexStorageBase
  
  public init<BaseIndex: ForwardIndexType>(_ base: BaseIndex) {
    _storage = _ForwardIndexStorage(base)
  }
  
  public func successor() -> ForwardIndex {
    return ForwardIndex(_storage.successor())
  }
  
  //===--- private --------------------------------------------------------===//
  internal func _unbox<T: ForwardIndexType>() -> T {
    return (_storage as! _ForwardIndexStorage<T>).base
  }
  
  internal var _typeID: Int {
    return _storage.typeID
  }
  
  internal init(_ storage: _StorageBase) {
    self._storage = storage
  }
  
  internal let _storage: _StorageBase
}

public func ~> (
  start:ForwardIndex, other : (_Distance, ForwardIndex)
) -> ForwardIndex.Distance {
  precondition(
    start._typeID == other.1._typeID,
    "distance: base index types differ.")
  return start._storage._distanceTo(other.1._storage)
}

public func ~> (
  start:ForwardIndex, distance : (_Advance, ForwardIndex.Distance)
) -> ForwardIndex {
  return ForwardIndex(start._storage._advancedBy(distance.1))
}

public func ~> (
  start:ForwardIndex, args : (_Advance, (ForwardIndex.Distance, ForwardIndex))
) -> ForwardIndex {
  precondition(
    start._typeID == args.1.1._typeID, "advance: base index types differ.")
  return ForwardIndex(start._storage._advancedBy(args.1.0, args.1.1._storage))
}

public func == (lhs: ForwardIndex, rhs: ForwardIndex) -> Bool {
  precondition(lhs._typeID == rhs._typeID, "base index types differ.")
  return lhs._storage.equals(rhs._storage)
}

public class ForwardCollection<T> : CollectionType {
  public var startIndex: ForwardIndex {_abstract()}
  public var endIndex: ForwardIndex {_abstract()}
  public subscript(index: ForwardIndex) -> T {_abstract()}
  public func generate() -> Generator<T> {_abstract()}

  public static func make<
    Base: CollectionType where Base.Generator.Element == T
  >(
    base: Base
  ) -> ForwardCollection<T> {
    return ForwardCollectionImpl(base)
  }
  //===--- private --------------------------------------------------------===//
  //===--------------------------------------------------------------------===//
  
  //===--- SequenceType ~> operations -------------------------------------===//
  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  func _copyToNativeArrayBuffer() -> AnyObject {
    // FIXME: can't return _ContiguousArrayBuffer<T>, which would be a
    // dependent struct, pending <rdar://20164041>
    _abstract()
  }
  
  /// Copy a Sequence into an array.
  func _initializeTo(UnsafeMutablePointer<Void>) {
    // FIXME: can't pass UnsafeMutablePointer<T>, which would be a
    // dependent struct, pending <rdar://20164041>
    _abstract()
  }

  //===--- CollectionType ~> operations -----------------------------------===//
  internal func _count() -> ForwardIndex.Distance {_abstract()}
}

public func ~> <T>(
  source: ForwardCollection<T>,
  ptr: (_InitializeTo, UnsafeMutablePointer<T>)
) {
  // FIXME: can't pass UnsafeMutablePointer<T>, which would be a
  // dependent struct, pending <rdar://20164041>, thus the pointer
  // cast.
  source._initializeTo(UnsafeMutablePointer(ptr.1))
}

public func ~> <T>(
  source: ForwardCollection<T>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<T> {
  
  return unsafeBitCast(
    // FIXME: this call can't return _ContiguousArrayBuffer<T>, which
    // would be a dependent struct, pending <rdar://20164041>.  Thus
    // the cast.
    source._copyToNativeArrayBuffer(),
    _ContiguousArrayBuffer<T>.self)
}

public func ~> <T>(source: ForwardCollection<T>, _:(_Count,()))
  -> ForwardIndex.Distance
{
  return source._count()
}

// FIXME: _InitializeTo doesn't have a public init, so outside the
// standard library we need to create a layout-compatible type and
// bitcast it.
internal struct _InitializeToHack {}

final internal class ForwardCollectionImpl<Base: CollectionType>
: ForwardCollection<Base.Generator.Element> {
  internal typealias Element = Base.Generator.Element
  
  internal init(_ base: Base) { self.base = base }
  override var startIndex: ForwardIndex { return ForwardIndex(base.startIndex) }
  override var endIndex: ForwardIndex { return ForwardIndex(base.endIndex) }
  override subscript(index: ForwardIndex) -> Element {
    return base[index._unbox()]
  }
  override func generate() -> Generator<Element> {
    return Generator<Element>.make(base.generate())
  }
  //===--- private --------------------------------------------------------===//
  //===--------------------------------------------------------------------===//

  //===--- SequenceType ~> operations -------------------------------------===//
  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  override func _copyToNativeArrayBuffer() -> AnyObject {
    // FIXME: can't return _ContiguousArrayBuffer<T>, which would be a
    // dependent struct, pending <rdar://20164041>
    return ContiguousArray(base)._buffer.owner
  }
  
  /// Copy a Sequence into an array.
  override func _initializeTo(ptr: UnsafeMutablePointer<Void>) {
    // FIXME: can't pass UnsafeMutablePointer<Element>, which would be
    // a dependent struct, pending <rdar://20164041>, thus the pointer
    // cast.
    base~>(
      // FIXME: _InitializeTo doesn't have a public init, so outside
      // the standard library we need to create a layout-compatible
      // type and bitcast it.
      unsafeBitCast(_InitializeToHack(), _InitializeTo.self),
      UnsafeMutablePointer(ptr))
  }

  //===--- CollectionType ~> operations -----------------------------------===//
  override internal func _count() -> ForwardIndex.Distance {
    return numericCast(count(base))
  }

  //===--- stored properties ----------------------------------------------===//
  internal let base: Base
}

//===--- tests ------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
import StdlibUnittest

var tests = TestSuite("ExistentialCollection")

tests.test("ForwardCollection") {
  let a0: ContiguousArray = [1, 2, 3, 5, 8, 13, 21]
  let fc0 = ForwardCollection.make(a0)
  let a1 = ContiguousArray(fc0)
  expectEqual(a0, a1)
  for e in a0 {
    let i = find(fc0, e)
    expectNotEmpty(i)
    expectEqual(e, fc0[i!])
  }
  for i in indices(fc0) {
    expectNotEqual(fc0.endIndex, i)
    expectEqual(1, count(filter(indices(fc0)) {$0 == i}))
  }
}

runAllTests()
