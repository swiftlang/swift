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
  fatalError("Implement me!", file: file, line: line)
}

internal final class _ForwardIndexStorage<
  CoreIndex: ForwardIndexType
> : ForwardIndex._StorageBase {
  internal typealias Super = ForwardIndex._StorageBase
  
  internal init(_ core: CoreIndex) {
    self.core = core
  }
  
  internal override func successor() -> Super {
    return _ForwardIndexStorage(self.core.successor())
  }
  
  internal func unsafeUnbox(other: Super) -> CoreIndex {
    return (unsafeDowncast(other) as _ForwardIndexStorage).core
  }
  
  internal override func equals(other: Super) -> Bool {
    return core == unsafeUnbox(other)
  }

  internal override func _distanceTo(other: Super) -> ForwardIndex.Distance {
    return numericCast(distance(core, unsafeUnbox(other)))
  }
  
  internal override func _advancedBy(n: ForwardIndex.Distance) -> Super {
    return _ForwardIndexStorage(advance(core, numericCast(n)))
    
  }
  internal override func _advancedBy(
    n: ForwardIndex.Distance, _ limit: Super) -> Super {
    return _ForwardIndexStorage(
      advance(core, numericCast(n), unsafeUnbox(limit)))
  }
  
  
  internal // private
  let core: CoreIndex
}

public struct ForwardIndex : ForwardIndexType {
  public typealias Distance = IntMax
  
  internal class _StorageBase {
    final var typeID: Int {
      return unsafeBitCast(self.dynamicType, Int.self)
    }
    
    internal func successor() -> _StorageBase {_abstract()}
    internal func equals(other: _StorageBase) -> Bool {_abstract()}
    internal func _distanceTo(other: _StorageBase) -> Distance {_abstract()}
    internal func _advancedBy(distance: Distance) -> _StorageBase {_abstract()}
    internal func _advancedBy(
      distance: Distance, _ limit: _StorageBase) -> _StorageBase {_abstract()}
  }

  public init<CoreIndex: ForwardIndexType>(_ core: CoreIndex) {
    _storage = _ForwardIndexStorage(core)
  }
  
  public func successor() -> ForwardIndex {
    return ForwardIndex(_storage.successor())
  }
  
  //===--- private --------------------------------------------------------===//
  internal func _unbox<T: ForwardIndexType>() -> T {
    return (_storage as! _ForwardIndexStorage<T>).core
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
    "distance: core index types differ.")
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
    start._typeID == args.1.1._typeID, "advance: core index types differ.")
  return ForwardIndex(start._storage._advancedBy(args.1.0, args.1.1._storage))
}

public func == (lhs: ForwardIndex, rhs: ForwardIndex) -> Bool {
  precondition(lhs._typeID == rhs._typeID, "core index types differ.")
  return lhs._storage.equals(rhs._storage)
}

public class ForwardCollection<T> : CollectionType {
  public var startIndex: ForwardIndex {_abstract()}
  public var endIndex: ForwardIndex {_abstract()}
  public subscript(index: ForwardIndex) -> T {_abstract()}
  public func generate() -> GeneratorOf<T> {_abstract()}

  public static func make<
    Core: CollectionType where Core.Generator.Element == T
  >(
    core: Core
  ) -> ForwardCollection<T> {
    return ForwardCollectionImpl(core)
  }
  //===--- private --------------------------------------------------------===//
  //===--------------------------------------------------------------------===//
  
  //===--- SequenceType ~> operations -------------------------------------===//
  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  func _copyToNativeArrayBuffer() -> _ContiguousArrayBuffer<T> {_abstract()}

  /// Copy a Sequence into an array.
  func _initializeTo(UnsafeMutablePointer<T>) {_abstract()}

  //===--- CollectionType ~> operations -----------------------------------===//
  internal func _count() -> ForwardIndex.Distance {_abstract()}
}

public func ~> <T>(
  source: ForwardCollection<T>,
  ptr: (_InitializeTo, UnsafeMutablePointer<T>)
) {
  source._initializeTo(ptr.1)
}

public func ~> <T>(
  source: ForwardCollection<T>, _: (_CopyToNativeArrayBuffer,())
) -> _ContiguousArrayBuffer<T> {
  return source._copyToNativeArrayBuffer()
}

public func ~> <T>(source: ForwardCollection<T>, _:(_Count,()))
  -> ForwardIndex.Distance
{
  return source._count()
}

internal struct _InitializeToHack {}

final internal class ForwardCollectionImpl<Core: CollectionType>
: ForwardCollection<Core.Generator.Element>, CollectionType {
  internal typealias Element = Core.Generator.Element
  
  internal init(_ core: Core) { self.core = core }
  override var startIndex: ForwardIndex { return ForwardIndex(core.startIndex) }
  override var endIndex: ForwardIndex { return ForwardIndex(core.endIndex) }
  override subscript(index: ForwardIndex) -> Element {
    return core[index._unbox()]
  }
  override func generate() -> GeneratorOf<Element> {
    return GeneratorOf(core.generate())
  }
  //===--- private --------------------------------------------------------===//
  //===--------------------------------------------------------------------===//
  
  //===--- SequenceType ~> operations -------------------------------------===//
  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  override func _copyToNativeArrayBuffer() -> _ContiguousArrayBuffer<Element> {
    return ContiguousArray(core)._buffer
  }

  /// Copy a Sequence into an array.
  override func _initializeTo(ptr: UnsafeMutablePointer<Element>) {
    core~>(unsafeBitCast(_InitializeToHack(), _InitializeTo.self), ptr)
  }

  //===--- CollectionType ~> operations -----------------------------------===//
  override internal func _count() -> ForwardIndex.Distance {
    return numericCast(count(core))
  }

  //===--- stored properties ----------------------------------------------===//
  internal let core: Core
}

//===--- tests ------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
import StdlibUnittest

var tests = TestSuite("ExistentialCollection")

tests.test("ForwardCollection") {
  let a0 = [1, 2, 3, 5, 8, 13, 21]
  let fc0 = ForwardCollection.make(a0)
  let a1 = Array(fc0)
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
