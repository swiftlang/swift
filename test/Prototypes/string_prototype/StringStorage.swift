// Prototype code to be added to an eventual StringStorage.swift

import Swift
import SwiftShims

protocol CopyConstructible {  }
extension CopyConstructible {
  init(_ me: Self) {
    self = me
  }
}

/// - Requires: Element is trivial (UInt8/UInt16)
@_versioned
final class _StringStorage<Element: UnsignedInteger>
 : /*_SwiftNativeNSString,*/ _NSStringCore, CopyConstructible {

  // NOTE: If you change these stored properties, edit the
  // definitions in GlobalObjects.h and GlobalObject.cpp
  var count: Int
  var capacity: Int // Should be a let.

  internal init(DoNotCallMe: ()) { count = 0; capacity = 0 }

  convenience init(count: Int, minimumCapacity: Int) {
    self.init(
      Builtin.allocWithTailElems_1(
      _StringStorage.self,
        Swift.max(count, minimumCapacity)._builtinWordValue, Element.self))

    let storageAddr = UnsafeMutableRawPointer(
      Builtin.projectTailElems(self, Element.self))
    let endAddr = storageAddr + _swift_stdlib_malloc_size(storageAddr)
    let realCapacity = endAddr.assumingMemoryBound(to: Element.self)
      - storageAddr.assumingMemoryBound(to: Element.self)

    // All stored properties are uninitialized, but we can use assignment
    // because they're trivial types.
    self.count = count
    self.capacity = realCapacity
  }

  /// The empty singleton that is used for every single empty String.
  /// The contents of the storage should never be mutated.
  internal static var empty: _StringStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyStringStorage))
  }

/*
}
// TODO: JIRA for error: @objc is not supported within extensions of generic classes
extension _StringStorage : _NSStringCore {
*/

  @objc
  func length() -> Int {
    return count
  }

  @objc
  func characterAtIndex(_ index: Int) -> UInt16 {
    return numericCast(self[index])
  }

  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    guard Element.self is UInt16.Type else { return nil }

    // Need to do manual projection, because compiler can't prove
    // firstElementAddress has the right type.
    return UnsafeMutablePointer(Builtin.projectTailElems(self, Element.self))
  }

  @objc(copyWithZone:)
  internal func copy(with _: _SwiftNSZone?) -> AnyObject {
    return self
  }
}

extension _StringStorage : RandomAccessCollection, MutableCollection {
  var startIndex : Int { return 0 }
  var endIndex : Int { return count }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return _StringBuffer(self)[i]
    }
    set {
      _StringBuffer(self)[i] = newValue
    }
  }
}

/// - Requires: Element is trivial (UInt8/UInt16)
struct _StringBuffer<Element: UnsignedInteger> {
  internal var _storage: _StringStorage<Element>

  init(_ storage: _StringStorage<Element>) { self._storage = storage }

  init() {
    self.init(_StringStorage.empty)
  }

  init(_buffer source: Buffer, shiftedToStartIndex: Int) {
    _sanityCheck(shiftedToStartIndex == 0, "shiftedToStartIndex must be 0")
    self.init(source._storage)
  }

  init(_uninitializedCount: Int, minimumCapacity: Int) {
    self.init(_StringStorage(count: _uninitializedCount, 
      minimumCapacity: minimumCapacity))
  }

}

extension _StringBuffer : _ArrayBufferProtocol, _ContiguousBufferProtocol {
  var count: Int { 
    get { return _storage.count }
    nonmutating set { _storage.count = newValue }
  }

  var capacity: Int { return _storage.capacity }
  var owner: AnyObject { return _storage }
  
  var firstElementAddress: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(Builtin.projectTailElems(_storage, Element.self))
  }
  
  var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return firstElementAddress
  }
  
  var identity: UnsafeRawPointer { return UnsafeRawPointer(firstElementAddress) }

  internal mutating func isUniquelyReferenced() -> Bool {
    return _isUnique(&_storage)
  }

  internal mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
  -> _StringBuffer? {
    if _fastPath(isUniquelyReferenced()) {
      if _fastPath(capacity >= minimumCapacity) {
        return self
      }
    }
    return nil
  }

  mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  func requestNativeBuffer() -> _StringBuffer? {
    return self
  }

  @_versioned
  internal func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeMutableBufferPointer(
        start: firstElementAddress,
        count: count
      )
    )
  }

  @_versioned
  internal func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeBufferPointer(
        start: firstElementAddress,
        count: count
      )
    )
  }

  @discardableResult
  internal func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _sanityCheck(bounds.lowerBound >= 0)
    _sanityCheck(bounds.upperBound >= bounds.lowerBound)
    _sanityCheck(bounds.upperBound <= count)

    defer { _fixLifetime(self) }

    let initializedCount = bounds.upperBound - bounds.lowerBound
    target.initialize(
      from: firstElementAddress + bounds.lowerBound, count: initializedCount)
    
    return target + initializedCount
  }

}

extension _StringBuffer : RandomAccessCollection, MutableCollection, 
    RangeReplaceableCollection {
  var startIndex : Int { return _storage.startIndex }
  var endIndex : Int { return _storage.endIndex }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    nonmutating set {
      withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}
