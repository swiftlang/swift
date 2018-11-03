//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// TODO(UTF8): We can drop the nonobjc annotations soon

@_fixed_layout
@usableFromInline
@objc
internal class _AbstractStringStorage: _SwiftNativeNSString, _NSStringCore {
  // Abstract interface
  internal var asString: String { get { Builtin.unreachable() } }
  internal var count: Int { get { Builtin.unreachable() } }
}

// ObjC interfaces
#if _runtime(_ObjC)
extension _AbstractStringStorage {
  @objc(length)
  final internal var length: Int { return asString._utf16Length() }

  @objc(characterAtIndex:)
  final func character(at index: Int) -> UInt16 {
    return asString._utf16CodeUnitAtOffset(index)
  }

  @objc(getCharacters:range:)
  final func getCharacters(
   _ buffer: UnsafeMutablePointer<UInt16>,
   range aRange: _SwiftNSRange) {
    _precondition(aRange.location >= 0 && aRange.length >= 0,
      "Range out of bounds")
    _precondition(aRange.location + aRange.length <= Int(count),
      "Range out of bounds")

    let range = Range(
      uncheckedBounds: (aRange.location, aRange.location+aRange.length))
    let slice = asString.utf16[asString._utf16OffsetToIndex(range)]
    let outputBufPtr = UnsafeMutableBufferPointer(
      start: buffer, count: range.count)

    let _ = slice._copyContents(initializing: outputBufPtr)
  }

  @objc(_fastCharacterContents)
  final func _fastCharacterContents() -> UnsafePointer<UInt16>? {
    return nil
  }

  @objc(_fastCStringContents)
  final func _fastCStringContents() -> UnsafePointer<CChar>? {
    if let native = self as? _StringStorage {
      return native.start._asCChar
    }

    // TODO(UTF8 perf): shared from literals are nul-terminated...

    return nil
  }

  @objc(copyWithZone:)
  @usableFromInline
  final func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While _StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }
}
#endif // _runtime(_ObjC)

@_fixed_layout
@usableFromInline
final internal class _StringStorage: _AbstractStringStorage {
  @nonobjc
  @usableFromInline
  internal var capacity: Int

  @nonobjc
  @usableFromInline
  internal var _count: Int

  @nonobjc
  @inlinable
  override internal var count: Int { @inline(__always) get { return _count } }

  @nonobjc
  @inlinable
  override internal var asString: String {
    @inline(__always) get { return String(_StringGuts(self)) }
  }

  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Use the create method")
  }
}

// Creation
extension _StringStorage {
  @nonobjc
  internal static func create(
    capacity: Int, count: Int = 0
  ) -> _StringStorage {
    _sanityCheck(capacity >= count)

    // Reserve enough capacity for a trailing nul character
    let capacity = 1 + Swift.max(capacity, _SmallUTF8String.capacity)
    _sanityCheck(capacity > count)

    let storage = Builtin.allocWithTailElems_1(
      _StringStorage.self,
      capacity._builtinWordValue, UInt8.self)

    let storageAddr = UnsafeRawPointer(
      Builtin.bridgeToRawPointer(storage))
    let endAddr = (
      storageAddr + _stdlib_malloc_size(storageAddr)
    ).assumingMemoryBound(to: UInt8.self)

    storage.capacity = endAddr - storage.start
    storage._count = count
    _sanityCheck(storage.capacity >= capacity)
    storage.unusedStorage[0] = 0 // nul-terminated
    storage._invariantCheck()

    return storage
  }

  @nonobjc
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>, capacity: Int
  ) -> _StringStorage {
    _sanityCheck(capacity >= bufPtr.count)
    let storage = _StringStorage.create(
      capacity: capacity, count: bufPtr.count)
    let addr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    storage.mutableStart.initialize(from: addr, count: bufPtr.count)
    return storage
  }

  @nonobjc
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>
  ) -> _StringStorage {
    return _StringStorage.create(
      initializingFrom: bufPtr, capacity: bufPtr.count)
  }

  @nonobjc
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>,
    andAppending secondBufPtr: UnsafeBufferPointer<UInt8>
  ) -> _StringStorage {
    let size = bufPtr.count + secondBufPtr.count
    let storage = _StringStorage.create(
      capacity: size, count: size)

    let addr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    storage.mutableStart.initialize(from: addr, count: bufPtr.count)

    let secondAddr = secondBufPtr.baseAddress._unsafelyUnwrappedUnchecked
    (storage.mutableStart + bufPtr.count).initialize(
      from: secondAddr, count: secondBufPtr.count)
    return storage
  }
}

// TODO(UTF8 perf): Append helpers, which can keep nul-termination

// Usage
extension _StringStorage {
  @nonobjc
  @inlinable
  internal var mutableStart: UnsafeMutablePointer<UInt8> {
    @inline(__always) get {
      return UnsafeMutablePointer(Builtin.projectTailElems(self, UInt8.self))
    }
  }
  @nonobjc
  @inlinable
  internal var mutableEnd: UnsafeMutablePointer<UInt8> {
    @inline(__always) get { return mutableStart + count }
  }

  @nonobjc
  @inlinable
  internal var start: UnsafePointer<UInt8> {
    @inline(__always) get { return UnsafePointer(mutableStart) }
  }

  @nonobjc
  @inlinable
  internal final var end: UnsafePointer<UInt8> {
    @inline(__always) get { return UnsafePointer(mutableEnd) }
  }

  @nonobjc
  @inlinable
  internal var codeUnits: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeBufferPointer(start: start, count: count)
    }
  }

  @inlinable
  @nonobjc
  internal var unusedStorage: UnsafeMutableBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeMutableBufferPointer(
        start: mutableEnd, count: unusedCapacity)
    }
  }

  @nonobjc
  @inlinable
  internal var unusedCapacity: Int {
    @inline(__always) get { return capacity &- count }
  }

  @nonobjc
  @inlinable @inline(__always)
  internal func _invariantCheck() {
    #if INTERNAL_CHECKS_ENABLED
    let rawSelf = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let rawStart = UnsafeRawPointer(start)
    _sanityCheck(rawSelf + Int(_StringObject.nativeBias) == rawStart)
    _sanityCheck(self.capacity > self.count, "no room for nul-terminator")
    _sanityCheck(self.unusedStorage[0] == 0, "not nul terminated")
    #endif
  }
}

// For bridging literals
//
// TODO(UTF8): Unify impls with _StringStorage
//
@_fixed_layout
@usableFromInline
final internal class _SharedStringStorage: _AbstractStringStorage {
  @nonobjc
  @usableFromInline
  internal var owner: AnyObject?

  @nonobjc
  @usableFromInline
  internal var contents: UnsafeBufferPointer<UInt8>

  @nonobjc
  @usableFromInline
  internal var start: UnsafePointer<UInt8> {
    return contents.baseAddress._unsafelyUnwrappedUnchecked
  }

  @nonobjc
  @usableFromInline
  override internal var count: Int { return contents.count }

  @nonobjc
  internal init(owner: AnyObject, contents bufPtr: UnsafeBufferPointer<UInt8>) {
    self.owner = owner
    self.contents = bufPtr
    super.init()
    self._invariantCheck()
  }

  @nonobjc
  internal init(immortal bufPtr: UnsafeBufferPointer<UInt8>) {
    self.owner = nil
    self.contents = bufPtr
    super.init()
    self._invariantCheck()
  }

  @nonobjc
  override internal var asString: String { return String(_StringGuts(self)) }
}

extension _SharedStringStorage {
  @nonobjc
  @inlinable @inline(__always)
  internal func _invariantCheck() {
    #if INTERNAL_CHECKS_ENABLED
    #endif
  }
}


