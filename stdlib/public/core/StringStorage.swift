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
internal class _AbstractStringStorage: __SwiftNativeNSString, _NSStringCore {
  // Abstract interface
  internal var asString: String { get { Builtin.unreachable() } }
  internal var count: Int { get { Builtin.unreachable() } }
  internal func getOrComputeBreadcrumbs() -> _StringBreadcrumbs {
    Builtin.unreachable()
  }
}

// ObjC interfaces
#if _runtime(_ObjC)
extension _AbstractStringStorage {
  @objc(length)
  final internal var length: Int { return asString.utf16.count }

  @objc(characterAtIndex:)
  final func character(at offset: Int) -> UInt16 {
    let str = asString
    return str.utf16[str._toUTF16Index(offset)]
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
    let str = asString
    let slice = str.utf16[str._toUTF16Indices(range)]
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
      // FIXME(UTF8): Need to check for interior nul
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

private typealias CountAndFlags = _StringObject.CountAndFlags

//
// TODO(UTF8 merge): Documentation about the runtime layout of these instances,
// which is growing in complexity. For now, the second trailing allocation holds
// an Optional<_StringBreadcrumbs>.
//

@_fixed_layout
@usableFromInline
final internal class _StringStorage: _AbstractStringStorage {
  // The capacity of our allocation. Note that this includes the nul-terminator,
  // which is not available for overridding.
  @nonobjc
  @usableFromInline
  internal var _realCapacityAndFlags: UInt

  @nonobjc
  @usableFromInline
  internal var _countAndFlags: _StringObject.CountAndFlags

  @nonobjc
  @inlinable
  override internal var count: Int {
    @inline(__always) get { return _countAndFlags.count }
    @inline(__always) set { _countAndFlags.count = newValue }
  }

  @nonobjc
  override internal var asString: String {
    @inline(__always) get { return String(_StringGuts(self)) }
  }

  @nonobjc
  private init(_doNotCallMe: ()) {
    _sanityCheckFailure("Use the create method")
  }

  deinit {
    _breadcrumbsAddress.deinitialize(count: 1)
  }
}

// Determine the actual number of code unit capacity to request from malloc. We
// round up the nearest multiple of 8 that isn't a mulitple of 16, to fully
// utilize malloc's small buckets while accounting for the trailing
// _StringBreadCrumbs.
//
// NOTE: We may still under-utilize the spare bytes from the actual allocation
// for Strings ~1KB or larger, though at this point we're well into our growth
// curve.
private func determineCodeUnitCapacity(_ desiredCapacity: Int) -> Int {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else

  // Bigger than _SmallString, and we need 1 extra for nul-terminator
  let minCap = 1 + Swift.max(desiredCapacity, _SmallString.capacity)
  _sanityCheck(minCap < 0x1_0000_0000_0000, "max 48-bit length")

  // Round up to the nearest multiple of 8 that isn't also a multiple of 16
  let capacity = ((minCap + 7) & -16) + 8
  _sanityCheck(
    capacity > desiredCapacity && capacity % 8 == 0 && capacity % 16 != 0)
  return capacity

#endif
}

// Creation
extension _StringStorage {
  @inline(never) // rdar://problem/44542202
  @_effects(releasenone)
  @nonobjc
  private static func create(
    realCodeUnitCapacity: Int, countAndFlags: CountAndFlags
  ) -> _StringStorage {
    let storage = Builtin.allocWithTailElems_2(
      _StringStorage.self,
      realCodeUnitCapacity._builtinWordValue, UInt8.self,
      1._builtinWordValue, Optional<_StringBreadcrumbs>.self)

    storage._realCapacityAndFlags = UInt(bitPattern: realCodeUnitCapacity)
    storage._countAndFlags = countAndFlags

    storage._breadcrumbsAddress.initialize(to: nil)
    storage.terminator.pointee = 0 // nul-terminated

    // NOTE: We can't _invariantCheck() now, because code units have not been
    // initialized. But, _StringGuts's initializer will.
    return storage
  }

  @_effects(releasenone)
  @nonobjc
  private static func create(
    capacity: Int, countAndFlags: CountAndFlags
  ) -> _StringStorage {
    _sanityCheck(capacity >= countAndFlags.count)

    let realCapacity = determineCodeUnitCapacity(capacity)
    _sanityCheck(realCapacity > capacity)
    return _StringStorage.create(
      realCodeUnitCapacity: realCapacity, countAndFlags: countAndFlags)
  }

  @_effects(releasenone)
  @nonobjc
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>,
    capacity: Int,
    isASCII: Bool
  ) -> _StringStorage {
    let countAndFlags = CountAndFlags(count: bufPtr.count, isASCII: isASCII)
    _sanityCheck(capacity >= bufPtr.count)
    let storage = _StringStorage.create(
      capacity: capacity, countAndFlags: countAndFlags)
    let addr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    storage.mutableStart.initialize(from: addr, count: bufPtr.count)
    storage._invariantCheck()
    return storage
  }

  @_effects(releasenone)
  @nonobjc
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>, isASCII: Bool
  ) -> _StringStorage {
    return _StringStorage.create(
      initializingFrom: bufPtr, capacity: bufPtr.count, isASCII: isASCII)
  }
}

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
  private var mutableEnd: UnsafeMutablePointer<UInt8> {
    @inline(__always) get { return mutableStart + count }
  }

  @nonobjc
  @inlinable
  internal var start: UnsafePointer<UInt8> {
    @inline(__always) get { return UnsafePointer(mutableStart) }
  }

  @nonobjc
  private final var end: UnsafePointer<UInt8> {
    @inline(__always) get { return UnsafePointer(mutableEnd) }
  }

  // Point to the nul-terminator
  @nonobjc
  private final var terminator: UnsafeMutablePointer<UInt8> {
    @inline(__always) get { return mutableEnd }
  }

  @nonobjc
  private var codeUnits: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeBufferPointer(start: start, count: count)
    }
  }

  @nonobjc
  private var isASCII: Bool { return _countAndFlags.isASCII }

  @nonobjc
  // @opaque
  internal var _breadcrumbsAddress: UnsafeMutablePointer<_StringBreadcrumbs?> {
    let raw = Builtin.getTailAddr_Word(
      start._rawValue,
      realCapacity._builtinWordValue,
      UInt8.self,
      Optional<_StringBreadcrumbs>.self)
    return UnsafeMutablePointer(raw)
  }

  // The total capacity available for code units. Note that this excludes the
  // required nul-terminator
  @nonobjc
  internal var capacity: Int {
    return realCapacity &- 1
  }

  // The total capacity available for code units. Note that this excludes the
  // required nul-terminator
  @nonobjc
  private var realCapacity: Int {
    return Int(bitPattern:
      _realCapacityAndFlags & _StringObject.Nibbles.largeAddressMask)
  }

  // The unused capacity available for appending. Note that this excludes the
  // required nul-terminator.
  //
  // NOTE: Callers who wish to mutate this storage should enfore nul-termination
  @nonobjc
  private var unusedStorage: UnsafeMutableBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeMutableBufferPointer(
        start: mutableEnd, count: unusedCapacity)
    }
  }

  // The capacity available for appending. Note that this excludes the required
  // nul-terminator
  @nonobjc
  internal var unusedCapacity: Int {
    get { return realCapacity &- count &- 1 }
  }

  #if !INTERNAL_CHECKS_ENABLED
  @nonobjc @inline(__always) internal func _invariantCheck() {}
  #else
  @nonobjc @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    let rawSelf = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let rawStart = UnsafeRawPointer(start)
    _sanityCheck(unusedCapacity >= 0)
    _sanityCheck(count <= capacity)
    _sanityCheck(rawSelf + Int(_StringObject.nativeBias) == rawStart)
    _sanityCheck(self.realCapacity > self.count, "no room for nul-terminator")
    _sanityCheck(self.terminator.pointee == 0, "not nul terminated")

    _countAndFlags._invariantCheck()
    if isASCII {
      _sanityCheck(_allASCII(self.codeUnits))
    }
    if let crumbs = _breadcrumbsAddress.pointee {
      crumbs._invariantCheck()
    }
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

// Appending
extension _StringStorage {
  // Perform common post-RRC adjustments and invariant enforcement
  @_effects(releasenone)
  @nonobjc
  private func _postRRCAdjust(newCount: Int, newIsASCII: Bool) {
    self._countAndFlags = CountAndFlags(
      count: newCount, isASCII: newIsASCII)
    self.terminator.pointee = 0
    _invariantCheck()
  }

  // Perform common post-append adjustments and invariant enforcement
  @_effects(releasenone)
  @nonobjc
  private func _postAppendAdjust(
    appendedCount: Int, appendedIsASCII isASCII: Bool
  ) {
    let oldTerminator = self.terminator
    _postRRCAdjust(
      newCount: self.count + appendedCount, newIsASCII: self.isASCII && isASCII)
    _sanityCheck(oldTerminator + appendedCount == self.terminator)
  }

  @_effects(releasenone)
  @nonobjc
  internal func appendInPlace(
    _ other: UnsafeBufferPointer<UInt8>, isASCII: Bool
  ) {
    _sanityCheck(self.capacity >= other.count)
    let srcAddr = other.baseAddress._unsafelyUnwrappedUnchecked
    let srcCount = other.count
    self.mutableEnd.initialize(from: srcAddr, count: srcCount)
    _postAppendAdjust(appendedCount: srcCount, appendedIsASCII: isASCII)
  }

  @_effects(releasenone)
  @nonobjc
  internal func appendInPlace<Iter: IteratorProtocol>(
    _ other: inout Iter, isASCII: Bool
  ) where Iter.Element == UInt8 {
    var srcCount = 0
    while let cu = other.next() {
      _sanityCheck(self.unusedCapacity >= 1)
      unusedStorage[srcCount] = cu
      srcCount += 1
    }
    _postAppendAdjust(appendedCount: srcCount, appendedIsASCII: isASCII)
  }

  @nonobjc
  internal func clear() {
    self._countAndFlags = CountAndFlags(count: 0, isASCII: true)
    self.terminator.pointee = 0
  }
}

// Removing
extension _StringStorage {
  @_effects(releasenone)
  @nonobjc
  internal func remove(from lower: Int, to upper: Int) {
    _sanityCheck(lower <= upper)

    let lowerPtr = mutableStart + lower
    let upperPtr = mutableStart + upper
    let tailCount = mutableEnd - upperPtr
    lowerPtr.moveInitialize(from: upperPtr, count: tailCount)

    _postRRCAdjust(
      newCount: self.count &- (upper &- lower), newIsASCII: self.isASCII)
  }

  // Reposition a tail of this storage from src to dst. Returns the length of
  // the tail.
  @_effects(releasenone)
  @nonobjc
  internal func _slideTail(
    src: UnsafeMutablePointer<UInt8>,
    dst: UnsafeMutablePointer<UInt8>
  ) -> Int {
    _sanityCheck(dst >= mutableStart && src <= mutableEnd)
    let tailCount = mutableEnd - src
    dst.moveInitialize(from: src, count: tailCount)
    return tailCount
  }

  @_effects(releasenone)
  @nonobjc
  internal func replace(
    from lower: Int, to upper: Int, with replacement: UnsafeBufferPointer<UInt8>
  ) {
    _sanityCheck(lower <= upper)
    let replCount = replacement.count
    _sanityCheck((upper - lower) + replCount <= unusedCapacity)

    // Position the tail
    let lowerPtr = mutableStart + lower
    let tailCount = _slideTail(
      src: mutableStart + upper, dst: lowerPtr + replCount)

    // Copy in the contents
    lowerPtr.moveInitialize(
      from: UnsafeMutablePointer(
        mutating: replacement.baseAddress._unsafelyUnwrappedUnchecked),
      count: replCount)

    _postRRCAdjust(
      newCount: lower + replCount + tailCount,
      newIsASCII: self.isASCII && isASCII)
  }


  @_effects(releasenone)
  @nonobjc
  internal func replace<C: Collection>(
    from lower: Int,
    to upper: Int,
    with replacement: C,
    replacementCount replCount: Int
  ) where C.Element == UInt8 {
    _sanityCheck(lower <= upper)
    _sanityCheck((upper - lower) + replCount <= unusedCapacity)

    // Position the tail
    let lowerPtr = mutableStart + lower
    let tailCount = _slideTail(
      src: mutableStart + upper, dst: lowerPtr + replCount)

    // Copy in the contents
    var isASCII = true
    var srcCount = 0
    for cu in replacement {
      if cu >= 0x80 { isASCII = false }
      lowerPtr[srcCount] = cu
      srcCount += 1
    }
    _sanityCheck(srcCount == replCount)

    _postRRCAdjust(
      newCount: lower + replCount + tailCount,
      newIsASCII: self.isASCII && isASCII)
  }
}

// For shared storage and bridging literals
final internal class _SharedStringStorage: _AbstractStringStorage {
  @nonobjc
  internal var _owner: AnyObject?

  @nonobjc
  internal var _start: UnsafePointer<UInt8>

  @nonobjc
  internal var _countAndFlags: _StringObject.CountAndFlags

  @nonobjc
  internal var _breadcrumbs: _StringBreadcrumbs? = nil

  @nonobjc
  internal var start: UnsafePointer<UInt8> { return _start }

  @nonobjc
  override internal var count: Int { return _countAndFlags.count }

  @nonobjc
  internal init(
    immortal ptr: UnsafePointer<UInt8>,
    countAndFlags: _StringObject.CountAndFlags
  ) {
    self._owner = nil
    self._start = ptr
    self._countAndFlags = countAndFlags
    super.init()
    self._invariantCheck()
  }

  @nonobjc
  override internal var asString: String { return String(_StringGuts(self)) }
}

extension _SharedStringStorage {
  #if !INTERNAL_CHECKS_ENABLED
  @nonobjc @inline(__always) internal func _invariantCheck() {}
  #else
  @nonobjc @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    if let crumbs = _breadcrumbs {
      crumbs._invariantCheck()
    }
    _countAndFlags._invariantCheck()
  }
  #endif // INTERNAL_CHECKS_ENABLED
}


