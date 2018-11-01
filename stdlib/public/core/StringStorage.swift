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

@_fixed_layout
@usableFromInline
internal class _AbstractStringStorage: __SwiftNativeNSString, _NSStringCore {
  // Abstract interface
  internal var asString: String { @_effects(readonly) get { Builtin.unreachable() } }
  internal var asGuts: _StringGuts { @_effects(readonly) get { Builtin.unreachable() } }
  internal var count: Int { @_effects(readonly) get { Builtin.unreachable() } }
  internal func getOrComputeBreadcrumbs() -> _StringBreadcrumbs {
    Builtin.unreachable()
  }
}

// ObjC interfaces
#if _runtime(_ObjC)
extension _AbstractStringStorage {

  @objc(length)
  final internal var length: Int {
    @_effects(readonly) get { 
      return asString.utf16.count
    }
  }
  
  @objc(characterAtIndex:)
  @_effects(readonly)
  final internal func character(at offset: Int) -> UInt16 {
    let str = asString
    return str.utf16[str._toUTF16Index(offset)]
  }

  @objc(getCharacters:range:)
  @_effects(releasenone)
  final internal func getCharacters(
   _ buffer: UnsafeMutablePointer<UInt16>,
   range aRange: _SwiftNSRange) {
    _precondition(aRange.location >= 0 && aRange.length >= 0,
      "Range out of bounds")
    _precondition(aRange.location + aRange.length <= Int(count),
      "Range out of bounds")

    let range = Range(
      uncheckedBounds: (aRange.location, aRange.location+aRange.length))
    let str = asString
    str._copyUTF16CodeUnits(
      into: UnsafeMutableBufferPointer(start: buffer, count: range.count),
      range: range)
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(_ requiresNulTermination:Int8) -> UnsafePointer<CChar>? {
    if let native = self as? _StringStorage, native.isASCII {
      return native.start._asCChar
    }

    // TODO(String performance): Check for nul-terminated shared strings, which
    // could be from bridged literals to/from ObjC (alternatively: reconsider
    // our bridging model for literals).

    return nil
  }
  
  @objc(UTF8String)
  final internal var utf8String: UnsafePointer<UInt8>? {
    @_effects(readonly) get { return asGuts.withFastUTF8 { $0 }.baseAddress }
  }
  
  @objc
  internal var fastestEncoding: Int {
    @_effects(readonly) get {
      if let native = self as? _StringStorage, native.isASCII {
        return 1 /* NSASCIIStringEncoding */
      }
      return 4 /* NSUTF8StringEncoding */
    }
  }

  @inline(never) //hide the shim call so we can use @_effects
  @_effects(readonly)
  private final func _isNSString(_ str:AnyObject) -> UInt8 {
    return _swift_stdlib_isNSString(str)
  }
  
  @objc(isEqualToString:)
  @_effects(readonly)
  internal final func isEqual(to other:AnyObject?) -> Int8 {
    guard let other = other else {
      return 0
    }

    if self === other {
      return 1
    }
   
    let ourGuts = self.asGuts
    defer { _fixLifetime(ourGuts) }

    //Handle the case where both strings were bridged from Swift.
    //We can't use String.== because it doesn't match NSString semantics.
    if let otherGuts = (other as? _AbstractStringStorage)?.asGuts {
      if otherGuts.count != ourGuts.count {
        return 0
      }
      return ourGuts.withFastUTF8 { ourBytes in
        defer {
          _fixLifetime(otherGuts)
        }
        return otherGuts.withFastUTF8 { otherBytes in
          return (ourBytes.baseAddress == otherBytes.baseAddress ||
            (memcmp(ourBytes.baseAddress!, otherBytes.baseAddress!, ourBytes.count) == 0)) ? 1 : 0
        }
      }
    }
    
    //we're allowed to crash, but for compatibility reasons NSCFString allows non-strings here
    if _isNSString(other) != 1 {
      return 0
    }
    
    //At this point we've proven that it is an NSString of some sort, but not one of ours
    if length != _stdlib_binary_CFStringGetLength(other) {
      return 0
    }
    
    defer {
      _fixLifetime(other)
    }

    //CFString will only give us ASCII bytes here, but that's fine
    //We already handled non-ASCII UTF8 strings earlier since they're Swift
    if let otherBytes = _cocoaUTF8Pointer(other) {
      return ourGuts.withFastUTF8 { ourBytes in
        return (ourBytes.baseAddress == otherBytes ||
          (memcmp(ourBytes.baseAddress!, otherBytes, ourBytes.count) == 0)) ? 1 : 0
      }
    }
    
    return _cocoaStringCompare(self, other) == 0 ? 1 : 0
  }

  @objc internal var hash: UInt {
    if let native = self as? _StringStorage, native.isASCII {
      return _cocoaHashASCIIBytes(native.start, length: native.count)
    } else {
      return _cocoaHashString(self)
    }
  }

  @objc(copyWithZone:)
  @usableFromInline
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While _StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }
}
#endif // _runtime(_ObjC)

#if arch(i386) || arch(arm)
private typealias Flags = _StringObject.Flags
#endif
private typealias CountAndFlags = _StringObject.CountAndFlags

//
// TODO(String docs): Documentation about the runtime layout of these instances,
// which is a little complex. The second trailing allocation holds an
// Optional<_StringBreadcrumbs>.
//

@_fixed_layout
@usableFromInline
final internal class _StringStorage: _AbstractStringStorage {
#if arch(i386) || arch(arm)
  // The total allocated storage capacity. Note that this includes the required
  // nul-terminator
  @usableFromInline
  internal var _realCapacity: Int

  @usableFromInline
  internal var _count: Int

  @usableFromInline
  internal var _flags: _StringObject.Flags

  internal var _reserved: UInt16

  @inlinable
  override internal var count: Int {
    @inline(__always) get { return _count }
    @inline(__always) set { _count = newValue }
  }
#else
  // The capacity of our allocation. Note that this includes the nul-terminator,
  // which is not available for overridding.
  @usableFromInline
  internal var _realCapacityAndFlags: UInt64

  @usableFromInline
  internal var _countAndFlags: _StringObject.CountAndFlags

  @inlinable
  override internal var count: Int {
    @inline(__always) get { return _countAndFlags.count }
    @inline(__always) set { _countAndFlags.count = newValue }
  }

  // The total allocated storage capacity. Note that this includes the required
  // nul-terminator
  internal var _realCapacity: Int {
    @inline(__always) get {
      return Int(truncatingIfNeeded:
        _realCapacityAndFlags & _StringObject.Nibbles.largeAddressMask)
    }
  }
#endif

  override internal var asString: String {
    @inline(__always) @_effects(readonly)  get { return String(_StringGuts(self)) }
  }
  
  override internal var asGuts: _StringGuts {
    @inline(__always) @_effects(readonly) get { return _StringGuts(self) }
  }

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
  // FIXME: Adapt to actual 32-bit allocator. For now, let's arrange things so
  // that the instance size will be a multiple of 4.
  let bias = Int(bitPattern: _StringObject.nativeBias)
  let minimum = bias + desiredCapacity + 1
  let size = (minimum + 3) & ~3
  _sanityCheck(size % 4 == 0)
  let capacity = size - bias
  _sanityCheck(capacity > desiredCapacity)
  return capacity
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
  private static func create(
    realCodeUnitCapacity: Int, countAndFlags: CountAndFlags
  ) -> _StringStorage {
    let storage = Builtin.allocWithTailElems_2(
      _StringStorage.self,
      realCodeUnitCapacity._builtinWordValue, UInt8.self,
      1._builtinWordValue, Optional<_StringBreadcrumbs>.self)
#if arch(i386) || arch(arm)
    storage._realCapacity = realCodeUnitCapacity
    storage._count = countAndFlags.count
    storage._flags = countAndFlags.flags
#else
    storage._realCapacityAndFlags =
      UInt64(truncatingIfNeeded: realCodeUnitCapacity)
    storage._countAndFlags = countAndFlags
#endif

    storage._breadcrumbsAddress.initialize(to: nil)
    storage.terminator.pointee = 0 // nul-terminated

    // NOTE: We can't _invariantCheck() now, because code units have not been
    // initialized. But, _StringGuts's initializer will.
    return storage
  }

  @_effects(releasenone)
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
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>,
    capacity: Int,
    isASCII: Bool
  ) -> _StringStorage {
#if arch(i386) || arch(arm)
    let flags = Flags(isASCII: isASCII)
    let countAndFlags = CountAndFlags(count: bufPtr.count, flags: flags)
#else
    let countAndFlags = CountAndFlags(count: bufPtr.count, isASCII: isASCII)
#endif
    _sanityCheck(capacity >= bufPtr.count)
    let storage = _StringStorage.create(
      capacity: capacity, countAndFlags: countAndFlags)
    let addr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    storage.mutableStart.initialize(from: addr, count: bufPtr.count)
    storage._invariantCheck()
    return storage
  }

  @_effects(releasenone)
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>, isASCII: Bool
  ) -> _StringStorage {
    return _StringStorage.create(
      initializingFrom: bufPtr, capacity: bufPtr.count, isASCII: isASCII)
  }
}

// Usage
extension _StringStorage {
  @inlinable
  internal var mutableStart: UnsafeMutablePointer<UInt8> {
    @inline(__always) get {
      return UnsafeMutablePointer(Builtin.projectTailElems(self, UInt8.self))
    }
  }
  private var mutableEnd: UnsafeMutablePointer<UInt8> {
    @inline(__always) get { return mutableStart + count }
  }

  @inlinable
  internal var start: UnsafePointer<UInt8> {
    @inline(__always) get { return UnsafePointer(mutableStart) }
  }

  private final var end: UnsafePointer<UInt8> {
    @inline(__always) get { return UnsafePointer(mutableEnd) }
  }

  // Point to the nul-terminator
  private final var terminator: UnsafeMutablePointer<UInt8> {
    @inline(__always) get { return mutableEnd }
  }

  private var codeUnits: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeBufferPointer(start: start, count: count)
    }
  }

  fileprivate var isASCII: Bool {
#if arch(i386) || arch(arm)
    return _flags.isASCII
#else
    return _countAndFlags.isASCII
#endif
  }

  // @opaque
  internal var _breadcrumbsAddress: UnsafeMutablePointer<_StringBreadcrumbs?> {
    let raw = Builtin.getTailAddr_Word(
      start._rawValue,
      _realCapacity._builtinWordValue,
      UInt8.self,
      Optional<_StringBreadcrumbs>.self)
    return UnsafeMutablePointer(raw)
  }

  // The total capacity available for code units. Note that this excludes the
  // required nul-terminator
  internal var capacity: Int {
    return _realCapacity &- 1
  }

  // The unused capacity available for appending. Note that this excludes the
  // required nul-terminator.
  //
  // NOTE: Callers who wish to mutate this storage should enfore nul-termination
  private var unusedStorage: UnsafeMutableBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeMutableBufferPointer(
        start: mutableEnd, count: unusedCapacity)
    }
  }

  // The capacity available for appending. Note that this excludes the required
  // nul-terminator
  internal var unusedCapacity: Int {
    get { return _realCapacity &- count &- 1 }
  }

  #if !INTERNAL_CHECKS_ENABLED
  @inline(__always) internal func _invariantCheck() {}
  #else
  internal func _invariantCheck() {
    let rawSelf = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let rawStart = UnsafeRawPointer(start)
    _sanityCheck(unusedCapacity >= 0)
    _sanityCheck(count <= capacity)
    _sanityCheck(rawSelf + Int(_StringObject.nativeBias) == rawStart)
    _sanityCheck(self._realCapacity > self.count, "no room for nul-terminator")
    _sanityCheck(self.terminator.pointee == 0, "not nul terminated")

#if arch(i386) || arch(arm)
    _flags._invariantCheck()
#else
    _countAndFlags._invariantCheck()
#endif
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
  private func _postRRCAdjust(newCount: Int, newIsASCII: Bool) {
#if arch(i386) || arch(arm)
    self._count = newCount
    self._flags = Flags(isASCII: newIsASCII)
#else
    self._countAndFlags = CountAndFlags(
      count: newCount, isASCII: newIsASCII)
#endif
    self.terminator.pointee = 0
    _invariantCheck()
  }

  // Perform common post-append adjustments and invariant enforcement
  @_effects(releasenone)
  private func _postAppendAdjust(
    appendedCount: Int, appendedIsASCII isASCII: Bool
  ) {
    let oldTerminator = self.terminator
    _postRRCAdjust(
      newCount: self.count + appendedCount, newIsASCII: self.isASCII && isASCII)
    _sanityCheck(oldTerminator + appendedCount == self.terminator)
  }

  @_effects(releasenone)
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

  internal func clear() {
    _postRRCAdjust(newCount: 0, newIsASCII: true)
  }
}

// Removing
extension _StringStorage {
  @_effects(releasenone)
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
  internal var _owner: AnyObject?

  internal var _start: UnsafePointer<UInt8>

#if arch(i386) || arch(arm)
  internal var _count: Int

  internal var _flags: _StringObject.Flags

  @inlinable
  internal var _countAndFlags: _StringObject.CountAndFlags {
    @inline(__always) get {
      return CountAndFlags(count: _count, flags: _flags)
    }
  }
#else
  internal var _countAndFlags: _StringObject.CountAndFlags
#endif

  internal var _breadcrumbs: _StringBreadcrumbs? = nil

  internal var start: UnsafePointer<UInt8> { return _start }

  override internal var count: Int {
#if arch(i386) || arch(arm)
    return _count
#else
    return _countAndFlags.count
#endif
  }

  internal init(
    immortal ptr: UnsafePointer<UInt8>,
    countAndFlags: _StringObject.CountAndFlags
  ) {
    self._owner = nil
    self._start = ptr
#if arch(i386) || arch(arm)
    self._count = countAndFlags.count
    self._flags = countAndFlags.flags
#else
    self._countAndFlags = countAndFlags
#endif
    super.init()
    self._invariantCheck()
  }

  override internal var asString: String { @_effects(readonly) get { return String(_StringGuts(self)) } }
  
  override internal var asGuts: _StringGuts { @_effects(readonly) get { return _StringGuts(self) } }

}

extension _SharedStringStorage {
  #if !INTERNAL_CHECKS_ENABLED
  @inline(__always) internal func _invariantCheck() {}
  #else
  internal func _invariantCheck() {
    if let crumbs = _breadcrumbs {
      crumbs._invariantCheck()
    }
    _countAndFlags._invariantCheck()
  }
  #endif // INTERNAL_CHECKS_ENABLED
}


