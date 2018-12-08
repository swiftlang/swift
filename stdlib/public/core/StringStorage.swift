//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// Having @objc stuff in an extension creates an ObjC category, which we don't
// want.
#if _runtime(_ObjC)

internal protocol _AbstractStringStorage : _NSCopying {
  var asString: String { get }
  var count: Int { get }
  var isASCII: Bool { get }
  var start: UnsafePointer<UInt8> { get }
  var length: Int { get } // In UTF16 code units.
}

internal let _cocoaASCIIEncoding:UInt = 1 /* NSASCIIStringEncoding */
internal let _cocoaUTF8Encoding:UInt = 4 /* NSUTF8StringEncoding */

@_effects(readonly)
private func _isNSString(_ str:AnyObject) -> UInt8 {
  return _swift_stdlib_isNSString(str)
}

#else

internal protocol _AbstractStringStorage {
  var asString: String { get }
  var count: Int { get }
  var isASCII: Bool { get }
  var start: UnsafePointer<UInt8> { get }
}

#endif

extension _AbstractStringStorage {

// ObjC interfaces.
#if _runtime(_ObjC)
  
  @inline(__always)
  @_effects(releasenone)
  internal func _getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>, _ aRange: _SwiftNSRange
  ) {
    _precondition(aRange.location >= 0 && aRange.length >= 0,
                  "Range out of bounds")
    _precondition(aRange.location + aRange.length <= Int(count),
                  "Range out of bounds")
    
    let range = Range(
      uncheckedBounds: (aRange.location, aRange.location+aRange.length))
    let str = asString
    str._copyUTF16CodeUnits(
      into: UnsafeMutableBufferPointer(
        _uncheckedStart: buffer, count: range.count),
      range: range)
  }

  @inline(__always)
  @_effects(releasenone)
  internal func _getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, _ maxLength: Int, _ encoding: UInt
  ) -> Int8 {
    switch (encoding, isASCII) {
    case (_cocoaASCIIEncoding, true):
      fallthrough
    case (_cocoaUTF8Encoding, _):
      guard maxLength >= count + 1 else { return 0 }
      outputPtr.initialize(_uncheckedFrom: start, count: count)
      outputPtr[count] = 0
      return 1
    default:
      return  _cocoaGetCStringTrampoline(self, outputPtr, maxLength, encoding)
    }
  }

  @inline(__always)
  @_effects(readonly)
  internal func _cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    switch (encoding, isASCII) {
    case (_cocoaASCIIEncoding, true):
      fallthrough
    case (_cocoaUTF8Encoding, _):
      return start
    default:
      return _cocoaCStringUsingEncodingTrampoline(self, encoding)
    }
  }
  
  @_effects(readonly)
  internal func _nativeIsEqual<T:_AbstractStringStorage>(
    _ nativeOther: T
  ) -> Int8 {
    if count != nativeOther.count {
      return 0
    }
    return (start == nativeOther.start ||
      (memcmp(start, nativeOther.start, count) == 0)) ? 1 : 0
  }

  @inline(__always)
  @_effects(readonly)
  internal func _isEqual(_ other: AnyObject?) -> Int8 {
    guard let other = other else {
      return 0
    }

    if self === other {
      return 1
    }

    // Handle the case where both strings were bridged from Swift.
    // We can't use String.== because it doesn't match NSString semantics.
    let knownOther = _KnownCocoaString(other)
    switch knownOther {
    case .storage:
      return _nativeIsEqual(
        _unsafeUncheckedDowncast(other, to: _StringStorage.self))
    case .shared:
      return _nativeIsEqual(
        _unsafeUncheckedDowncast(other, to: _SharedStringStorage.self))
#if !(arch(i386) || arch(arm))
    case .tagged:
      fallthrough
#endif
    case .cocoa:
      // We're allowed to crash, but for compatibility reasons NSCFString allows
      // non-strings here.
      if _isNSString(other) != 1 {
        return 0
      }
      // At this point we've proven that it is an NSString of some sort, but not
      // one of ours.
      if length != _stdlib_binary_CFStringGetLength(other) {
        return 0
      }
      defer { _fixLifetime(other) }
      // CFString will only give us ASCII bytes here, but that's fine.
      // We already handled non-ASCII UTF8 strings earlier since they're Swift.
      if let otherStart = _cocoaUTF8Pointer(other) {
        return (start == otherStart ||
          (memcmp(start, otherStart, count) == 0)) ? 1 : 0
      }
      /*
       The abstract implementation of -isEqualToString: falls back to -compare:
       immediately, so when we run out of fast options to try, do the same.
       We can likely be more clever here if need be
      */
      return _cocoaStringCompare(self, other) == 0 ? 1 : 0
    }
  }
  
#endif //_runtime(_ObjC)
}

#if arch(i386) || arch(arm)
private typealias Flags = _StringObject.Flags
#endif
private typealias CountAndFlags = _StringObject.CountAndFlags

//
// TODO(String docs): Documentation about the runtime layout of these instances,
// which is a little complex. The second trailing allocation holds an
// Optional<_StringBreadcrumbs>.
//

final internal class _StringStorage
  : __SwiftNativeNSString, _AbstractStringStorage {
#if arch(i386) || arch(arm)
  // The total allocated storage capacity. Note that this includes the required
  // nul-terminator.
  internal var _realCapacity: Int
  internal var _count: Int
  internal var _flags: _StringObject.Flags
  internal var _reserved: UInt16

  internal var count: Int {
    @inline(__always) get { return _count }
    @inline(__always) set { _count = newValue }
  }
#else
  // The capacity of our allocation. Note that this includes the nul-terminator,
  // which is not available for overriding.
  internal var _realCapacityAndFlags: UInt64
  internal var _countAndFlags: _StringObject.CountAndFlags

  internal var count: Int {
    @inline(__always) get { return _countAndFlags.count }
    @inline(__always) set { _countAndFlags.count = newValue }
  }

  // The total allocated storage capacity. Note that this includes the required
  // nul-terminator.
  internal var _realCapacity: Int {
    @inline(__always) get {
      return Int(truncatingIfNeeded:
        _realCapacityAndFlags & _StringObject.Nibbles.largeAddressMask)
    }
  }
#endif

  final internal var isASCII: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      return _flags.isASCII
#else
      return _countAndFlags.isASCII
#endif
    }
  }

  final internal var asString: String {
    @_effects(readonly) @inline(__always) get {
      return String(_StringGuts(self))
    }
  }

#if _runtime(_ObjC)
  
  @objc(length)
  final internal var length: Int {
    @_effects(readonly) @inline(__always) get {
      return asString.utf16.count // UTF16View special-cases ASCII for us.
    }
  }

  @objc
  final internal var hash: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaHashASCIIBytes(start, length: count)
      }
      return _cocoaHashString(self)
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
   _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  ) {
    _getCharacters(buffer, aRange)
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return start._asCChar
    }
    return nil
  }
  
  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8>? {
    return start
  }

  @objc(cStringUsingEncoding:)
  @_effects(readonly)
  final internal func cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    return _cString(encoding: encoding)
  }
  
  @objc(getCString:maxLength:encoding:)
  @_effects(releasenone)
  final internal func getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, maxLength: Int, encoding: UInt
  ) -> Int8 {
    return _getCString(outputPtr, maxLength, encoding)
  }

  @objc
  final internal var fastestEncoding: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaASCIIEncoding
      }
      return _cocoaUTF8Encoding
    }
  }

  @objc(isEqualToString:)
  @_effects(readonly)
  final internal func isEqual(to other: AnyObject?) -> Int8 {
    return _isEqual(other)
  }
  
  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While _StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }

#endif // _runtime(_ObjC)

  private init(_doNotCallMe: ()) {
    _internalInvariantFailure("Use the create method")
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
  _internalInvariant(size % 4 == 0)
  let capacity = size - bias
  _internalInvariant(capacity > desiredCapacity)
  return capacity
#else
  // Bigger than _SmallString, and we need 1 extra for nul-terminator.
  let minCap = 1 + Swift.max(desiredCapacity, _SmallString.capacity)
  _internalInvariant(minCap < 0x1_0000_0000_0000, "max 48-bit length")

  // Round up to the nearest multiple of 8 that isn't also a multiple of 16.
  let capacity = ((minCap + 7) & -16) + 8
  _internalInvariant(
    capacity > desiredCapacity && capacity % 8 == 0 && capacity % 16 != 0)
  return capacity
#endif
}

// Creation
extension _StringStorage {
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
    _internalInvariant(capacity >= countAndFlags.count)

    let realCapacity = determineCodeUnitCapacity(capacity)
    _internalInvariant(realCapacity > capacity)
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
    _internalInvariant(capacity >= bufPtr.count)
    let storage = _StringStorage.create(
      capacity: capacity, countAndFlags: countAndFlags)
    let addr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    storage.mutableStart.initialize(_uncheckedFrom: addr, count: bufPtr.count)
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

  // Point to the nul-terminator.
  private final var terminator: UnsafeMutablePointer<UInt8> {
    @inline(__always) get { return mutableEnd }
  }

  private var codeUnits: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeBufferPointer(_uncheckedStart: start, count: count)
    }
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
  // required nul-terminator.
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
        _uncheckedStart: mutableEnd, count: unusedCapacity)
    }
  }

  // The capacity available for appending. Note that this excludes the required
  // nul-terminator.
  internal var unusedCapacity: Int {
    get { return _realCapacity &- count &- 1 }
  }

  #if !INTERNAL_CHECKS_ENABLED
  @inline(__always) internal func _invariantCheck() {}
  #else
  internal func _invariantCheck() {
    let rawSelf = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let rawStart = UnsafeRawPointer(start)
    _internalInvariant(unusedCapacity >= 0)
    _internalInvariant(count <= capacity)
    _internalInvariant(rawSelf + Int(_StringObject.nativeBias) == rawStart)
    _internalInvariant(self._realCapacity > self.count, "no room for nul-terminator")
    _internalInvariant(self.terminator.pointee == 0, "not nul terminated")

#if arch(i386) || arch(arm)
    _flags._invariantCheck()
#else
    _countAndFlags._invariantCheck()
#endif
    if isASCII {
      _internalInvariant(_allASCII(self.codeUnits))
    }
    if let crumbs = _breadcrumbsAddress.pointee {
      crumbs._invariantCheck(for: self.asString)
    }
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

// Appending
extension _StringStorage {
  // Perform common post-RRC adjustments and invariant enforcement.
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

    // TODO(String performance): Consider updating breadcrumbs when feasible.
    self._breadcrumbsAddress.pointee = nil
    _invariantCheck()
  }

  // Perform common post-append adjustments and invariant enforcement.
  @_effects(releasenone)
  private func _postAppendAdjust(
    appendedCount: Int, appendedIsASCII isASCII: Bool
  ) {
    let oldTerminator = self.terminator
    _postRRCAdjust(
      newCount: self.count + appendedCount, newIsASCII: self.isASCII && isASCII)
    _internalInvariant(oldTerminator + appendedCount == self.terminator)
  }

  @_effects(releasenone)
  internal func appendInPlace(
    _ other: UnsafeBufferPointer<UInt8>, isASCII: Bool
  ) {
    _internalInvariant(self.capacity >= other.count)
    let srcAddr = other.baseAddress._unsafelyUnwrappedUnchecked
    let srcCount = other.count
    self.mutableEnd.initialize(_uncheckedFrom: srcAddr, count: srcCount)
    _postAppendAdjust(appendedCount: srcCount, appendedIsASCII: isASCII)
  }

  @_effects(releasenone)
  internal func appendInPlace<Iter: IteratorProtocol>(
    _ other: inout Iter, isASCII: Bool
  ) where Iter.Element == UInt8 {
    var srcCount = 0
    while let cu = other.next() {
      _internalInvariant(self.unusedCapacity >= 1)
      unusedStorage[_unchecked: srcCount] = cu
      srcCount &+= 1
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
    _internalInvariant(lower <= upper)

    let lowerPtr = mutableStart + lower
    let upperPtr = mutableStart + upper
    let tailCount = mutableEnd - upperPtr
    lowerPtr.moveInitialize(_uncheckedFrom: upperPtr, count: tailCount)

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
    _internalInvariant(dst >= mutableStart && src <= mutableEnd)
    let tailCount = mutableEnd - src
    dst.moveInitialize(_uncheckedFrom: src, count: tailCount)
    return tailCount
  }

  @_effects(releasenone)
  internal func replace(
    from lower: Int, to upper: Int, with replacement: UnsafeBufferPointer<UInt8>
  ) {
    _internalInvariant(lower <= upper)
    let replCount = replacement.count
    _internalInvariant(replCount - (upper - lower) <= unusedCapacity)

    // Position the tail.
    let lowerPtr = mutableStart + lower
    let tailCount = _slideTail(
      src: mutableStart + upper, dst: lowerPtr + replCount)

    // Copy in the contents.
    lowerPtr.moveInitialize(
      _uncheckedFrom: UnsafeMutablePointer(
        mutating: replacement.baseAddress._unsafelyUnwrappedUnchecked),
      count: replCount)

    let isASCII = self.isASCII && _allASCII(replacement)
    _postRRCAdjust(newCount: lower + replCount + tailCount, newIsASCII: isASCII)
  }


  @_effects(releasenone)
  internal func replace<C: Collection>(
    from lower: Int,
    to upper: Int,
    with replacement: C,
    replacementCount replCount: Int
  ) where C.Element == UInt8 {
    _internalInvariant(lower <= upper)
    _internalInvariant(replCount - (upper - lower) <= unusedCapacity)

    // Position the tail.
    let lowerPtr = mutableStart + lower
    let tailCount = _slideTail(
      src: mutableStart + upper, dst: lowerPtr + replCount)

    // Copy in the contents.
    var isASCII = self.isASCII
    var srcCount = 0
    for cu in replacement {
      if cu >= 0x80 { isASCII = false }
      lowerPtr[srcCount] = cu
      srcCount += 1
    }
    _internalInvariant(srcCount == replCount)

    _postRRCAdjust(
      newCount: lower + replCount + tailCount, newIsASCII: isASCII)
  }
}

// For shared storage and bridging literals
final internal class _SharedStringStorage
  : __SwiftNativeNSString, _AbstractStringStorage {
  internal var _owner: AnyObject?
  internal var start: UnsafePointer<UInt8>

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

  internal var count: Int {
    @_effects(readonly) @inline(__always) get {
#if arch(i386) || arch(arm)
      return _count
#else
      return _countAndFlags.count
#endif
    }
  }

  internal init(
    immortal ptr: UnsafePointer<UInt8>,
    countAndFlags: _StringObject.CountAndFlags
  ) {
    self._owner = nil
    self.start = ptr
#if arch(i386) || arch(arm)
    self._count = countAndFlags.count
    self._flags = countAndFlags.flags
#else
    self._countAndFlags = countAndFlags
#endif
    super.init()
    self._invariantCheck()
  }
  
  final internal var isASCII: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      return _flags.isASCII
#else
      return _countAndFlags.isASCII
#endif
    }
  }

  final internal var asString: String {
    @_effects(readonly) @inline(__always) get {
      return String(_StringGuts(self))
    }
  }

#if _runtime(_ObjC)
  
  @objc(length)
  final internal var length: Int {
    @_effects(readonly) get {
      return asString.utf16.count // UTF16View special-cases ASCII for us.
    }
  }
  
  @objc
  final internal var hash: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaHashASCIIBytes(start, length: count)
      }
      return _cocoaHashString(self)
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
    _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  ) {
    _getCharacters(buffer, aRange)
  }
  
  @objc
  final internal var fastestEncoding: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaASCIIEncoding
      }
      return _cocoaUTF8Encoding
    }
  }
  
  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return start._asCChar
    }
    return nil
  }
  
  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8>? {
    return start
  }

  @objc(cStringUsingEncoding:)
  @_effects(readonly)
  final internal func cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    return _cString(encoding: encoding)
  }
  
  @objc(getCString:maxLength:encoding:)
  @_effects(releasenone)
  final internal func getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, maxLength: Int, encoding: UInt
  ) -> Int8 {
    return _getCString(outputPtr, maxLength, encoding)
  }

  @objc(isEqualToString:)
  @_effects(readonly)
  final internal func isEqual(to other:AnyObject?) -> Int8 {
    return _isEqual(other)
  }
  
  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While _StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }

#endif // _runtime(_ObjC)

}

extension _SharedStringStorage {
#if !INTERNAL_CHECKS_ENABLED
  @inline(__always)
  internal func _invariantCheck() {}
#else
  internal func _invariantCheck() {
    if let crumbs = _breadcrumbs {
      crumbs._invariantCheck(for: self.asString)
    }
    _countAndFlags._invariantCheck()
  }
#endif // INTERNAL_CHECKS_ENABLED
}
