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

// Having @objc stuff in an extension creates an ObjC category, which we don't
// want.
#if _runtime(_ObjC)

internal protocol _AbstractStringStorage: _NSCopying {
  var asString: String { get }
  var count: Int { get }
  var isASCII: Bool { get }
  var start: UnsafePointer<UInt8> { get }
  var UTF16Length: Int { get }
}

#else

internal protocol _AbstractStringStorage {
  var asString: String { get }
  var count: Int { get }
  var isASCII: Bool { get }
  var start: UnsafePointer<UInt8> { get }
}

#endif

private typealias CountAndFlags = _StringObject.CountAndFlags

//
// TODO(String docs): Documentation about the runtime layout of these instances,
// which is a little complex. The second trailing allocation holds an
// Optional<_StringBreadcrumbs>.
//

// NOTE: older runtimes called this class _StringStorage. The two
// must coexist without conflicting ObjC class names, so it was
// renamed. The old name must not be used in the new runtime.
final internal class __StringStorage
  : __SwiftNativeNSString, _AbstractStringStorage {
#if arch(i386) || arch(arm)
  // The total allocated storage capacity. Note that this includes the required
  // nul-terminator.
  internal var _realCapacity: Int
  internal var _count: Int
  internal var _flags: UInt16
  internal var _reserved: UInt16

  @inline(__always)
  internal var count: Int { return _count }

  @inline(__always)
  internal var _countAndFlags: _StringObject.CountAndFlags {
    return CountAndFlags(count: _count, flags: _flags)
  }
#else
  // The capacity of our allocation. Note that this includes the nul-terminator,
  // which is not available for overriding.
  internal var _realCapacityAndFlags: UInt64
  internal var _countAndFlags: _StringObject.CountAndFlags

  @inline(__always)
  internal var count: Int { return _countAndFlags.count }

  // The total allocated storage capacity. Note that this includes the required
  // nul-terminator.
  @inline(__always)
  internal var _realCapacity: Int {
    return Int(truncatingIfNeeded:
      _realCapacityAndFlags & CountAndFlags.countMask)
  }
#endif

  @inline(__always)
  final internal var isASCII: Bool { return _countAndFlags.isASCII }

  final internal var asString: String {
    @_effects(readonly) @inline(__always)
    get { return String(_StringGuts(self)) }
  }


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
extension __StringStorage {
  @_effects(releasenone)
  private static func create(
    realCodeUnitCapacity: Int, countAndFlags: CountAndFlags
  ) -> __StringStorage {
    let storage = Builtin.allocWithTailElems_2(
      __StringStorage.self,
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
  ) -> __StringStorage {
    _internalInvariant(capacity >= countAndFlags.count)

    let realCapacity = determineCodeUnitCapacity(capacity)
    _internalInvariant(realCapacity > capacity)
    return __StringStorage.create(
      realCodeUnitCapacity: realCapacity, countAndFlags: countAndFlags)
  }

  // The caller is expected to check UTF8 validity and ASCII-ness and update
  // the resulting StringStorage accordingly
  internal static func create(
    uninitializedCapacity capacity: Int,
    initializingUncheckedUTF8With initializer: (
      _ buffer: UnsafeMutableBufferPointer<UInt8>
    ) throws -> Int
  ) rethrows -> __StringStorage {
    let storage = __StringStorage.create(
      capacity: capacity,
      countAndFlags: CountAndFlags(mortalCount: 0, isASCII: false)
    )
    let buffer = UnsafeMutableBufferPointer(start: storage.mutableStart,
                                            count: capacity)
    let count = try initializer(buffer)

    let countAndFlags = CountAndFlags(mortalCount: count, isASCII: false)
    #if arch(i386) || arch(arm)
    storage._count = countAndFlags.count
    storage._flags = countAndFlags.flags
    #else
    storage._countAndFlags = countAndFlags
    #endif

    storage.terminator.pointee = 0 // nul-terminated
    return storage
  }

  @_effects(releasenone)
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>,
    capacity: Int,
    isASCII: Bool
  ) -> __StringStorage {
    let countAndFlags = CountAndFlags(
      mortalCount: bufPtr.count, isASCII: isASCII)
    _internalInvariant(capacity >= bufPtr.count)
    let storage = __StringStorage.create(
      capacity: capacity, countAndFlags: countAndFlags)
    let addr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    storage.mutableStart.initialize(from: addr, count: bufPtr.count)
    storage._invariantCheck()
    return storage
  }

  @_effects(releasenone)
  internal static func create(
    initializingFrom bufPtr: UnsafeBufferPointer<UInt8>, isASCII: Bool
  ) -> __StringStorage {
    return __StringStorage.create(
      initializingFrom: bufPtr, capacity: bufPtr.count, isASCII: isASCII)
  }
}

// Usage
extension __StringStorage {
  @inline(__always)
  private var mutableStart: UnsafeMutablePointer<UInt8> {
    return UnsafeMutablePointer(Builtin.projectTailElems(self, UInt8.self))
  }
  @inline(__always)
  private var mutableEnd: UnsafeMutablePointer<UInt8> {
     return mutableStart + count
  }

  @inline(__always)
  internal var start: UnsafePointer<UInt8> {
     return UnsafePointer(mutableStart)
  }

  @inline(__always)
  private final var end: UnsafePointer<UInt8> {
    return UnsafePointer(mutableEnd)
  }

  // Point to the nul-terminator.
  @inline(__always)
  private final var terminator: UnsafeMutablePointer<UInt8> {
    return mutableEnd
  }

  @inline(__always)
  internal var codeUnits: UnsafeBufferPointer<UInt8> {
    return UnsafeBufferPointer(start: start, count: count)
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
  @inline(__always)
  private var unusedStorage: UnsafeMutableBufferPointer<UInt8> {
    return UnsafeMutableBufferPointer(
      start: mutableEnd, count: unusedCapacity)
  }

  // The capacity available for appending. Note that this excludes the required
  // nul-terminator.
  internal var unusedCapacity: Int { return _realCapacity &- count &- 1 }

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
    let str = asString
    _internalInvariant(str._guts._object.isPreferredRepresentation)

    _countAndFlags._invariantCheck()
    if isASCII {
      _internalInvariant(_allASCII(self.codeUnits))
    }
    if let crumbs = _breadcrumbsAddress.pointee {
      crumbs._invariantCheck(for: self.asString)
    }
    _internalInvariant(_countAndFlags.isNativelyStored)
    _internalInvariant(_countAndFlags.isTailAllocated)
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

// Appending
extension __StringStorage {
  // Perform common post-RRC adjustments and invariant enforcement.
  @_effects(releasenone)
  internal func _updateCountAndFlags(newCount: Int, newIsASCII: Bool) {
    let countAndFlags = CountAndFlags(
      mortalCount: newCount, isASCII: newIsASCII)
#if arch(i386) || arch(arm)
    self._count = countAndFlags.count
    self._flags = countAndFlags.flags
#else
    self._countAndFlags = countAndFlags
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
    _updateCountAndFlags(
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
    self.mutableEnd.initialize(from: srcAddr, count: srcCount)
    _postAppendAdjust(appendedCount: srcCount, appendedIsASCII: isASCII)
  }

  @_effects(releasenone)
  internal func appendInPlace<Iter: IteratorProtocol>(
    _ other: inout Iter, isASCII: Bool
  ) where Iter.Element == UInt8 {
    var srcCount = 0
    while let cu = other.next() {
      _internalInvariant(self.unusedCapacity >= 1)
      unusedStorage[srcCount] = cu
      srcCount += 1
    }
    _postAppendAdjust(appendedCount: srcCount, appendedIsASCII: isASCII)
  }

  internal func clear() {
    _updateCountAndFlags(newCount: 0, newIsASCII: true)
  }
}

// Removing
extension __StringStorage {
  @_effects(releasenone)
  internal func remove(from lower: Int, to upper: Int) {
    _internalInvariant(lower <= upper)

    let lowerPtr = mutableStart + lower
    let upperPtr = mutableStart + upper
    let tailCount = mutableEnd - upperPtr
    lowerPtr.moveInitialize(from: upperPtr, count: tailCount)

    _updateCountAndFlags(
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
    dst.moveInitialize(from: src, count: tailCount)
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
      from: UnsafeMutablePointer(
        mutating: replacement.baseAddress._unsafelyUnwrappedUnchecked),
      count: replCount)

    let isASCII = self.isASCII && _allASCII(replacement)
    _updateCountAndFlags(newCount: lower + replCount + tailCount, newIsASCII: isASCII)
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

    _updateCountAndFlags(
      newCount: lower + replCount + tailCount, newIsASCII: isASCII)
  }
}

// For shared storage and bridging literals
// NOTE: older runtimes called this class _SharedStringStorage. The two
// must coexist without conflicting ObjC class names, so it was
// renamed. The old name must not be used in the new runtime.
final internal class __SharedStringStorage
  : __SwiftNativeNSString, _AbstractStringStorage {
  internal var _owner: AnyObject?
  internal var start: UnsafePointer<UInt8>

#if arch(i386) || arch(arm)
  internal var _count: Int
  internal var _flags: UInt16

  @inline(__always)
  internal var _countAndFlags: _StringObject.CountAndFlags {
    return CountAndFlags(count: _count, flags: _flags)
  }
#else
  internal var _countAndFlags: _StringObject.CountAndFlags
#endif

  internal var _breadcrumbs: _StringBreadcrumbs? = nil

  internal var count: Int { return _countAndFlags.count }

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

  @inline(__always)
  final internal var isASCII: Bool { return _countAndFlags.isASCII }

  final internal var asString: String {
    @_effects(readonly) @inline(__always) get {
      return String(_StringGuts(self))
    }
  }
}

extension __SharedStringStorage {
#if !INTERNAL_CHECKS_ENABLED
  @inline(__always)
  internal func _invariantCheck() {}
#else
  internal func _invariantCheck() {
    if let crumbs = _breadcrumbs {
      crumbs._invariantCheck(for: self.asString)
    }
    _countAndFlags._invariantCheck()
    _internalInvariant(!_countAndFlags.isNativelyStored)
    _internalInvariant(!_countAndFlags.isTailAllocated)
    let str = asString
    _internalInvariant(!str._guts._object.isPreferredRepresentation)
  }
#endif // INTERNAL_CHECKS_ENABLED
}
