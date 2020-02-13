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

/// Instrumentation helpers
import SwiftShims
private func report<S: CustomStringConvertible>(
  _ s: S,
  file: String = #file,
  line: UInt = #line
) {
  var err = _Stderr()
  err.write("<report ")
  err.write(file)
  err.write(":")
  err.write(String(line))
  err.write("> ")
  err.write(s.description)
  err.write("\n")
}
extension String {
  // DO NOT PUSH: Just for gathering stats
  public struct _AllocationStats {
    // Size of the entire allocation, as determined by malloc_size
    public var totalAllocationSize: Int

    // Each of the components of that allocation
    public var headerSize: Int
    public var usedCodeUnitSize: Int
    public var terminatorSize: Int { 1 }
    public var spareCodeUnitCapacity: Int
    public var breadcrumbPointerSize: Int

    public var unclaimedCapacity: Int {
      totalAllocationSize - headerSize - usedCodeUnitSize - terminatorSize
        - spareCodeUnitCapacity - breadcrumbPointerSize
    }

    internal init(_ storage: __StringStorage) {
      let storageAddr = UnsafeRawPointer(
        Builtin.bridgeToRawPointer(storage))

      let codeUnitAddr = UnsafeRawPointer(storage.mutableStart)
      let terminatorAddr = UnsafeRawPointer(storage.terminator)
      let capacityEndAddr = UnsafeRawPointer(storage._realCapacityEnd)

      self.totalAllocationSize = _swift_stdlib_malloc_size(storageAddr)
      self.headerSize = codeUnitAddr - storageAddr
      self.usedCodeUnitSize = terminatorAddr - codeUnitAddr
      self.spareCodeUnitCapacity =
        capacityEndAddr - terminatorAddr - 1 /* self.terminatorSize */
      self.breadcrumbPointerSize =
        storage.hasBreadcrumbs ? MemoryLayout<Int>.size : 0

      // Some sanity checks
      assert(headerSize == _StringObject.nativeBias)
      assert(usedCodeUnitSize == storage.count)
      assert(spareCodeUnitCapacity == storage.unusedCapacity)
      assert(unclaimedCapacity >= 0)
    }
  }

  public static func _allocationStats(
    forCount count: Int, requestingCapacity: Int? = nil
  ) -> _AllocationStats {
    let capacity = requestingCapacity ?? count

    return _AllocationStats(__StringStorage.create(
      count: count, capacity: capacity))
  }

  public var _allocationStats: _AllocationStats? {
    guard _guts._object.hasNativeStorage else { return nil }
    return _AllocationStats(_guts._object.nativeStorage)
  }
}


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

/*

 String's storage class has a header, which includes the isa pointer, reference
 count, and stored properties (count and capacity). After the header is a tail
 allocation for the UTF-8 code units, a null terminator, and some spare capacity
 (if available). After that, it optionally contains another tail allocation for
 the breadcrumbs pointer.

 If the requested code unit capacity is less than the breadcrumbs stride, no
 pointer is allocated. This has the effect of either allowing us to save space
 with a smaller allocation, or claim additional excess capacity, depending on
 which half of the malloc bucket the requested capacity lies within.

 On 64-bit platforms:

 0                                                                            32
 ├─────────────────────────────────────────────────────────────────────────────┤
 │ Class Header                                                                │
 ├─────────────┬─────────────────┬────────────────────┬────────────────────────┤
 │ B0 ..< B8   │ B8 ..< B16      │ B16 ..< B24        │ B24 ..< B32            │
 ├─────────────┼─────────────────┼────────────────────┼────────────────────────┤
 │ isa pointer │ reference count │ capacity and flags │ count and flags        │
 └─────────────┴─────────────────┴────────────────────┴────────────────────────┘

 1) If breadcrumbs are not present, the tail allocation is the requested
 capacity plus one (for the null terminator) rounded up to the nearest multiple
 of 16 (estimated malloc bucket size).

 32                                                                         32+n
 ├─────────────────────────────────────────────────────────────────────────────┤
 │ Tail allocation, no breadcrumbs pointer                                     │
 ├────────────────────┬─────────┬──────────────────────────────────────────────┤
 │ B32 ..< B<32+r>    │ B<32+r> │ B<32+r+1> ..< B<32+n>                        │
 ├────────────────────┼─────────┼──────────────────────────────────────────────┤
 │ requested capacity │ null    │ spare capacity                               │
 └────────────────────┴─────────┴──────────────────────────────────────────────┘

  where *r* is the requested capacity, and *n* is (r+16)&~15, or the nearest
  multiple of 16 greater than (but not equal to) the requested code unit
  capacity.

 2) If breadcrumbs are present, the tail allocation is the requested capacity
 plus one (for the null terminator) plus eight (for the pointer) rounded up to
 the nearest multiple of 16.

 32                                                                       32+m+8
 ├─────────────────────────────────────────────────────────────────────────────┤
 │ Tail allocations, with breadcrumbs pointer                                  │
 ├────────────────────┬─────────┬───────────────────────┬──────────────────────┤
 │ B32 ..< B<32+r>    │ B<32+r> │ B<32+r+1> ..< B<32+m> │ B<32+m> ..< B<32+m+8>│
 ├────────────────────┼─────────┼───────────────────────┼──────────────────────┤
 │ requested capacity │ null    │ spare capacity        │ breadcrumbs pointer  │
 └────────────────────┴─────────┴───────────────────────┴──────────────────────┘

  where *r* is the requested capacity, and *m* is ((r+8)&-16)+8, or the nearest
  multiple of 8, which is not also a multiple of 16, greater than (but not equal
  to) the requested code unit capacity.


 On 32-bit platforms:

  TODO


  TODO: size savings of PR for 64-bit systems, and 32-bit systems.

*/


// NOTE: older runtimes called this class _StringStorage. The two
// must coexist without conflicting ObjC class names, so it was
// renamed. The old name must not be used in the new runtime.
final internal class __StringStorage
  : __SwiftNativeNSString, _AbstractStringStorage {
#if arch(i386) || arch(arm) || arch(wasm32)
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
    if hasBreadcrumbs {
      _breadcrumbsAddress.deinitialize(count: 1)
    }
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
//
// TODO: Further comment, or refactor logic, incorporating breadcrumbs decision
private func determineCodeUnitCapacity(
  _ desiredCapacity: Int
) -> (realCodeUnitCapacity: Int, includeBreadcrumbs: Bool) {
#if arch(i386) || arch(arm) || arch(wasm32)
  // FIXME: Adapt to actual 32-bit allocator. For now, let's arrange things so
  // that the instance size will be a multiple of 4.
  let bias = Int(bitPattern: _StringObject.nativeBias)
  let size = (desiredCapacity + 4) & ~3
  _internalInvariant(size % 4 == 0)
  let capacity = size - bias
  _internalInvariant(capacity > desiredCapacity)
  return capacity
#else
  _internalInvariant((0..<0x1_0000_0000_0000).contains(desiredCapacity),
    "max 48-bit length")

  // If the resultant code unit capacity is less than the breadcrumbs stride,
  // don't allocate a breadcrumb pointer. We determine this by checking if we
  // are within a bucket (estimated to be 16 bytes) of that stride by
  // overestimating the result.
  let (capacity, includeBreadcrumbs): (Int, Bool)

  // Round up to the nearest multiple of 16 (bucket estimate) greater than,
  // but not equal to (for null terminator) the requested capacity
  let crumblessCapacity = (desiredCapacity + 16) & ~15

  // We over-allocate by 1 for the nul-terminator, which should not participate
  // in the breadcrumb stride
  if crumblessCapacity - 1 < _StringBreadcrumbs.breadcrumbStride {
    (capacity, includeBreadcrumbs) = (crumblessCapacity, false)
  } else {
    // Round up to the nearest multiple of 8, which isn't also a multiple of 16,
    // greater than, but not equal to (for null terminator) the requested
    // capacity
    capacity = ((desiredCapacity + 8) & ~15) + 8
    _internalInvariant(
      capacity > desiredCapacity && capacity % 8 == 0 && capacity % 16 != 0)
    includeBreadcrumbs = true
  }

  _internalInvariant(capacity <= desiredCapacity + 16,
    "We exceeded the nearest bucket")
  return (capacity, includeBreadcrumbs)
#endif
}

// Creation
extension __StringStorage {
  @_effects(releasenone)
  private static func create(
    realCodeUnitCapacity: Int,
    countAndFlags: CountAndFlags,
    includeBreadcrumbs: Bool
  ) -> __StringStorage {

    let storage: __StringStorage
    if includeBreadcrumbs {
      storage = Builtin.allocWithTailElems_2(
        __StringStorage.self,
        realCodeUnitCapacity._builtinWordValue, UInt8.self,
        1._builtinWordValue, Optional<_StringBreadcrumbs>.self)
    } else {
      storage = Builtin.allocWithTailElems_1(
        __StringStorage.self,
        realCodeUnitCapacity._builtinWordValue, UInt8.self)
    }

#if arch(i386) || arch(arm) || arch(wasm32)
    storage._realCapacity = realCodeUnitCapacity
    storage._count = countAndFlags.count
    storage._flags = countAndFlags.flags
#else
    storage._realCapacityAndFlags =
      UInt64(truncatingIfNeeded: realCodeUnitCapacity)
    storage._countAndFlags = countAndFlags
#endif

    // FIXME TODO: Add a bit on the storage class that tracks breadcrumb-ness
    // and guard all access on that bit, likely through a hard precondition
    if storage.hasBreadcrumbs {
      storage._breadcrumbsAddress.initialize(to: nil)
    }

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

    let (realCapacity, includeBreadcrumbs) = determineCodeUnitCapacity(capacity)
    _internalInvariant(realCapacity > capacity)

    let storage = __StringStorage.create(
      realCodeUnitCapacity: realCapacity,
      countAndFlags: countAndFlags,
      includeBreadcrumbs: includeBreadcrumbs)

    return storage
  }

  // DO NOT PUSH: For stats gathering only...
  internal static func create(count: Int, capacity: Int) -> __StringStorage {
    __StringStorage.create(
      capacity: capacity,
      countAndFlags: CountAndFlags(count: count, flags: 0))
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
  internal var hasBreadcrumbs: Bool {
    // FIXME: Record and rely on a bit on capacityAndFlags instead
    (_realCapacity - 1) >= _StringBreadcrumbs.breadcrumbStride
  }

  @inline(__always)
  internal var mutableStart: UnsafeMutablePointer<UInt8> {
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
  internal final var terminator: UnsafeMutablePointer<UInt8> {
    return mutableEnd
  }

  @inline(__always)
  internal var codeUnits: UnsafeBufferPointer<UInt8> {
    return UnsafeBufferPointer(start: start, count: count)
  }

  // The address after the last bytes of capacity
  //
  // If breadcrumbs are present, this will point to them, otherwise it will
  // point to the end of the allocation (as far as Swift is concerned).
  internal var _realCapacityEnd: Builtin.RawPointer {
    Builtin.getTailAddr_Word(
      start._rawValue,
      _realCapacity._builtinWordValue,
      UInt8.self,
      Optional<_StringBreadcrumbs>.self)
  }

  // @opaque
  internal var _breadcrumbsAddress: UnsafeMutablePointer<_StringBreadcrumbs?> {
    // TODO: better message
    precondition(
      hasBreadcrumbs, "Internal error: string breadcrumbs not present")
    return UnsafeMutablePointer(_realCapacityEnd)
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
    if hasBreadcrumbs, let crumbs = _breadcrumbsAddress.pointee {
      crumbs._invariantCheck(for: self.asString)
    }
    _internalInvariant(_countAndFlags.isNativelyStored)
    _internalInvariant(_countAndFlags.isTailAllocated)

    // Capacity end
    _internalInvariant(UnsafeMutablePointer<UInt8>(_realCapacityEnd)
      == unusedStorage.baseAddress! + (unusedStorage.count + 1))
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
#if arch(i386) || arch(arm) || arch(wasm32)
    self._count = countAndFlags.count
    self._flags = countAndFlags.flags
#else
    self._countAndFlags = countAndFlags
#endif
    self.terminator.pointee = 0

    // TODO(String performance): Consider updating breadcrumbs when feasible.
    if hasBreadcrumbs {
      self._breadcrumbsAddress.pointee = nil
    }
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

#if arch(i386) || arch(arm) || arch(wasm32)
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
#if arch(i386) || arch(arm) || arch(wasm32)
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
