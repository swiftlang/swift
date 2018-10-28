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

//
// NOTE: This is a prototype, it does not have e.g. 32-bit support yet.
//

//
// StringObject abstracts the bit-level interpretation and creation of the
// String struct.
//

@_fixed_layout
@usableFromInline
internal struct _StringObject {
  //
  // Laid out as (_otherBits, _object), which allows small string contents to
  // naturally start on vector-alignment.
  //

  @usableFromInline
  internal var _otherBits: UInt

  @usableFromInline
  internal var _object: Builtin.BridgeObject

  // @inlinable @inline(__always)
  // internal init(_ otherBits: UInt, _ object: Builtin.BridgeObject) {
  //   self._otherBits = otherBits
  //   self._object = object
  //   _invariantCheck()
  // }

  @inlinable @inline(__always)
  internal init(zero: ()) {
    self._otherBits = 0
    self._object = Builtin.reinterpretCast(0)
  }
}

// Raw
extension _StringObject {
  @usableFromInline
  internal typealias RawBitPattern = (UInt, UInt)

  @inlinable
  internal var rawBits: RawBitPattern {
    @inline(__always) get { return (_otherBits, objectRawBits) }
  }

  @inlinable @inline(__always)
  internal init(rawObject: UInt, rawDiscrim: UInt, otherBits: UInt) {
    let builtinRawObject: Builtin.Int64 = Builtin.reinterpretCast(rawObject)
    let builtinDiscrim: Builtin.Int64 = Builtin.reinterpretCast(rawDiscrim)
    self._object = Builtin.reinterpretCast(
      Builtin.stringObjectOr_Int64(builtinRawObject, builtinDiscrim))

    self._otherBits = otherBits
    _invariantCheck()
  }

  @inlinable @inline(__always)
  internal init(raw bits: RawBitPattern) {
    self.init(zero:())
    self._otherBits = bits.0
    self._object = Builtin.reinterpretCast(bits.1)
    _sanityCheck(self.rawBits == bits)
    _invariantCheck()
  }

  @inlinable @_transparent
  internal var objectRawBits: UInt {
    @inline(__always) get { return Builtin.reinterpretCast(_object) }
  }

  @inlinable @_transparent
  internal var undiscriminatedObjectRawBits: UInt {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      return (objectRawBits & 0x00FF_FFFF_FFFF_FFFF)
#endif
    }
  }

  // Namespace to hold magic numbers
  @usableFromInline @_frozen
  enum Nibbles {}
}

/*

 Encoding is optimized for common fast creation. The canonical empty string,
 ASCII small strings, as well as most literals, have all consecutive 1s in their
 high nibble mask, and thus can all be encoded as a logical immediate operand
 on arm64.

 The high nibble of the bridge object has the following encoding:

 ┌───────┬──────────┬───────┬─────┬──────┐
 │ Form  │   b63    │  b62  │ b61 │ b60  │
 ├───────┼──────────┼───────┼─────┼──────┤
 │ Small │    1     │ ASCII │  1  │  0   │
 │ Large │ Immortal │ ObjC  │  0  │ Slow │
 └───────┴──────────┴───────┴─────┴──────┘

 Immortal (b63): Should the Swift runtime skip ARC
   - Small strings are just values, always immortal
   - Large strings can sometimes be immortal, e.g. literals
 ObjC (b62): Should the runtime perform ObjC ARC rather than Swift ARC
   - Lazily-bridged NSStrings set this bit, are managed by ObjC ARC
   - Note: Small strings repurpose this bit to denote ASCII-only contents
 isSmall (b61): Dedicated bit to denote small strings
 Slow (b60): If set, string is unable to provide access to contiguous UTF-8
   - Small strings can spill to the stack
   - Native strings are UTF-8
   - Lazily bridged Cocoa strings or other foreign strings may be able to

 The canonical empty string is the zero-sized small string. It has a leading
 nibble of 1110, and all other bits are 0.

*/
extension _StringObject.Nibbles {
  // The canonical empty sting is an empty small string
  @inlinable
  internal static var emptyString: UInt {
    @inline(__always) get { return _StringObject.Nibbles.small(isASCII: true) }
  }
}

/*

 Large strings can either be "native", "shared", or "foreign".

 Native strings have tail-allocated storage, which begins at an offset of
 `nativeBias` from the storage object's address. String literals, which reside
 in the constant section, are encoded as their start address minus `nativeBias`,
 unifying code paths for both literals ("immortal native") and native strings.
 Native Strings are always managed by the Swift runtime.

 Shared strings do not have tail-allocated storage, but can provide access
 upon query to contiguous UTF-8 code units. Lazily-bridged NSStrings capable of
 providing access to contiguous ASCII/UTF-8 set the ObjC bit. Accessing shared
 string's pointer should always be behind a resilience barrier, permitting
 future evolution.

 Foreign strings, currently, only encompass lazily-bridged NSStrings that
 cannot be treated as "shared". Such strings may provide access to contiguous
 UTF-16, or may be discontiguous in storage. Accessing foreign strings should
 remain behind a resilience barrier for future evolution.

 ┌────────────┐
 │ nativeBias │
 ├────────────┤
 │     32     │
 └────────────┘


 The rest of the "object" bit representation for large strings:

 ┌──────────┬─────────┬─────┬─────┬─────┬─────┬────────────┐
 │   Form   │ b63:b60 │ b59 │ b58 │ b57 │ b56 │   b55:b0   │
 ├──────────┼─────────┼─────┼─────┼─────┼─────┼────────────┤
 │ Native   │ x 0 0 0 │  0  │ TBD │ TBD │ TBD │ objectAddr │
 │ Shared   │ x x 0 0 │  1  │ TBD │ TBD │ TBD │ objectAddr │
 │ Foreign  │ x 0 0 1 │  1  │ TBD │ TBD │ TBD │ objectAddr │
 └──────────┴─────────┴─────┴─────┴─────┴─────┴────────────┘

 b59: Set if not a typical tail-allocated native Swift string
   - Native Swift strings are tail-allocated contiguous UTF-8
   - Shared strings can provide access to contiguous UTF-8
   - Foreign string cannot

 TODO(UTF8): Allocate the rest of the bits appropriately

 TODO(UTF8): For Foreign strings, consider allocating a bit for whether they can
 provide contiguous UTF-16 code units, which would allow us to avoid doing the
 full call for non-contiguous NSString.

 objectAddr: The address of the beginning of the potentially-managed object.

 Other foreign forms are reserved for the future.

 All forms share the same structure for the rest of the bits:

 ┌────────────┬───────┐
 │   b63:48   │ b47:0 │
 ├────────────┼───────┤
 │ perf flags │ count │
 └────────────┴───────┘

 Allocation of performance flags is TBD, un-used bits will be reserved for
 future use. Count stores the number of code units: corresponds to `endIndex`.

*/
extension _StringObject.Nibbles {
  // Discriminator for small strings
  @inlinable @inline(__always)
  internal static func small(isASCII: Bool) -> UInt {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
    return isASCII ? 0xE000_0000_0000_0000 : 0xA000_0000_0000_0000
#endif
  }

  // Discriminator for large, immortal, swift-native strings
  @inlinable @inline(__always)
  internal static func largeImmortal() -> UInt {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
    return 0x8000_0000_0000_0000
#endif
  }

  // Discriminator for large, mortal (i.e. managed), swift-native strings
  @inlinable @inline(__always)
  internal static func largeMortal() -> UInt {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
    return 0x0000_0000_0000_0000
#endif
  }

  // Discriminator for large, shared, mortal (i.e. managed), swift-native
  // strings
  @inlinable @inline(__always)
  internal static func largeSharedMortal() -> UInt {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
    return 0x0800_0000_0000_0000
#endif
  }

  internal static func largeCocoa(providesFastUTF8: Bool) -> UInt {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
    return providesFastUTF8 ? 0x4800_0000_0000_0000 : 0x5800_0000_0000_0000
#endif
  }
}

extension _StringObject {
  @inlinable
  internal static var nativeBias: UInt { @inline(__always) get { return 32 } }

  @inlinable
  internal var isImmortal: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      return (objectRawBits & 0x8000_0000_0000_0000) != 0
#endif
    }
  }

  @inlinable
  internal var isMortal: Bool {
    @inline(__always) get { return !isImmortal }
  }

  @inlinable
  internal var isSmall: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      return (objectRawBits & 0x2000_0000_0000_0000) != 0
#endif
    }
  }

  @inlinable
  internal var isLarge: Bool { @inline(__always) get { return !isSmall } }

  // Whether this string can provide access to contiguous UTF-8 code units:
  //   - Small strings can by spilling to the stack
  //   - Large native strings can through an offset
  //   - Shared strings can:
  //     - Cocoa strings which respond to e.g. CFStringGetCStringPtr()
  //     - TODO(UTF8): non-Cocoa shared strings
  @inlinable
  internal var providesFastUTF8: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      return (objectRawBits & 0x1000_0000_0000_0000) == 0
#endif
    }
  }

  @inlinable
  internal var isForeign: Bool {
    @inline(__always) get { return !providesFastUTF8 }
  }

  // Whether we are a mortal, native string
  @inlinable
  internal var hasNativeStorage: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      return (objectRawBits & 0xF800_0000_0000_0000) == 0
#endif
    }
  }

  // Whether we are a mortal, shared string (managed by Swift runtime)
  internal var hasSharedStorage: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      return (objectRawBits & 0xF800_0000_0000_0000)
        == Nibbles.largeSharedMortal()
#endif
    }
  }
}

// Queries conditional on being in a large or fast form.
extension _StringObject {
  // Whether this string is native, presupposing it is both large and fast
  @inlinable
  internal var largeFastIsNative: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(isLarge && providesFastUTF8)
      return (objectRawBits & 0x0800_0000_0000_0000) == 0
#endif
    }
  }
  // Whether this string is shared, presupposing it is both large and fast
  @inlinable
  internal var largeFastIsShared: Bool {
    @inline(__always) get { return !largeFastIsNative }
  }

  // Whether this string is a lazily-bridged NSString, presupposing it is large
  @inlinable
  internal var largeIsCocoa: Bool {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(isLarge)
      return (objectRawBits & 0x4000_0000_0000_0000) != 0
#endif
    }
  }
}


/*

 Small strings have the following per-byte layout. When stored in memory
 (little-endian), their first character ('a') is in the lowest address and their
 top-nibble and count is in the highest address.

 ┌───┬───┬───┬───┬───┬───┬───┬───┬────┬────┬────┬────┬────┬────┬───┬───────────┐
 │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │ 15 │ 14 │ 13 │ 12 │ 11 │ 10 │ 9 │     8     │
 ├───┼───┼───┼───┼───┼───┼───┼───┼────┼────┼────┼────┼────┼────┼───┼───────────┤
 │ h │ g │ f │ e │ d │ c │ b │ a │ p  │ o  │ n  │ l  │ k  │ j  │ i │ 1x0x count│
 └───┴───┴───┴───┴───┴───┴───┴───┴────┴────┴────┴────┴────┴────┴───┴───────────┘

 */
extension _StringObject {
  @inlinable
  internal var smallCount: Int {
    @inline(__always)
    get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(isSmall)
      return Int(bitPattern: (objectRawBits & 0x0F00_0000_0000_0000) &>> 56)
#endif
    }
  }

  @inlinable @inline(__always)
  mutating internal func setSmallCount(_ count: Int, isASCII: Bool) {
    // TODO(UTF8 codegen): Ensure this is just a couple simple ops
    _sanityCheck(isSmall && count <= _SmallString.capacity)

    let rawObject = self.undiscriminatedObjectRawBits
    let discrim = Nibbles.small(isASCII: isASCII)
                | (UInt(bitPattern: count) &<< 56)
    self = _StringObject(
      rawObject: rawObject, rawDiscrim: discrim, otherBits: self._otherBits)
  }

  @inlinable
  internal var smallIsASCII: Bool {
    @inline(__always)
    get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(isSmall)
      return objectRawBits & 0x4000_0000_0000_0000 != 0
#endif
    }
  }
  @inlinable
  internal var asSmallString: _SmallString {
    @inline(__always)
    get {
      _sanityCheck(isSmall)
      return _SmallString(raw:rawBits)
    }
  }

  @inlinable @inline(__always)
  internal init(_ small: _SmallString) {
    self.init(raw: small.rawBits)
  }

  // Canonical empty pattern: small zero-length string
  @inlinable @inline(__always)
  internal init(empty:()) {
    self._otherBits = 0
    self._object = Builtin.reinterpretCast(Nibbles.emptyString)

    _sanityCheck(self.smallCount == 0)
    _invariantCheck()
  }
}

// Extract
extension _StringObject {
  @inlinable
  static var countMask: Int {
    @inline(__always) get { return 0x0000_FFFF_FFFF_FFFF }
  }

  @inlinable
  internal var largeCount: Int {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(isLarge)
      return Int(bitPattern: _otherBits) & _StringObject.countMask
#endif
    }
    @inline(__always) set {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(largeCount == 0)
      _sanityCheck(newValue == newValue & _StringObject.countMask, "too large")
      _otherBits |= UInt(bitPattern: newValue)
      _sanityCheck(newValue == largeCount)
      _invariantCheck()
#endif
    }
  }

  @inlinable
  internal var largeAddressBits: UInt {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(isLarge)
      return undiscriminatedObjectRawBits
#endif
    }
  }

  @inlinable
  internal var nativeUTF8Start: UnsafePointer<UInt8> {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(largeFastIsNative)
      let largeAddressBits = objectRawBits & 0x00FF_FFFF_FFFF_FFFF

      return UnsafePointer(
        bitPattern: largeAddressBits &+ _StringObject.nativeBias
      )._unsafelyUnwrappedUnchecked
#endif
    }
  }
  @inlinable
  internal var nativeUTF8: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      _sanityCheck(largeFastIsNative)
      return UnsafeBufferPointer(start: nativeUTF8Start, count: largeCount)
    }
  }

  // Resilient way to fetch a pointer
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func getSharedUTF8Start() -> UnsafePointer<UInt8> {
    _sanityCheck(largeFastIsShared)
    if largeIsCocoa {
      return _cocoaUTF8Pointer(cocoaObject)._unsafelyUnwrappedUnchecked
    }

    return sharedStorage.start
  }

  @inlinable
  internal var sharedUTF8: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      _sanityCheck(largeFastIsShared)
      let start = self.getSharedUTF8Start()
      return UnsafeBufferPointer(start: start, count: largeCount)
    }
  }

  @inlinable
  internal var nativeStorage: _StringStorage {
    @inline(__always) get {
      _sanityCheck(hasNativeStorage)
      return Builtin.reinterpretCast(largeAddressBits)
    }
  }

  internal var sharedStorage: _SharedStringStorage {
    @inline(__always) get {
      _sanityCheck(largeFastIsShared && !largeIsCocoa)
      _sanityCheck(hasSharedStorage)
      return Builtin.reinterpretCast(largeAddressBits)
    }
  }

  internal var cocoaObject: AnyObject {
    @inline(__always) get {
      _sanityCheck(largeIsCocoa && !isImmortal)
      return Builtin.reinterpretCast(largeAddressBits)
    }
  }

  @_fixed_layout @usableFromInline
  internal struct _StringObjectPerfFlags {
    @usableFromInline
    internal var bits: UInt16
  }

  @inlinable
  internal var largePerfFlags: _StringObjectPerfFlags {
    @inline(__always) get {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
      _sanityCheck(isLarge)
      return Builtin.reinterpretCast(
        UInt16(
          truncatingIfNeeded: _otherBits & 0xFFFF_0000_0000_0000) &>> 48)
#endif
    }
  }
}

// Aggregate queries / abstractions
extension _StringObject {
  // The number of code units stored
  //
  // TODO(UTF8 compiles): Check generated code
  @inlinable
  internal var count: Int {
    @inline(__always) get { return isSmall ? smallCount : largeCount }
  }

  // Whether the string is all ASCII
  //
  //
  @inlinable
  internal var isASCII: Bool {
    @inline(__always) get {
      if isSmall { return smallIsASCII }
      // TODO(UTF8 perf and testing): track known asciiness
      return false
    }
  }

  // Get access to fast UTF-8 contents for large strings which provide it.
  @inlinable
  internal var fastUTF8: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      _sanityCheck(self.isLarge && self.providesFastUTF8)
      let start: UnsafePointer<UInt8>
      let count = self.largeCount
      if _slowPath(self.largeFastIsShared) {
        start = getSharedUTF8Start()
      } else {
        start = self.nativeUTF8Start
      }
      return UnsafeBufferPointer(start: start, count: count)
    }
  }

  // Whether the object stored can be bridged directly as a NSString
  @usableFromInline // @opaque
  internal var hasObjCBridgeableObject: Bool {
    // Currently, all mortal objects can zero-cost bridge
    return !self.isImmortal
  }

  // Fetch the stored subclass of NSString for bridging
  @inlinable
  internal var objCBridgeableObject: AnyObject {
    @inline(__always) get {
      _sanityCheck(hasObjCBridgeableObject)
      return Builtin.reinterpretCast(largeAddressBits)
    }
  }

  // Whether the object provides fast UTF-8 contents that are nul-terminated
  @inlinable
  internal var isFastZeroTerminated: Bool {
    if _slowPath(!providesFastUTF8) { return false }

    // Small strings nul-terminate when spilling for contiguous access
    if isSmall { return true }

    // TODO(UTF8 perf): Use performance flag, which could be more inclusive. For
    // now, we only know native strings and small strings (when accessed) are.
    // We could also know about some shared strings.

    return largeFastIsNative
  }
}

// Object creation
extension _StringObject {
  @inlinable @inline(__always)
  init(start: UnsafePointer<UInt8>, discrim: UInt, largeCount: Int) {
    // TODO: Use with a perf flags helper
    self.init(
      rawObject: UInt(bitPattern: start) &- _StringObject.nativeBias,
      rawDiscrim: discrim,
      otherBits: UInt(bitPattern: largeCount))
  }

  @inlinable @inline(__always)
  init(immortal bufPtr: UnsafeBufferPointer<UInt8>, isASCII: Bool) {
    // TODO(UTF8) perf flags to track known ascii
    self.init(
      start: bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
      discrim: Nibbles.largeImmortal(),
      largeCount: bufPtr.count)
  }
  @inlinable @inline(__always)
  init(_ storage: _StringStorage, isASCII: Bool) {
    // TODO(UTF8) perf flags to track known ascii
    self.init(
      start: storage.start,
      discrim: Nibbles.largeMortal(),
      largeCount: storage.count)
  }

  @inlinable @inline(__always)
  init(_ storage: _SharedStringStorage, isASCII: Bool) {
    // TODO(UTF8) perf flags to track known ascii
    self.init(
      rawObject: Builtin.reinterpretCast(storage),
      rawDiscrim: Nibbles.largeSharedMortal(),
      otherBits: UInt(bitPattern: storage.count))
  }

  init(cocoa: AnyObject, providesFastUTF8: Bool, length: Int) {
    let discrim = Nibbles.largeCocoa(providesFastUTF8: providesFastUTF8)

    // TODO: Use with a perf flags helper
    self.init(
      rawObject: Builtin.reinterpretCast(cocoa),
      rawDiscrim: discrim,
      otherBits: UInt(bitPattern: length))

    _sanityCheck(self.largeAddressBits == Builtin.reinterpretCast(cocoa))
    _sanityCheck(self.providesFastUTF8 == providesFastUTF8)
    _sanityCheck(self.largeCount == length)
  }
}

// Internal invariants
extension _StringObject {
  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    _sanityCheck(MemoryLayout<_StringObject>.size == 16)
    if isForeign {
      _sanityCheck(largeIsCocoa, "No other foreign forms yet")
    }

    if isSmall {
      _sanityCheck(isImmortal)
      _sanityCheck(smallCount <= 15)
      _sanityCheck(smallCount == count)
      _sanityCheck(!hasObjCBridgeableObject)
    } else {
      _sanityCheck(isLarge)
      _sanityCheck(largeCount == count)
      if providesFastUTF8 && largeFastIsNative {
        _sanityCheck(!isSmall)
        _sanityCheck(!largeIsCocoa)

        if isImmortal {
          _sanityCheck(!hasNativeStorage)
          _sanityCheck(!hasObjCBridgeableObject)
        } else {
          _sanityCheck(hasNativeStorage)
          _sanityCheck(hasObjCBridgeableObject)
          _sanityCheck(nativeStorage.count == self.count)
          nativeStorage._invariantCheck()
        }
      }
      if largeIsCocoa {
        _sanityCheck(hasObjCBridgeableObject)
        _sanityCheck(!isSmall)
        if isForeign {

        } else {
          _sanityCheck(largeFastIsShared)
        }
      }
    }
  }
  #endif // INTERNAL_CHECKS_ENABLED

  @inline(never)
  internal func _dump() {
    // TODO(UTF8): Print out any useful internal information
    #if INTERNAL_CHECKS_ENABLED
    if isSmall {
      asSmallString._dump()
      return
    }
    if providesFastUTF8 && largeFastIsNative {
      let repr = _StringGuts(self)._classify()
      print("""
        Native(\
        owner: \(repr._objectIdentifier!), \
        count: \(repr._count), \
        capacity: \(repr._capacity))
        """)
    } else if largeIsCocoa {
      print("Cocoa")
    } else {
      print("TODO...")
    }
    #endif // INTERNAL_CHECKS_ENABLED
  }
}
