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
// StringObject abstracts the bit-level interpretation and creation of the
// String struct.
//

// TODO(String docs): Word-level diagram

/*

  On 64-bit platforms, the discriminator is the most significant 8 bits of the
  bridge object.

  ┌─────────────────────╥─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
  │ Form                ║  7  │  6  │  5  │  4  │  3  │  2  │  1  │  0  │
  ╞═════════════════════╬═════╪═════╪═════╪═════╪═════╧═════╧═════╧═════╡
  │ Immortal, Small     ║  1  │ASCII│  1  │  0  │      small count      │
  ├─────────────────────╫─────┼─────┼─────┼─────┼─────┬─────┬─────┬─────┤
  │ Immortal, Large     ║  1  │  0  │  0  │  0  │  0  │ TBD │ TBD │ TBD │
  ╞═════════════════════╬═════╪═════╪═════╪═════╪═════╪═════╪═════╪═════╡
  │ Native              ║  0  │  0  │  0  │  0  │  0  │ TBD │ TBD │ TBD │
  ├─────────────────────╫─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
  │ Shared              ║  x  │  0  │  0  │  0  │  1  │ TBD │ TBD │ TBD │
  ├─────────────────────╫─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
  │ Shared, Bridged     ║  0  │  1  │  0  │  0  │  1  │ TBD │ TBD │ TBD │
  ╞═════════════════════╬═════╪═════╪═════╪═════╪═════╪═════╪═════╪═════╡
  │ Foreign             ║  x  │  0  │  0  │  1  │  1  │ TBD │ TBD │ TBD │
  ├─────────────────────╫─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
  │ Foreign, Bridged    ║  0  │  1  │  0  │  1  │  1  │ TBD │ TBD │ TBD │
  └─────────────────────╨─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘

  b7: isImmortal: Should the Swift runtime skip ARC
    - Small strings are just values, always immortal
    - Large strings can sometimes be immortal, e.g. literals
  b6: (large) isBridged / (small) isASCII
    - For large strings, this means lazily-bridged NSString: perform ObjC ARC
    - Small strings repurpose this as a dedicated bit to remember ASCII-ness
  b5: isSmall: Dedicated bit to denote small strings
  b4: isForeign: aka isSlow, cannot provide access to contiguous UTF-8
  b3: (large) not isTailAllocated: payload isn't a biased pointer
    - Shared strings provide contiguous UTF-8 through extra level of indirection

  The canonical empty string is the zero-sized small string. It has a leading
  nibble of 1110, and all other bits are 0.

  A "dedicated" bit is used for the most frequent fast-path queries so that they
  can compile to a fused check-and-branch, even if that burns part of the
  encoding space.

  On 32-bit platforms, we use an explicit discriminator with the same encoding
  as above, except bit 7 is omitted from storage -- it is left free, to supply
  extra inhabitants in the StringObject structure. The missing bit can be
  recovered by looking at `_variant.isImmortal`.
*/

@_fixed_layout @usableFromInline
internal struct _StringObject {
  // Namespace to hold magic numbers
  @usableFromInline @_frozen
  enum Nibbles {}

  // Abstract the count and performance-flags containing word
  @_fixed_layout @usableFromInline
  struct CountAndFlags {
    @usableFromInline
    var _storage: UInt64

    @inlinable @inline(__always)
    internal init(zero: ()) { self._storage = 0 }
  }

#if arch(i386) || arch(arm)
  @usableFromInline @_frozen
  internal enum Variant {
    case immortal(UInt)
    case native(AnyObject)
    case bridged(_CocoaString)

    @inlinable @inline(__always)
    internal static func immortal(start: UnsafePointer<UInt8>) -> Variant {
      let biased = UInt(bitPattern: start) &- _StringObject.nativeBias
      return .immortal(biased)
    }

    @inlinable
    internal var isImmortal: Bool {
      @inline(__always) get {
        if case .immortal = self { return true }
        return false
      }
    }
  }

  @usableFromInline
  internal var _count: Int

  @usableFromInline
  internal var _variant: Variant

  @usableFromInline
  internal var _discriminator: UInt8

  @usableFromInline
  internal var _flags: UInt16

  @inlinable @inline(__always)
  init(count: Int, variant: Variant, discriminator: UInt64, flags: UInt16) {
    _internalInvariant(
      discriminator == discriminator & _StringObject.Nibbles.discriminatorMask)

    self._count = count
    self._variant = variant
    self._discriminator = UInt8(truncatingIfNeeded: discriminator &>> 56)
    self._flags = flags
    self._invariantCheck()
  }

  @inlinable @inline(__always)
  init(variant: Variant, discriminator: UInt64, countAndFlags: CountAndFlags) {
    self.init(
      count: countAndFlags.count,
      variant: variant,
      discriminator: discriminator,
      flags: countAndFlags.flags)
  }

  @inlinable @inline(__always)
  internal var _countAndFlags: CountAndFlags {
    return CountAndFlags(count: _count, flags: _flags)
  }
#else

  //
  // Laid out as (_countAndFlags, _object), which allows small string contents
  // to naturally start on vector-alignment.
  //
  @usableFromInline
  internal var _countAndFlags: CountAndFlags

  @usableFromInline
  internal var _object: Builtin.BridgeObject

  @inlinable @inline(__always)
  internal init(zero: ()) {
    self._countAndFlags = CountAndFlags(zero:())
    self._object = Builtin.valueToBridgeObject(UInt64(0)._value)
  }

#endif
}

// Raw
extension _StringObject {
  @usableFromInline
  internal typealias RawBitPattern = (UInt64, UInt64)

#if arch(i386) || arch(arm)
  // On 32-bit platforms, raw bit conversion is one-way only and uses the same
  // layout as on 64-bit platforms.
  @usableFromInline
  internal var rawBits: RawBitPattern {
    @inline(__always) get {
      let count = UInt64(truncatingIfNeeded: UInt(bitPattern: _count))
      let payload = UInt64(truncatingIfNeeded: discriminatedObjectRawBits)
                  & _StringObject.Nibbles.largeAddressMask
      let flags = UInt64(truncatingIfNeeded: _flags)
      let discr = UInt64(truncatingIfNeeded: _discriminator)
      if isSmall {
        // Rearrange small strings in a different way, compacting bytes into a
        // contiguous sequence. See comment on small string layout below.
        return (count | (payload &<< 32), flags | (discr &<< 56))
      }
      return (count | (flags &<< 48), payload | (discr &<< 56))
    }
  }
#else
  @inlinable
  internal var rawBits: RawBitPattern {
    @inline(__always) get {
      return (_countAndFlags.rawBits, discriminatedObjectRawBits)
    }
  }

  @inlinable @inline(__always)
  internal init(
    bridgeObject: Builtin.BridgeObject, countAndFlags: CountAndFlags
  ) {
    self._object = bridgeObject
    self._countAndFlags = countAndFlags
    _invariantCheck()
  }

  @inlinable @inline(__always)
  internal init(
    object: AnyObject, discriminator: UInt64, countAndFlags: CountAndFlags
  ) {
    let builtinRawObject: Builtin.Int64 = Builtin.reinterpretCast(object)
    let builtinDiscrim: Builtin.Int64 = discriminator._value
    self.init(
      bridgeObject: Builtin.reinterpretCast(
        Builtin.stringObjectOr_Int64(builtinRawObject, builtinDiscrim)),
      countAndFlags: countAndFlags)
  }

  // Initializer to use for tagged (unmanaged) values
  @inlinable @inline(__always)
  internal init(
    pointerBits: UInt64, discriminator: UInt64, countAndFlags: CountAndFlags
  ) {
    let builtinValueBits: Builtin.Int64 = pointerBits._value
    let builtinDiscrim: Builtin.Int64 = discriminator._value
    self.init(
      bridgeObject: Builtin.valueToBridgeObject(Builtin.stringObjectOr_Int64(
        builtinValueBits, builtinDiscrim)),
      countAndFlags: countAndFlags)
  }

  @inlinable @inline(__always)
  internal init(rawUncheckedValue bits: RawBitPattern) {
    self.init(zero:())
    self._countAndFlags = CountAndFlags(rawUnchecked: bits.0)
    self._object = Builtin.valueToBridgeObject(bits.1._value)
    _internalInvariant(self.rawBits == bits)
  }

  @inlinable @inline(__always)
  internal init(rawValue bits: RawBitPattern) {
    self.init(rawUncheckedValue: bits)
    _invariantCheck()
  }
#endif

  @inlinable @_transparent
  internal var discriminatedObjectRawBits: UInt64 {
#if arch(i386) || arch(arm)
    let low32: UInt
    switch _variant {
    case .immortal(let bitPattern):
      low32 = bitPattern
    case .native(let storage):
      low32 = Builtin.reinterpretCast(storage)
    case .bridged(let object):
      low32 = Builtin.reinterpretCast(object)
    }

    return UInt64(truncatingIfNeeded: _discriminator) &<< 56
         | UInt64(truncatingIfNeeded: low32)
#else
    return Builtin.reinterpretCast(_object)
#endif
  }
}

extension _StringObject.CountAndFlags {
  @usableFromInline
  internal typealias RawBitPattern = UInt64

  @inlinable @inline(__always)
  internal var rawBits: RawBitPattern {
   return _storage
  }

  @inlinable @inline(__always)
  internal init(rawUnchecked bits: RawBitPattern) {
    self._storage = bits
  }

  @inlinable @inline(__always)
  internal init(raw bits: RawBitPattern) {
    self.init(rawUnchecked: bits)
    _invariantCheck()
  }
}

/*
 Encoding is optimized for common fast creation. The canonical empty string,
 ASCII small strings, as well as most literals, have all consecutive 1s in their
 high nibble mask, and thus can all be encoded as a logical immediate operand
 on arm64.
*/
extension _StringObject.Nibbles {
  // The canonical empty sting is an empty small string
  @inlinable
  internal static var emptyString: UInt64 {
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

 Foreign strings cannot provide access to contiguous UTF-8. Currently, this only
 encompasses lazily-bridged NSStrings that cannot be treated as "shared". Such
 strings may provide access to contiguous UTF-16, or may be discontiguous in
 storage. Accessing foreign strings should remain behind a resilience barrier
 for future evolution. Other foreign forms are reserved for the future.

 Shared and foreign strings are always created and accessed behind a resilience
 barrier, providing flexibility for the future.

 ┌────────────┐
 │ nativeBias │
 ├────────────┤
 │     32     │
 └────────────┘

 ┌───────────────┬────────────┐
 │    b63:b56    │   b55:b0   │
 ├───────────────┼────────────┤
 │ discriminator │ objectAddr │
 └───────────────┴────────────┘

 discriminator: See comment for _StringObject.Discriminator
 objectAddr: The address of the beginning of the potentially-managed object.

 TODO(Future): For Foreign strings, consider allocating a bit for whether they
 can provide contiguous UTF-16 code units, which would allow us to avoid doing
 the full call for non-contiguous NSString.

*/
extension _StringObject.Nibbles {
  // Mask for address bits, i.e. non-discriminator and non-extra high bits
  @inlinable @inline(__always)
  static internal var largeAddressMask: UInt64 { return 0x00FF_FFFF_FFFF_FFFF }

  // Mask for address bits, i.e. non-discriminator and non-extra high bits
  @inlinable @inline(__always)
  static internal var discriminatorMask: UInt64 { return ~largeAddressMask }
}

extension _StringObject.Nibbles {
  // Discriminator for small strings
  @inlinable @inline(__always)
  internal static func small(isASCII: Bool) -> UInt64 {
    return isASCII ? 0xE000_0000_0000_0000 : 0xA000_0000_0000_0000
  }

  // Discriminator for small strings
  @inlinable @inline(__always)
  internal static func small(withCount count: Int, isASCII: Bool) -> UInt64 {
    _internalInvariant(count <= _SmallString.capacity)
    return small(isASCII: isASCII) | UInt64(truncatingIfNeeded: count) &<< 56
  }

  // Discriminator for large, immortal, swift-native strings
  @inlinable @inline(__always)
  internal static func largeImmortal() -> UInt64 {
    return 0x8000_0000_0000_0000
  }

  // Discriminator for large, mortal (i.e. managed), swift-native strings
  @inlinable @inline(__always)
  internal static func largeMortal() -> UInt64 {
    return 0x0000_0000_0000_0000
  }

  // Discriminator for large, shared, mortal (i.e. managed), swift-native
  // strings
  @inlinable @inline(__always)
  internal static func largeSharedMortal() -> UInt64 {
    return 0x0800_0000_0000_0000
  }

  internal static func largeCocoa(providesFastUTF8: Bool) -> UInt64 {
    return providesFastUTF8 ? 0x4800_0000_0000_0000 : 0x5800_0000_0000_0000
  }
}

extension _StringObject {
  @inlinable
  internal static var nativeBias: UInt {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      return 20
#else
      return 32
#endif
    }
  }

  @inlinable
  internal var isImmortal: Bool {
    @inline(__always) get {
      return (discriminatedObjectRawBits & 0x8000_0000_0000_0000) != 0
    }
  }

  @inlinable
  internal var isMortal: Bool {
    @inline(__always) get { return !isImmortal }
  }

  @inlinable
  internal var isSmall: Bool {
    @inline(__always) get {
      return (discriminatedObjectRawBits & 0x2000_0000_0000_0000) != 0
    }
  }

  @inlinable
  internal var isLarge: Bool { @inline(__always) get { return !isSmall } }

  // Whether this string can provide access to contiguous UTF-8 code units:
  //   - Small strings can by spilling to the stack
  //   - Large native strings can through an offset
  //   - Shared strings can:
  //     - Cocoa strings which respond to e.g. CFStringGetCStringPtr()
  //     - Non-Cocoa shared strings
  @inlinable
  internal var providesFastUTF8: Bool {
    @inline(__always) get {
      return (discriminatedObjectRawBits & 0x1000_0000_0000_0000) == 0
    }
  }

  @inlinable
  internal var isForeign: Bool {
    @inline(__always) get { return !providesFastUTF8 }
  }

  // Whether we are a mortal, native (tail-allocated) string
  @inlinable
  internal var hasNativeStorage: Bool {
    @inline(__always) get {
      return (discriminatedObjectRawBits & 0xF800_0000_0000_0000) == 0
    }
  }

  // Whether we are a mortal, shared string (managed by Swift runtime)
  internal var hasSharedStorage: Bool {
    @inline(__always) get {
      return (discriminatedObjectRawBits & 0xF800_0000_0000_0000)
        == Nibbles.largeSharedMortal()
    }
  }
}

// Queries conditional on being in a large or fast form.
extension _StringObject {
  // Whether this string is native, presupposing it is both large and fast
  @inlinable
  internal var largeFastIsNative: Bool {
    @inline(__always) get {
      _internalInvariant(isLarge && providesFastUTF8)
      return (discriminatedObjectRawBits & 0x0800_0000_0000_0000) == 0
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
      _internalInvariant(isLarge)
      return (discriminatedObjectRawBits & 0x4000_0000_0000_0000) != 0
    }
  }
}


/*

 On 64-bit platforms, small strings have the following per-byte layout. When
 stored in memory (little-endian), their first character ('a') is in the lowest
 address and their top-nibble and count is in the highest address.

 ┌───────────────────────────────┬─────────────────────────────────────────────┐
 │ _countAndFlags                │ _object                                     │
 ├───┬───┬───┬───┬───┬───┬───┬───┼───┬───┬────┬────┬────┬────┬────┬────────────┤
 │ 0 │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │ 9 │ 10 │ 11 │ 12 │ 13 │ 14 │     15     │
 ├───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼────┼────┼────┼────┼────┼────────────┤
 │ a │ b │ c │ d │ e │ f │ g │ h │ i │ j │ k  │ l  │ m  │ n  │ o  │ 1x0x count │
 └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴────┴────┴────┴────┴────┴────────────┘

 On 32-bit platforms, we have less space to store code units, and it isn't
 contiguous. However, we still use the above layout for the RawBitPattern
 representation.

 ┌───────────────┬───────────────────┬───────┬─────────┐
 │ _count        │_variant .immortal │_discr │ _flags  │
 ├───┬───┬───┬───┼───┬───┬───┬───┬───┼───────┼────┬────┤
 │ 0 │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │   9   │ 10 │ 11 │
 ├───┼───┼───┼───┼───┴───┴───┴───┴───┼───────┼────┼────┤
 │ a │ b │ c │ d │   e   f   g   h   │x10 cnt│ i  │ j  │
 └───┴───┴───┴───┴───────────────────┴───────┴────┴────┘

 */
extension _StringObject {
#if arch(i386) || arch(arm)
  @inlinable @inline(__always)
  internal init(
    leadingFour countBits: Int,
    nextFour variantBits: UInt,
    smallDiscriminatorAndCount discriminatorBits: UInt64,
    trailingTwo flagBits: UInt16
  ) {
    _internalInvariant(
      discriminatorBits & 0xA000_0000_0000_0000 == 0xA000_0000_0000_0000)

    self.init(
      count: countBits,
      variant: .immortal(variantBits),
      discriminator: discriminatorBits,
      flags: flagBits)
    _internalInvariant(isSmall)
  }

  @inlinable @inline(__always)
  internal init(_ small: _SmallString) {
    // On 32-bit, we need to unpack the small string.
    let (word1, word2) = small.rawBits
    self.init(
      leadingFour: Int(truncatingIfNeeded: word1),
      nextFour: UInt(truncatingIfNeeded: word1 &>> 32),
      smallDiscriminatorAndCount:
        word2 & _StringObject.Nibbles.discriminatorMask,
      trailingTwo: UInt16(truncatingIfNeeded: word2))
    _internalInvariant(isSmall)
  }
#else
  @inlinable @inline(__always)
  internal init(_ small: _SmallString) {
    self.init(rawValue: small.rawBits)
    _internalInvariant(isSmall)
  }
#endif

  @inlinable
  internal static func getSmallCount(fromRaw x: UInt64) -> Int {
    return Int(truncatingIfNeeded: (x & 0x0F00_0000_0000_0000) &>> 56)
  }

  @inlinable
  internal var smallCount: Int {
    @inline(__always)
    get {
      _internalInvariant(isSmall)
      return _StringObject.getSmallCount(fromRaw: discriminatedObjectRawBits)
    }
  }

  @inlinable
  internal static func getSmallIsASCII(fromRaw x: UInt64) -> Bool {
    return x & 0x4000_0000_0000_0000 != 0
  }
  @inlinable
  internal var smallIsASCII: Bool {
    @inline(__always)
    get {
      _internalInvariant(isSmall)
      return _StringObject.getSmallIsASCII(fromRaw: discriminatedObjectRawBits)
    }
  }

  @inlinable @inline(__always)
  internal init(empty:()) {
    // Canonical empty pattern: small zero-length string
#if arch(i386) || arch(arm)
    self.init(
      count: 0,
      variant: .immortal(0),
      discriminator: Nibbles.emptyString,
      flags: 0)
#else
    self._countAndFlags = CountAndFlags(zero:())
    self._object = Builtin.valueToBridgeObject(Nibbles.emptyString._value)
#endif
    _internalInvariant(self.smallCount == 0)
    _invariantCheck()
  }
}

/*

 // TODO(String docs): Combine this with Nibbles table, and perhaps small string
 // table, into something that describes the higher-level structure of
 // _StringObject.

 All non-small forms share the same structure for the other half of the bits
 (i.e. non-object bits) as a word containing code unit count and various
 performance flags. The top 16 bits are for performance flags, which are not
 semantically relevant but communicate that some operations can be done more
 efficiently on this particular string, and the lower 48 are the code unit
 count (aka endIndex).

┌─────────┬───────┬────────┬───────┐
│   b63   │  b62  │ b61:48 │ b47:0 │
├─────────┼───────┼────────┼───────┤
│ isASCII │ isNFC │ TBD    │ count │
└─────────┴───────┴────────┴───────┘

 isASCII: set when all code units are known to be ASCII, enabling:
   - Trivial Unicode scalars, they're just the code units
   - Trivial UTF-16 transcoding (just bit-extend)
   - Also, isASCII always implies isNFC
 isNFC: set when the contents are in normal form C, enable:
   - Trivial lexicographical comparisons: just memcmp

 Allocation of more performance flags is TBD, un-used bits will be reserved for
 future use. Count stores the number of code units: corresponds to `endIndex`.

*/
extension _StringObject.CountAndFlags {
  @inlinable @inline(__always)
  internal init(count: Int) {
    let countUInt = UInt64(truncatingIfNeeded: count)
    _internalInvariant(countUInt <= _StringObject.CountAndFlags.countMask)

    self._storage = countUInt
    _invariantCheck()
  }

  @inlinable @inline(__always)
  internal init(count: Int, flags: UInt16) {
    let rawBits = UInt64(truncatingIfNeeded: flags) &<< 48
                | UInt64(truncatingIfNeeded: count)
    self.init(raw: rawBits)
    _internalInvariant(self.count == count && self.flags == flags)
  }

  @inlinable @inline(__always)
  internal init(count: Int, isASCII: Bool) {
    self.init(count: count)
    if isASCII {
      // ASCII implies NFC
      self._storage |= 0xC000_0000_0000_0000
    }
    _invariantCheck()
  }

  @inlinable @inline(__always)
  internal static var countMask: UInt64 { return 0x0000_FFFF_FFFF_FFFF }

  @inlinable @inline(__always)
  internal static var flagsMask: UInt64 { return ~countMask }

  @inlinable @inline(__always)
  internal var count: Int {
    return Int(
      truncatingIfNeeded: _storage & _StringObject.CountAndFlags.countMask)
  }
  @inlinable @inline(__always)
  internal var flags: UInt16 {
    return UInt16(truncatingIfNeeded: _storage &>> 48)
  }

  @inlinable
  internal var isASCII: Bool {
    return 0 != _storage & 0x8000_0000_0000_0000
  }
  @inlinable
  internal var isNFC: Bool {
    return 0 != _storage & 0x4000_0000_0000_0000
  }

  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    if isASCII {
      _internalInvariant(isNFC)
    }
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

// Extract
extension _StringObject {
  @inlinable @inline(__always)
  internal var largeCount: Int {
    _internalInvariant(isLarge)
    return _countAndFlags.count
  }

  @inlinable
  internal var largeAddressBits: UInt {
    @inline(__always) get {
      _internalInvariant(isLarge)
      return UInt(truncatingIfNeeded:
        discriminatedObjectRawBits & Nibbles.largeAddressMask)
    }
  }

  @inlinable
  internal var nativeUTF8Start: UnsafePointer<UInt8> {
    @inline(__always) get {
      _internalInvariant(largeFastIsNative)
      return UnsafePointer(
        bitPattern: largeAddressBits &+ _StringObject.nativeBias
      )._unsafelyUnwrappedUnchecked
    }
  }

  @inlinable
  internal var nativeUTF8: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      _internalInvariant(largeFastIsNative)
      return UnsafeBufferPointer(start: nativeUTF8Start, count: largeCount)
    }
  }

  // Resilient way to fetch a pointer
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func getSharedUTF8Start() -> UnsafePointer<UInt8> {
    _internalInvariant(largeFastIsShared)
#if _runtime(_ObjC)
    if largeIsCocoa {
      return _cocoaUTF8Pointer(cocoaObject)._unsafelyUnwrappedUnchecked
    }
#endif

    return sharedStorage.start
  }

  @usableFromInline
  internal var sharedUTF8: UnsafeBufferPointer<UInt8> {
    @_effects(releasenone) @inline(never) get {
      _internalInvariant(largeFastIsShared)
      let start = self.getSharedUTF8Start()
      return UnsafeBufferPointer(start: start, count: largeCount)
    }
  }

  internal var nativeStorage: _StringStorage {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      guard case .native(let storage) = _variant else {
        _internalInvariantFailure()
      }
      return _unsafeUncheckedDowncast(storage, to: _StringStorage.self)
#else
      _internalInvariant(hasNativeStorage)
      return Builtin.reinterpretCast(largeAddressBits)
#endif
    }
  }

  internal var sharedStorage: _SharedStringStorage {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      guard case .native(let storage) = _variant else {
        _internalInvariantFailure()
      }
      return _unsafeUncheckedDowncast(storage, to: _SharedStringStorage.self)
#else
      _internalInvariant(largeFastIsShared && !largeIsCocoa)
      _internalInvariant(hasSharedStorage)
      return Builtin.reinterpretCast(largeAddressBits)
#endif
    }
  }

  internal var cocoaObject: AnyObject {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      guard case .bridged(let object) = _variant else {
        _internalInvariantFailure()
      }
      return object
#else
      _internalInvariant(largeIsCocoa && !isImmortal)
      return Builtin.reinterpretCast(largeAddressBits)
#endif
    }
  }
}

// Aggregate queries / abstractions
extension _StringObject {
  // The number of code units stored
  //
  // TODO(String micro-performance): Check generated code
  @inlinable
  internal var count: Int {
    @inline(__always) get { return isSmall ? smallCount : largeCount }
  }

  //
  // Whether the string is all ASCII
  //
  @inlinable
  internal var isASCII: Bool {
    @inline(__always) get {
      if isSmall { return smallIsASCII }
      return _countAndFlags.isASCII
    }
  }

  @inlinable
  internal var isNFC: Bool {
    @inline(__always) get {
      if isSmall {
        // TODO(String performance): Worth implementing more sophisiticated
        // check, or else performing normalization on- construction. For now,
        // approximate it with isASCII
        return smallIsASCII
      }
      return _countAndFlags.isNFC
    }
  }

  // Get access to fast UTF-8 contents for large strings which provide it.
  @inlinable
  internal var fastUTF8: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      _internalInvariant(self.isLarge && self.providesFastUTF8)
      if _slowPath(self.largeFastIsShared) {
        return sharedUTF8
      }
      return UnsafeBufferPointer(
        start: self.nativeUTF8Start, count: self.largeCount)
    }
  }

  // Whether the object stored can be bridged directly as a NSString
  @usableFromInline // @opaque
  internal var hasObjCBridgeableObject: Bool {
    @_effects(releasenone) get {
      // Currently, all mortal objects can zero-cost bridge
      return !self.isImmortal
    }
  }

  // Fetch the stored subclass of NSString for bridging
  @inlinable
  internal var objCBridgeableObject: AnyObject {
    @inline(__always) get {
      _internalInvariant(hasObjCBridgeableObject)
      return Builtin.reinterpretCast(largeAddressBits)
    }
  }

  // Whether the object provides fast UTF-8 contents that are nul-terminated
  @inlinable
  internal var isFastZeroTerminated: Bool {
    if _slowPath(!providesFastUTF8) { return false }

    // Small strings nul-terminate when spilling for contiguous access
    if isSmall { return true }

    // TODO(String performance): Use performance flag, which could be more
    // inclusive. For now, we only know native strings and small strings (when
    // accessed) are. We could also know about some shared strings.

    return largeFastIsNative
  }
}

// Object creation
extension _StringObject {
  @inlinable @inline(__always)
  internal init(immortal bufPtr: UnsafeBufferPointer<UInt8>, isASCII: Bool) {
    let countAndFlags = CountAndFlags(count: bufPtr.count, isASCII: isASCII)
#if arch(i386) || arch(arm)
    self.init(
      variant: .immortal(start: bufPtr.baseAddress._unsafelyUnwrappedUnchecked),
      discriminator: Nibbles.largeImmortal(),
      countAndFlags: countAndFlags)
#else
    // We bias to align code paths for mortal and immortal strings
    let biasedAddress = UInt(
      bitPattern: bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    ) &- _StringObject.nativeBias

    self.init(
      pointerBits: UInt64(truncatingIfNeeded: biasedAddress),
      discriminator: Nibbles.largeImmortal(),
      countAndFlags: countAndFlags)
#endif
  }

  @inline(__always)
  internal init(_ storage: _StringStorage) {
#if arch(i386) || arch(arm)
    self.init(
      variant: .native(storage),
      discriminator: Nibbles.largeMortal(),
      countAndFlags: storage._countAndFlags)
#else
    self.init(
      object: storage,
      discriminator: Nibbles.largeMortal(),
      countAndFlags: storage._countAndFlags)
#endif
  }

  internal init(_ storage: _SharedStringStorage, isASCII: Bool) {
#if arch(i386) || arch(arm)
    self.init(
      variant: .native(storage),
      discriminator: Nibbles.largeSharedMortal(),
      countAndFlags: storage._countAndFlags)
#else
    self.init(
      object: storage,
      discriminator: Nibbles.largeSharedMortal(),
      countAndFlags: storage._countAndFlags)
#endif
  }

  internal init(
    cocoa: AnyObject, providesFastUTF8: Bool, isASCII: Bool, length: Int
  ) {
    let countAndFlags = CountAndFlags(count: length, isASCII: isASCII)
    let discriminator = Nibbles.largeCocoa(providesFastUTF8: providesFastUTF8)
#if arch(i386) || arch(arm)
    self.init(
      variant: .bridged(cocoa),
      discriminator: discriminator,
      countAndFlags: countAndFlags)
#else
    self.init(
      object: cocoa, discriminator: discriminator, countAndFlags: countAndFlags)
    _internalInvariant(self.largeAddressBits == Builtin.reinterpretCast(cocoa))
    _internalInvariant(self.providesFastUTF8 == providesFastUTF8)
    _internalInvariant(self.largeCount == length)
#endif
  }
}

// Internal invariants
extension _StringObject {
  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    #if arch(i386) || arch(arm)
    _internalInvariant(MemoryLayout<_StringObject>.size == 12)
    _internalInvariant(MemoryLayout<_StringObject>.stride == 12)
    _internalInvariant(MemoryLayout<_StringObject>.alignment == 4)

    _internalInvariant(MemoryLayout<_StringObject?>.size == 12)
    _internalInvariant(MemoryLayout<_StringObject?>.stride == 12)
    _internalInvariant(MemoryLayout<_StringObject?>.alignment == 4)
    #else
    _internalInvariant(MemoryLayout<_StringObject>.size == 16)

    _internalInvariant(MemoryLayout<_StringObject?>.size == 16)
    #endif

    if isForeign {
      _internalInvariant(largeIsCocoa, "No other foreign forms yet")
    }

    if isSmall {
      _internalInvariant(isImmortal)
      _internalInvariant(smallCount <= 15)
      _internalInvariant(smallCount == count)
      _internalInvariant(!hasObjCBridgeableObject)
    } else {
      _internalInvariant(isLarge)
      _internalInvariant(largeCount == count)
      if providesFastUTF8 && largeFastIsNative {
        _internalInvariant(!isSmall)
        _internalInvariant(!largeIsCocoa)

        if isImmortal {
          _internalInvariant(!hasNativeStorage)
          _internalInvariant(!hasObjCBridgeableObject)
        } else {
          _internalInvariant(hasNativeStorage)
          _internalInvariant(hasObjCBridgeableObject)
          _internalInvariant(nativeStorage.count == self.count)
        }
      }
      if largeIsCocoa {
        _internalInvariant(hasObjCBridgeableObject)
        _internalInvariant(!isSmall)
        if isForeign {

        } else {
          _internalInvariant(largeFastIsShared)
        }
      }
    }
    #if arch(i386) || arch(arm)
    switch _variant {
    case .immortal:
      _internalInvariant(isImmortal)
    case .native:
      _internalInvariant(hasNativeStorage || hasSharedStorage)
    case .bridged:
      _internalInvariant(isLarge)
      _internalInvariant(largeIsCocoa)
    }
    #endif
  }
  #endif // INTERNAL_CHECKS_ENABLED

  @inline(never)
  internal func _dump() {
#if INTERNAL_CHECKS_ENABLED
    let raw = self.rawBits
    let word0 = ("0000000000000000" + String(raw.0, radix: 16)).suffix(16)
    let word1 = ("0000000000000000" + String(raw.1, radix: 16)).suffix(16)
#if arch(i386) || arch(arm)
    print("""
      StringObject(\
      <\(word0) \(word1)> \
      count: \(String(_count, radix: 16)), \
      variant: \(_variant), \
      discriminator: \(_discriminator), \
      flags: \(_flags))
      """)
#else
    print("StringObject(<\(word0) \(word1)>)")
#endif
    let repr = _StringGuts(self)._classify()
    switch repr._form {
    case ._small:
      _SmallString(self)._dump()
    case ._immortal(address: let address):
      print("""
        Immortal(\
        start: \(UnsafeRawPointer(bitPattern: address)!), \
        count: \(repr._count))
        """)
    case ._native(_):
      print("""
        Native(\
        owner: \(repr._objectIdentifier!), \
        count: \(repr._count), \
        capacity: \(repr._capacity))
        """)
    case ._cocoa(object: let object):
      let address: UnsafeRawPointer = Builtin.reinterpretCast(object)
      print("Cocoa(address: \(address))")
    }
#endif // INTERNAL_CHECKS_ENABLED
  }
}
