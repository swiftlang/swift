// TODO
import SwiftShims

//
// Largely inspired by string-recore at
// https://github.com/apple/swift/pull/10747.
//

//
// StringGuts is always 16 bytes on both 32bit and 64bit platforms. This
// effectively makes the worse-case (from a spare bit perspective) ABIs be the
// 64bit ones, as all 32bit pointers effectively have a 32-bit address space
// while the 64bit ones have a 56-bit address space.
//
// Of the 64bit ABIs, x86_64 has the fewest spare bits, so that's the ABI we
// design for.
//
// FIXME: what about ppc64 and s390x?
//

@_fixed_layout
public // FIXME
struct _StringGuts {
  // TODO 32-bit: Insert padding between fields
  public // FIXME for testing only
  var _storage: (_BuiltinBridgeObject, UInt)

  var _object: _BuiltinBridgeObject {
    get { return _storage.0 }
    set { _storage.0 = newValue }
  }
  var _otherBits: UInt {
    get { return _storage.1 }
    set { _storage.1 = newValue }
  }
  var _objectBitPattern: UInt {
    get { return _bitPattern(_object) }
    set { _object = Builtin.reinterpretCast(newValue) }
  }

  internal
  init(_ object: _BuiltinBridgeObject, _ otherBits: UInt) {
    self._storage.0 = object
    self._storage.1 = otherBits
  }
}

//
// Discriminators
//

// FIXME: Define this properly per ABI. For now, it's taking advantage of tag
// bits being highest (and possibly lowest but shifted away). Furthermore, these
// won't work out-of-the-box for 32bit platforms.

// The bit used to discriminate value or managed.
/*fileprivate*/ internal var _tagBit: UInt { return _objCTaggedPointerBits }

// When a value, the bit that discriminates a small string from an unsafe string
//
// NOTE: Until we wean ourselves off of _StringCore, the only "small" strings we
// can store are tagged NSStrings. For now, this is synonymous with a tagged
// NSString but this will be expanded in the future to store other forms.
//
/*fileprivate*/ internal var _smallBit: UInt { return _tagBit >> 1 }

// When managed, or an unsafe string, the bit used to discriminate two-byte or
// one-byte code units.
/*fileprivate*/ internal var _twoByteCodeUnitBit: UInt { return _smallBit >> 1 }

//
// Flags
//
// TODO: It would be more clear to wrap up BridgeObject in a
// "StringObject"-like struct, and provide this funcitonality on that.
//
extension _StringGuts {
  /*private*/ internal var _flagsMask: UInt {
    return _twoByteCodeUnitBit
  }

  @_versioned
  var isSingleByte: Bool {
    switch classification {
    case .native, .unsafe, .nonTaggedCocoa:
      return (_objectBitPattern & _twoByteCodeUnitBit) == 0
    case .smallCocoa, .error:
      return false
    }
  }

  var byteWidth: Int {
    return isSingleByte ? 1 : 2
  }

  // NOTE: Currently, single byte representation is synonymous with ASCII. This
  // may change in the future in preference of either UTF8 or Latin1.
  @_inlineable
  @_versioned
  internal
  var isASCII: Bool {
    return isSingleByte
  }

  var _unflaggedObject: _BuiltinBridgeObject {
    _sanityCheck(!_isSmallCocoa, "TODO: drop small cocoa")
    return Builtin.reinterpretCast(_objectBitPattern & ~_flagsMask)
  }

  var _isTagged: Bool { return _isTaggedObject(_object) }

  var _untaggedUnflaggedBitPattern: UInt {
    _sanityCheck(_isTagged)
    return _bridgeObject(toTagPayload: _unflaggedObject)
  }

  /*private*/ internal init(
    _unflagged object: _BuiltinBridgeObject,
    isSingleByte: Bool,
    otherBits: UInt
  ) {
    self.init(object, otherBits)
    if !isSingleByte {
      self._objectBitPattern |= _twoByteCodeUnitBit
    }

    _sanityCheck(_bitPattern(self._unflaggedObject) == _bitPattern(object))
    _invariantCheck()
  }

  /// Create the guts of an empty string.
  @_inlineable
  public init() {
    self.init(UnsafeString(
        baseAddress: _emptyStringBase,
        count: 0,
        isSingleByte: true))
  }

  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    if let native = self._native {
      _sanityCheck(self.isSingleByte == native.isSingleByte)
      _sanityCheck(self.count == native.count)
      _sanityCheck(self.capacity == native.capacity)
      _sanityCheck(self.count >= 0)
      _sanityCheck(self.capacity >= self.count)
    } else if let unsafe = self._unsafeString {
      _sanityCheck(self.isSingleByte == unsafe.isSingleByte)
      _sanityCheck(self.count == unsafe.count)
      _sanityCheck(self.count >= 0)
      _sanityCheck(self.capacity == 0)
    } else if let cocoa = self._cocoa {
      _sanityCheck(self.count >= 0)
      _sanityCheck(self.capacity == 0)
      // Single-byte Cocoa strings must be contiguous
      _sanityCheck(!self.isSingleByte || cocoa.start != nil)
    } else if let _ = self._smallCocoa {
      _sanityCheck(!self.isSingleByte)
      _sanityCheck(self.count >= 0)
      _sanityCheck(self.capacity == 0)
    } else {
      fatalError("Unimplemented string form")
    }
#endif
  }
}

//
// Transient structures
//

// NOTE: Some of the following structures help me keep things straight in my
// head, but they are not necessary. Only the leaf structures (i.e.
// non-"Abstract") probably are, and even then it might be the case that only
// UnsafeString is a useful type to keep. For now, though, it illustrates the
// bit-discriminated heirarchy very explicitly.

//
// StringGuts's BridgeObject:
//   - AbstractTaggedString
//   - NativeString
//   - CocoaString
//
// AbstractTaggedString:
//   - UnsafeString
//   - AbstractSmallString
//
// AbstractSmallString:
//   - SmallCocoaString
//   - FatalErrorString (for now)
//

// A useful placeholder for more String forms, especially additional small
// string forms.
/*fileprivate*/ internal struct FatalErrorString {
  init() {
    fatalError("I AM ERROR")
  }
}

/*fileprivate*/ internal struct SmallCocoaString {
  // TODO: Use the extra 60 bits
  //
  // StringGuts's second word is entirely taken up by tagged pointer bit
  // pattern. The x86_64 BridgeObject uses 4 bits to designate a
  // CocoaSmallString, 2 for tagged, one for small, one for small-cocoa.
  //
  // Consider caching flags, length, etc. Anything we want to compute once but
  // read multiple times we can cache away to avoid excessive CF calls.
  //
  // NOTE: Alternatively, we could bake in deep knowledge of tagged NSStrings
  // and use a non-NSString tagged patterns to designate Unsafe or other small
  // forms. This would give us a full 64 bits of extra storage in the second
  // word. Probably not worth it at this time.
  //

  // The Cocoa tagged pointer, treated as an opaque bit pattern. This is stored
  // verbatim in the second word of StringGuts.
  let taggedPointer: UInt

  init(_ tagBitPattern: UInt) {
    _sanityCheck(_isObjCTaggedPointer(tagBitPattern))
    self.taggedPointer = tagBitPattern
  }
  init(_ object: AnyObject) {
    _sanityCheck(_isObjCTaggedPointer(object))
    self.taggedPointer = Builtin.reinterpretCast(object)
  }

  var taggedObject: AnyObject {
    return Builtin.reinterpretCast(taggedPointer)
  }
}

@_fixed_layout
@_versioned
internal struct UnsafeString {
  // TODO: Use the extra 13 bits
  //
  // StringGuts when representing UnsafeStrings should have an extra 13 bits *at
  // least* to store whatever we want, e.g. flags. x86_64 ABI has at least 13
  // bits due to:
  //   * 8 bits from count: 56-bit (max) address spaces means we need at most
  //     56-bit count
  //   * 5 bits from BridgeObject: 64 - 2 tagging - 56-bit address space - 1 bit
  //     designating UnsafeString
  //

  @_versioned
  internal var baseAddress: UnsafeRawPointer

  @_versioned
  internal var count: Int

  @_versioned
  internal var isSingleByte: Bool

  @_versioned
  internal var sizeInBytes: Int {
    return count * byteWidth
  }

  @_versioned
  internal var byteWidth: Int {
    return isSingleByte ? 1 : 2
  }

  @_inlineable
  @_versioned
  internal
  var isASCII: Bool {
    // NOTE: For now, single byte means ASCII. Might change in future
    return isSingleByte
  }

  @_versioned
  init(
    baseAddress: UnsafeRawPointer,
    count: Int,
    isSingleByte: Bool
  ) {
    self.baseAddress = baseAddress
    self.count = count
    self.isSingleByte = isSingleByte
  }

  @_versioned
  var utf16Buffer: UnsafeBufferPointer<UInt16> {
    _sanityCheck(!isSingleByte)
    return UnsafeBufferPointer(
      start: baseAddress.assumingMemoryBound(to: UInt16.self),
      count: count)
  }
  @_versioned
  var asciiBuffer: UnsafeBufferPointer<UInt8> {
    _sanityCheck(isSingleByte)
    return UnsafeBufferPointer(
      start: baseAddress.assumingMemoryBound(to: UInt8.self),
      count: count)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal subscript(position: Int) -> UTF16.CodeUnit {
    @inline(__always)
    get {
      _sanityCheck(
        position >= 0,
        "subscript: index precedes String start")
      _sanityCheck(
        position <= count,
        "subscript: index points past String end")
      if isSingleByte {
        return UTF16.CodeUnit(asciiBuffer[position])
      }
      return utf16Buffer[position]
    }
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal subscript(bounds: Range<Int>) -> UnsafeString {
    _sanityCheck(
      bounds.lowerBound >= 0,
      "subscript: subrange start precedes String start")
    _sanityCheck(
      bounds.upperBound <= count,
      "subscript: subrange extends past String end")
    return UnsafeString(
      baseAddress: _pointer(toElementAt: bounds.lowerBound),
      count: bounds.upperBound - bounds.lowerBound,
      isSingleByte: self.isSingleByte)
  }

  /// Returns a pointer to the Nth element of contiguous
  /// storage.  Caveats: The element may be 1 or 2 bytes wide,
  /// depending on element width.
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _pointer(toElementAt n: Int) -> UnsafeRawPointer {
    _sanityCheck(n >= 0 && n <= count)
    return baseAddress + (n &<< (byteWidth &- 1))
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _copy(
    into dest: UnsafeMutableRawPointer,
    capacityEnd: UnsafeMutableRawPointer,
    accomodatingElementWidth width: Int
  ) {
    _sanityCheck(width == 1 || width == 2)
    let elementShift = width &- 1
    _sanityCheck(capacityEnd >= dest + self.count &<< elementShift)

    if _fastPath(self.byteWidth == width) {
      _memcpy(
        dest: dest,
        src: self.baseAddress,
        size: UInt(self.sizeInBytes))
    } else if self.byteWidth == 1 && width == 2 {
      var dest = dest.assumingMemoryBound(to: UTF16.CodeUnit.self)
      for byte in self.asciiBuffer {
        dest.pointee = UTF16.CodeUnit(byte)
        dest += 1
      }
    } else {
      _sanityCheck(self.byteWidth == 2 && width == 1)
      var dest = dest.assumingMemoryBound(to: UInt8.self)
      for unit in self.utf16Buffer {
        _sanityCheck(unit & ~0x7F == 0) // ASCII only
        dest.pointee = UInt8(truncatingIfNeeded: unit)
        dest += 1
      }
    }
  }
}

@_versioned
/*fileprivate*/ internal
struct NativeString {
  // TODO: Use the extra 72 bits.
  //
  // StringGuts when representing a native Swift string should have an extra 72
  // bits *at least* to store whatever is most profitable (e.g. flags and a
  // count). x86_64 has at least 72 bits available due to:
  //
  // * 8 bits from BridgeObject: 11 spare bits from native references - 2
  //   (conservatively) for tagging - 1 to designate native vs objc reference.
  // * 64 bits from the second word

  @_versioned
  var stringBuffer: _StringBuffer

  @_versioned
  @_inlineable
  var nativeObject: AnyObject {
    return stringBuffer._storage.buffer
  }

  var unsafe: UnsafeString {
    return UnsafeString(
      baseAddress: self.baseAddress,
      count: self.count,
      isSingleByte: self.isSingleByte)
  }

  var baseAddress: UnsafeMutableRawPointer {
    return stringBuffer.start
  }

  var count: Int {
    return stringBuffer.usedCount
  }

  var capacity: Int {
    return stringBuffer.capacity
  }

  var isSingleByte: Bool {
    _sanityCheck((0...1).contains(stringBuffer.elementShift))
    return stringBuffer.elementShift == 0
  }

  var byteWidth: Int {
    return isSingleByte ? 1 : 2
  }

  @_versioned
  init(_ native: _BuiltinNativeObject) {
    // TODO: Does this in practice incur overhead? Should we cast it?
    let storage = _StringBuffer._Storage(_nativeObject: native)
    self.stringBuffer = _StringBuffer(storage)
  }

  @_versioned
  internal init(_ buffer: _StringBuffer) {
    self.stringBuffer = buffer
  }

  init(_ object: AnyObject) {
    self.init(
      _StringBuffer(_StringBuffer._Storage(
        _uncheckedUnsafeBufferObject: object
      ))._nativeObject)
  }

  // Copy in `other` code units direclty.
  @_versioned
  internal
  // FIXME: mutating
  func _appendInPlace(_ other: _StringGuts) {
    let otherCount = other.count
    _sanityCheck(self.capacity >= self.count + otherCount)

    // TODO: Does this incur ref counting?
    var buffer = self.stringBuffer

    other._copy(
      into: buffer.usedEnd,
      capacityEnd: buffer.capacityEnd,
      accomodatingElementWidth: buffer.elementWidth)
    buffer.usedEnd += otherCount &<< buffer.elementShift
  }
}

/*fileprivate*/ internal struct NonTaggedCocoaString {
  // TODO: Use the extra 72 bits. For now, we cargo-cult legacy string core
  //
  // StringGuts when representing a non-tagged ObjC string should have an extra
  // 72 bits *at least* to store whatever is most profitable (e.g. flags and an
  // optional contiguous pointer). x86_64 has at least 72 bits available due to:
  //
  // * 8 bits from BridgeObject: 11 spare bits from native references - 2
  //   (conservatively) for tagging - 1 to designate native vs objc reference.
  // * 64 bits from the second word

  // TODO: AnyObject ABI encompases tagged pointers too. What we could use is a
  // NonTaggedObjCObject.
  let owner: AnyObject
  let isSingleByte: Bool
  let start: UnsafeRawPointer?

  init(_ owner: AnyObject, isSingleByte: Bool, start: UnsafeRawPointer?) {
    self.owner = owner
    self.isSingleByte = isSingleByte
    self.start = start
  }

  init(_ owner: AnyObject) {
    self.init(owner, isSingleByte: false, start: nil)
  }

  internal var unsafe: UnsafeString? {
    guard let start = start else { return nil }
    let count = _stdlib_binary_CFStringGetLength(owner)
    return UnsafeString(
      baseAddress: UnsafeMutableRawPointer(mutating: start),
      count: count,
      isSingleByte: isSingleByte)
  }
}

@_versioned
@_fixed_layout
internal struct OpaqueCocoaString {
  @_versioned
  let object: AnyObject

  @_versioned
  let count: Int

  init(_ object: AnyObject) {
    self.init(object, count: _stdlib_binary_CFStringGetLength(object))
  }

  init(_ object: AnyObject, count: Int) {
    self.object = object
    self.count = count
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(position: Int) -> UTF16.CodeUnit {
    return _cocoaStringSubscript(object, position)
  }

  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(bounds: Range<Int>) -> _StringGuts {
    return _makeCocoaStringGuts(_cocoaStringSlice(object, bounds))
  }
}


//
// Masks
//
/*fileprivate*/ internal var _smallStringMask: UInt {
  return _tagBit | _smallBit
}

extension _StringGuts {
  enum Form {
    case native
    case nonTaggedCocoa
    case unsafe
    case smallCocoa
    case error
  }

  // TODO(performance): Make sure this generates sensible code
  /*fileprivate*/ internal // TODO: private in Swift 4
  var classification: Form {
    @inline(__always) get {
      if _isNative { return .native }
      if _isNonTaggedCocoa { return .nonTaggedCocoa }
      if _isUnsafe { return .unsafe }
      _sanityCheck(_isSmallCocoa)
      return .smallCocoa
    }
  }
}

extension _StringGuts {
  //
  // Native Swift Strings
  //

  ///*fileprivate*/ internal // TODO: private in Swift 4
  public // TODO(StringGuts): for testing only
  var _isNative: Bool {
    return _isNativePointer(_object)
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _asNative: NativeString {
    _sanityCheck(_isNative)
    let nativeObject = _nativeObject(fromBridge: _object)
    return NativeString(nativeObject)
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _native: NativeString? {
    guard _isNative else { return nil }
    return _asNative
  }

  @_versioned
  /*fileprivate*/ internal // TODO: private in Swift 4
  init(_ s: NativeString) {
    self.init(s.stringBuffer)
  }

  @_versioned
  init(_ buffer: _StringBuffer) {
    self.init(
      _unflagged: _bridgeObject(fromNativeObject: buffer._nativeObject),
      isSingleByte: buffer.elementWidth == 1,
      otherBits: 0)
  }

  //
  // Cocoa (non-tagged) Strings
  //
  ///*fileprivate*/ internal // TODO: private in Swift 4
  public // TODO(StringGuts): for testing only
  var _isNonTaggedCocoa: Bool {
    return _isNonTaggedObjCPointer(_object)
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _cocoa: NonTaggedCocoaString? {
    guard _isNonTaggedCocoa else { return nil }
    return NonTaggedCocoaString(
      _bridgeObject(toNonTaggedObjC: _object),
      isSingleByte: self.isSingleByte,
      start: UnsafeRawPointer(bitPattern: _otherBits))
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  init(_ s: NonTaggedCocoaString) {
    _sanityCheck(!_isObjCTaggedPointer(s.owner))
    self.init(
      _unflagged: _bridgeObject(fromNonTaggedObjC: s.owner),
      isSingleByte: s.isSingleByte,
      otherBits: UInt(bitPattern: s.start))
  }

  //
  // Unsafe Strings
  //

  ///*fileprivate*/ internal // TODO: private in Swift 4
  public // TODO(StringGuts): for testing only
  var _isUnsafe: Bool {
    return _isTagged && _objectBitPattern & _smallBit == 0
  }

  @_versioned
  var _unsafeString: UnsafeString? {
    guard _isUnsafe else { return nil }

    // Unflag it, untag it, and go
    let pointer = UnsafeMutableRawPointer(
      bitPattern: self._untaggedUnflaggedBitPattern
    )._unsafelyUnwrappedUnchecked
    return UnsafeString(
      baseAddress: pointer,
      count: Int(self._otherBits),
      isSingleByte: self.isSingleByte)
  }

  @_versioned
  init(_ s: UnsafeString) {
    // Tag it, flag it, and go
    let object = _bridgeObject(taggingPayload: UInt(bitPattern: s.baseAddress))
    self.init(
      _unflagged: object,
      isSingleByte: s.isSingleByte,
      otherBits: UInt(s.count))
  }

  //
  // Tagged Cocoa Strings
  //

  ///*fileprivate*/ internal // TODO: private in Swift 4
  public // TODO(StringGuts): for testing only
  var _isSmallCocoa: Bool {
    return _isTagged && _objectBitPattern & _smallBit != 0
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _smallCocoa: SmallCocoaString? {
    guard _isSmallCocoa else { return nil }
    return SmallCocoaString(_otherBits)
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  init(_ s: SmallCocoaString) {
    self.init(_bridgeObject(fromTagged: _smallStringMask), s.taggedPointer)
  }

  //
  // Small Strings
  //
  ///*fileprivate*/ internal // TODO: private in Swift 4
  public // TODO(StringGuts): for testing only
  var _isError: Bool { return false }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _error: FatalErrorString? {
    guard _isError else { return nil }
    return FatalErrorString()
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  init(_ s: FatalErrorString) {
    fatalError("I AM ERROR")
  }

}

// Internal hex dumping function. Useful because `print` is implemented in the
// stdlib through a series of very high level String calls, resulting in
// infinite recursion.
internal func internalDumpHexImpl(_ x: UInt, newline: Bool) {
  _swift_stdlib_print_hex(x, newline ? 1 : 0)
}
internal func internalDumpHex(_ x: UInt, newline: Bool = true) {
  internalDumpHexImpl(x, newline: newline)
}
internal func internalDumpHex(_ x: AnyObject, newline: Bool = true) {
  internalDumpHexImpl(Builtin.reinterpretCast(x), newline: newline)
}
internal func internalDumpHex(_ x: UnsafeRawPointer?, newline: Bool = true) {
  internalDumpHexImpl(Builtin.reinterpretCast(x), newline: newline)
}
internal func internalDumpHex(_ x: Bool, newline: Bool = true) {
  internalDumpHexImpl(x ? 1 : 0, newline: newline)
}


extension _StringGuts {
  enum Stats {
    internal static var numNativeSelfSlice = 0
    internal static var numCocoaSelfSlice = 0
  }
}
//
// Stats
//

extension _LegacyStringCore {
  @_versioned // FIXME(sil-serialize-all)
  internal func _copyToStringBuffer() -> _StringBuffer {
    var copy = self
    copy._copyInPlace(
      newSize: self.count,
      newCapacity: self.count,
      minElementWidth: self.elementWidth)
    return copy.nativeBuffer!
  }
}

// Conversions two/from legacy core
extension _StringGuts {
  var _legacyCore: _LegacyStringCore {
    if let unsafeString = self._unsafeString {
      return _LegacyStringCore(
        baseAddress: UnsafeMutableRawPointer(
          mutating: unsafeString.baseAddress),
        count: unsafeString.count,
        elementShift: unsafeString.isSingleByte ? 0 : 1,
        hasCocoaBuffer: false,
        owner: nil)
    }
    if let cocoa = self._cocoa {
      return makeCocoaLegacyStringCore(_cocoaString: cocoa.owner)
    }
    if let native = self._native {
      return _LegacyStringCore(native.stringBuffer)
    }
    if let smallCocoa = self._smallCocoa {
      return makeCocoaLegacyStringCore(_cocoaString: smallCocoa.taggedObject)
    }
    if let _ = self._error {
      fatalError("I AM ERROR")
    }

    fatalError("TODO: unreachable")
  }

  @_versioned
  init(_ legacyCore: _LegacyStringCore) {
    if _slowPath(legacyCore._baseAddress == nil) {
      // Opaque Cocoa string
      _sanityCheck(legacyCore.hasCocoaBuffer,
        "Non-cocoa, non-contiguous legacy String")
      _sanityCheck(legacyCore._owner != nil, "Cocoa string with no owner")
      let owner = legacyCore._owner._unsafelyUnwrappedUnchecked
      let guts = _makeCocoaStringGuts(owner)
      _sanityCheck(guts.count == legacyCore.count,
        "Self-slice of non-contiguous Cocoa string")
      self = guts
      return
    }

    let baseAddress = legacyCore._baseAddress._unsafelyUnwrappedUnchecked

    guard let owner = legacyCore._owner else {
      // Immortal String
      _sanityCheck((0...2).contains(legacyCore.elementWidth))
      let immortal = UnsafeString(
        baseAddress: baseAddress,
        count: legacyCore.count,
        isSingleByte: legacyCore.elementWidth == 1)
      self.init(immortal)
      return
    }

    if _slowPath(legacyCore.hasCocoaBuffer) {
      let guts = _makeCocoaStringGuts(owner)
      // NOTE: Sometimes a _LegacyStringCore is a self-slice of a Cocoa string
      // without having properly sliced the backing Cocoa string itself. Detect
      // that situation in retrospect and create a native copy.
      if _slowPath(guts.count != legacyCore.count) {
        Stats.numCocoaSelfSlice += 1
        self.init(legacyCore._copyToStringBuffer())
        return
      }
      self = guts
      return
    }

    let nativeBuffer = unsafeBitCast(owner, to: _StringBuffer.self)

    // Check for a self-slice, in which case we'll work around that temporarily
    // by copying out the contents into a new native string.
    //
    // TODO: Forbid this. We need to audit all uses of ephemeralString and
    // switch everything possible over to UnsafeString. Besides ephemeralString,
    // many of the views still slice and dice _LegacyStringCore, need to audit
    // those too.
    //
    if _slowPath(nativeBuffer.usedCount != legacyCore.count) {
      Stats.numNativeSelfSlice += 1
      self.init(legacyCore._copyToStringBuffer())
      return
    }
    self.init(nativeBuffer)
  }
}

extension _StringGuts {
  public func _dump() {
    print("<", terminator: "")
    internalDumpHex(_objectBitPattern, newline: false)
    print(" ", terminator: "")
    internalDumpHex(_otherBits, newline: false)
    if let native = self._native {
      print(" native ", terminator: "")
      internalDumpHex(native.nativeObject, newline: false)
      print(" @", terminator: "")
      internalDumpHex(native.baseAddress, newline: false)
      print(" ", terminator: "")
      print(native.count, terminator: "")
      print("/", terminator: "")
      print(native.capacity, terminator: "")
    } else if let cocoa = self._cocoa {
      print(" cocoa ", terminator: "")
      internalDumpHex(cocoa.owner, newline: false)
    } else if let unsafeString  = self._unsafeString {
      print(" unsafe ", terminator: "")
      internalDumpHex(unsafeString.baseAddress, newline: false)
      print(" ", terminator: "")
      print(unsafeString.count, terminator: "")
    } else if let _ = self._smallCocoa {
      print(" smallCocoa", terminator: "")
    } else {
      print(" error", terminator: "")
    }
    if isSingleByte {
      print(" ascii", terminator: "")
    }
    else {
      print(" utf16", terminator: "")
    }
    print(">")
  }
}

// Subscript-based access
extension _StringGuts {
  /// Get the UTF-16 code unit stored at the specified position in this string.
  @_versioned
  @_inlineable // FIXME(sil-serialize-all)
  subscript(position: Int) -> UTF16.CodeUnit {
    let unsafe = self._unmanagedContiguous
    if _fastPath(unsafe != nil) {
      return unsafe._unsafelyUnwrappedUnchecked[position]
    }
    return getOpaque()[position]
  }
}

//
// String API helpers
//
extension _StringGuts {
  // Return a _StringBuffer with the same contents as this string. Uses the
  // existing buffer if possible; otherwise copies the string into a new buffer.
  @_versioned
  internal
  func _extractStringBuffer() -> _StringBuffer {
    if _fastPath(_isNative) {
      return _asNative.stringBuffer
    }
    return _copyToStringBuffer(capacity: self.count, byteWidth: self.byteWidth)
  }

  @_versioned
  internal
  func _copyToStringBuffer(capacity: Int, byteWidth: Int) -> _StringBuffer {
    _sanityCheck(capacity >= self.count)
    _sanityCheck(byteWidth == 1 || byteWidth == 2)

    let buffer = _StringBuffer(
      capacity: capacity,
      initialSize: self.count,
      elementWidth: byteWidth)
    _sanityCheck(buffer.capacity >= capacity)

    // Copy ourselves in
    self._copy(
      into: buffer.start,
      capacityEnd: buffer.capacityEnd,
      accomodatingElementWidth: byteWidth)
    return buffer
  }

  @_inlineable
  // TODO: @_versioned
  // TODO: internal
  public // TODO(StringGuts): for testing only
  mutating func isUniqueNative() -> Bool {
    return _isUnique(&_storage.0) && _isNative
  }

  @_versioned
  internal
  mutating func _ensureUniqueNative(
    minimumCapacity: Int,
    minimumByteWidth: Int
  ) {
    _sanityCheck(minimumByteWidth == 1 || minimumByteWidth == 2)
    var newCapacity = minimumCapacity
    let newWidth = Swift.max(self.byteWidth, minimumByteWidth)
    if _fastPath(_isNative) {
      let isUnique = isUniqueNative()
      let nativeBuffer = self._asNative.stringBuffer
      let oldCapacity = nativeBuffer.capacity
      if oldCapacity < minimumCapacity {
        newCapacity = Swift.max(
          _growArrayCapacity(oldCapacity),
          minimumCapacity)
      } else if _fastPath(isUnique && self.byteWidth == newWidth) {
        // TODO: width extension can be done in place if there's capacity
        return
      }
    }

    let newBuffer = _copyToStringBuffer(
      capacity: newCapacity,
      byteWidth: newWidth)
    self = _StringGuts(newBuffer)
  }

  // Convert ourselves (if needed) to a NativeString for appending purposes.
  // After this call, self is ready to be memcpy-ed into. Does not adjust the
  // referenced StringBuffer's usedCount.
  @_versioned
  internal
  mutating func _formNative(forAppending other: _StringGuts) {
    _ensureUniqueNative(
      minimumCapacity: self.count + other.count,
      minimumByteWidth: other.byteWidth)
  }

  // Copy in our elements to the new storage
  //
  @_versioned
  internal
  func _copy(
    into dest: UnsafeMutableRawPointer,
    capacityEnd: UnsafeMutableRawPointer,
    accomodatingElementWidth width: Int
  ) {
    _sanityCheck(byteWidth == 1 || byteWidth == 2)
    _sanityCheck(capacityEnd >= dest + (self.count &<< (byteWidth &- 1)))

    // TODO: Eventual small form check on other. We could even do this now for
    // the tagged cocoa strings.
    let unmanagedSelfOpt = self._unmanagedContiguous
    if _fastPath(unmanagedSelfOpt != nil) {
      unmanagedSelfOpt._unsafelyUnwrappedUnchecked._copy(
        into: dest, capacityEnd: capacityEnd, accomodatingElementWidth: width)
      _fixLifetime(self)
      return
    }

    _sanityCheck(width == 2)
    let opaque = getOpaque()
    _cocoaStringReadAll(
      opaque.object,
      dest.assumingMemoryBound(to: UTF16.CodeUnit.self))
  }

  // NOTE: Follow up calls to this with _fixLifetime(self) after the last use of
  // the return value.
  @_versioned
  internal
  var _unmanagedContiguous: UnsafeString? {
    if _isUnsafe {
      return _unsafeString
    }
    if _isNative {
      return _asNative.unsafe
    }
    if _isNonTaggedCocoa {
      return _cocoa._unsafelyUnwrappedUnchecked.unsafe
    }
    return nil
  }

  // TODO(perf): guarantee this is a simple bitmask operation, and probably
  // make inlineable or inline allways
  @_versioned
  internal
  var _isOpaque: Bool {
      @inline(never) // TODO(perf): to inspect code quality
      get { return _unmanagedContiguous == nil }
  }

  @_versioned
  internal
  func getOpaque() -> OpaqueCocoaString {
    _sanityCheck(_isOpaque)
    if let cocoa = self._cocoa {
      return OpaqueCocoaString(cocoa.owner)
    } else {
      return OpaqueCocoaString(self._smallCocoa!.taggedObject)
    }
  }

  @_versioned
  internal
  var _cocoaObject: AnyObject? {
    if let cocoa = self._cocoa {
      return cocoa.owner
    }
    if let smallCocoa = self._smallCocoa {
      return smallCocoa.taggedObject
    }
    return nil
  }
}

//
// String API
//
// TODO: Probably better to host top level API directly on the String type itself
extension _StringGuts {
  @_versioned
  internal
  var startIndex: Int { return 0 }

  @_versioned
  internal
  var endIndex: Int { return count }

  @_versioned
  internal
  var count: Int {
    let contigOpt = self._unmanagedContiguous
    if _fastPath(contigOpt != nil) {
      return contigOpt._unsafelyUnwrappedUnchecked.count
    }
    return self.getOpaque().count
  }

  @_versioned
  internal
  var capacity: Int {
    if _fastPath(_isNative) {
      return self._asNative.capacity
    }
    return 0
  }

  @_versioned
  internal
  var _isEmpty: Bool {
    // TODO: Could just have an empty bit pattern
    return count == 0
  }

  public // TODO(StringGuts): for testing
  mutating func reserveCapacity(_ n: Int) {
    if _fastPath(isUniqueNative() && self.capacity >= n) {
      return
    }
    let newBuffer = _copyToStringBuffer(
      capacity: n,
      byteWidth: self.byteWidth)
    self = _StringGuts(newBuffer)
  }

  // @_inlineable // TODO: internal-inlineable, if that's possible
  // TODO: @_versioned
  // TODO: internal
  public // TODO(StringGuts): for testing only
  mutating func append(_ other: _StringGuts) {
    guard !other._isEmpty else { return }

    // TODO: Eventual small form check on self and other. We could even do this
    // now for the tagged cocoa strings.

    self._formNative(forAppending: other)
    self._asNative._appendInPlace(other)
    _invariantCheck()
  }
}

@_versioned // FIXME(sil-serialize-all)
internal var _emptyStringStorage: UInt32 = 0

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal var _emptyStringBase: UnsafeRawPointer {
  return UnsafeRawPointer(Builtin.addressof(&_emptyStringStorage))
}

//
// TODO: Consider adding to, or replacing, _SwiftStringView instead
//
extension String {
  // NOTE: Follow up calls to this with _fixLifetime(self) after the last use of
  // the return value.
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal var _unmanagedContiguous : UnsafeString? {
    return self._guts._unmanagedContiguous
  }
}
extension Substring {
  // NOTE: Follow up calls to this with _fixLifetime(self) after the last use of
  // the return value.
  @_inlineable
  @_versioned
  internal
  var _unmanagedContiguous: UnsafeString? {
    return self._wholeString._unmanagedContiguous?[
      self.startIndex.encodedOffset..<self.endIndex.encodedOffset]
  }

  // // A potentially-unmanaged ephemeral string for very temporary purposes.
  // // Unlike _ephemeralString, caller must ensure lifetime.
  // //
  // // NOTE: Follow up calls to this with _fixLifetime(self) after the last use of
  // // the return value.
  // @_versioned
  // internal
  // var _unmanagedTransientString: String {
  //   let contigOpt = self._unmanagedContiguous
  //   if _slowPath(contigOpt == nil) {
  //     return self._ephemeralString
  //   }
  //   return String(_StringGuts(contigOpt._unsafelyUnwrappedUnchecked))
  // }
}

extension StringProtocol {
  @_inlineable
  @_versioned
  internal
  var _unmanagedContiguous: UnsafeString? {
    @inline(__always)
    get {
      // FIXME: Is this really the best way to unify this?
      if self is String {
        return (
          self as? String
        )._unsafelyUnwrappedUnchecked._unmanagedContiguous
      }
      return (
        self as? Substring
      )._unsafelyUnwrappedUnchecked._unmanagedContiguous
    }
  }
}

//
// String API
//
// TODO: Reorganize to place these in right files. For now, useful to see diff
// here.
//

extension StringProtocol {
  @_inlineable // FIXME(sil-serialize-all)
  public static func ==<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
#if _runtime(_ObjC)
    let lhsContigOpt = lhs._unmanagedContiguous
    let rhsContigOpt = rhs._unmanagedContiguous
    if _slowPath(lhsContigOpt == nil || rhsContigOpt == nil) {
      return lhs._ephemeralString._compareString(rhs._ephemeralString) == 0
    }
    let result = lhsContigOpt._unsafelyUnwrappedUnchecked.equal(
      to: rhsContigOpt._unsafelyUnwrappedUnchecked)
    _fixLifetime(lhs)
    _fixLifetime(rhs)
    return result
#else
    return lhs._ephemeralString._compareString(rhs._ephemeralString) == 0
#endif
  }

  @_inlineable // FIXME(sil-serialize-all)
  public static func !=<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
    return !(lhs == rhs)
  }
}
extension String : Equatable {
  // FIXME: Why do I need this? If I drop it, I get "ambiguous use of operator"
  @_inlineable // FIXME(sil-serialize-all)
  public static func ==(lhs: String, rhs: String) -> Bool {
#if _runtime(_ObjC)
    let lhsContigOpt = lhs._unmanagedContiguous
    let rhsContigOpt = rhs._unmanagedContiguous
    if _slowPath(lhsContigOpt == nil || rhsContigOpt == nil) {
      return lhs._ephemeralString._compareString(rhs._ephemeralString) == 0
    }
    let result = lhsContigOpt._unsafelyUnwrappedUnchecked.equal(
      to: rhsContigOpt._unsafelyUnwrappedUnchecked)
    _fixLifetime(lhs)
    _fixLifetime(rhs)
    return result
#else
    return lhs._ephemeralString._compareString(rhs._ephemeralString) == 0
#endif
  }

}
extension Substring : Equatable {}

extension UnsafeString {
  @inline(__always)
  @_versioned
  internal func _sanityCheckIdentical(to other: UnsafeString) {
    _sanityCheck(self.baseAddress == other.baseAddress)
    _sanityCheck(self.count == other.count)
    // Empty string storage can be presented as any element width
    _sanityCheck(self.count == 0 || self.isSingleByte == other.isSingleByte)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  internal func equal(to other: UnsafeString) -> Bool {
    if self.baseAddress == other.baseAddress && self.count == other.count {
      // Binary equivalence is always sufficient for canonical equivalence
      _sanityCheckIdentical(to: other)
      return true
    }
    if self.isASCII && other.isASCII {
      // ASCII equivalence requires same size, same bits.
      return self.count == other.count
        && _swift_stdlib_memcmp(
          self.baseAddress, other.baseAddress, self.count
        ) == (0 as CInt)
    }
    return self._compareDeterministicUnicodeCollation(other) == 0
  }

  @inline(never) // Hide the CF/ICU dependency
  @_versioned
  internal func _compareDeterministicUnicodeCollation(
    _ other: UnsafeString
  ) -> Int {
    let lhsStr = _NSContiguousString(_StringGuts(self))
    let rhsStr = _NSContiguousString(_StringGuts(other))
    let res = lhsStr._unsafeWithNotEscapedSelfPointerPair(rhsStr) {
      return Int(
          _stdlib_compareNSStringDeterministicUnicodeCollationPointer($0, $1))
    }
    return res
  }
}


