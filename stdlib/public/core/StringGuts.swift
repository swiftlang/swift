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

public // FIXME
struct _StringGuts {
  // TODO 32-bit: Insert padding between fields
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

// When managed, or an unsafe string, the bit used to discriminate Cocoa-buffer-
// backed Strings.
//
// TODO: Remove when weaned off of _LegacyStringCore. Is should be effectively
// redundant with BridgeObject's isObjC bit, and unsafe strings shouldn't care
// where they came from.
//
/*fileprivate*/ internal var _hasCocoaBufferBit: UInt {
  return _twoByteCodeUnitBit >> 1
}

//
// Flags
//
// TODO: It would be more clear to wrap up BridgeObject in a
// "StringObject"-like struct, and provide this funcitonality on that.
//
extension _StringGuts {
  /*private*/ internal var _flagsMask: UInt {
    return _twoByteCodeUnitBit | _hasCocoaBufferBit
  }

  var isSingleByte: Bool {
    switch classification {
    case .native, .unsafe:
      return (_objectBitPattern & _twoByteCodeUnitBit) == 0
    case .nonTaggedCocoa, .smallCocoa, .error:
      return false
    }   
  }

  var byteWidth: Int {
    return isSingleByte ? 1 : 2
  }

  // NOTE: Currently, single byte representation is synonymous with ASCII. This
  // may change in the future in preference of either UTF8 or Latin1.
  var isASCII: Bool {
    return isSingleByte
  }

  // TODO: remove
  var hasCocoaBuffer: Bool {
    switch classification {
    case .native, .unsafe:
      return (_objectBitPattern & _hasCocoaBufferBit) != 0
    case .nonTaggedCocoa:
      return true
    case .error, .smallCocoa:
      return false
    }   
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
    hasCocoaBuffer: Bool,
    otherBits: UInt
  ) {
    self.init(object, otherBits)
    if !isSingleByte {
      self._objectBitPattern |= _twoByteCodeUnitBit
    }
    if hasCocoaBuffer {
      self._objectBitPattern |= _hasCocoaBufferBit
    }

    _sanityCheck(_bitPattern(self._unflaggedObject) == _bitPattern(object))
    _invariantCheck()
  }

  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    if let native = self._native {
      _sanityCheck(self.isSingleByte == native.isSingleByte)
      _sanityCheck(!self.hasCocoaBuffer)
      _sanityCheck(self.count == native.count)
      _sanityCheck(self.capacity == native.capacity)
      _sanityCheck(self.count >= 0)
      _sanityCheck(self.capacity >= self.count)
    } else if let unsafe = self._unsafeString {
      _sanityCheck(self.isSingleByte == unsafe.isSingleByte)
      _sanityCheck(self.hasCocoaBuffer == unsafe.hasCocoaBuffer)
      _sanityCheck(self.count == unsafe.count)
      _sanityCheck(self.count >= 0)
      _sanityCheck(self.capacity == 0)
    } else if let cocoa = self._cocoa {
      _sanityCheck(!self.isSingleByte)
      _sanityCheck(self.hasCocoaBuffer)
      _sanityCheck(self.count >= 0)
      _sanityCheck(self.capacity >= self.count)
    } else if let _ = self._smallCocoa {
      _sanityCheck(!self.isSingleByte)
      _sanityCheck(!self.hasCocoaBuffer) // FIXME: Is this right?
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

  var baseAddress: UnsafeMutableRawPointer
  var count: Int

  var isSingleByte: Bool

  // TODO: Is this actually important to track? Can we drop it when we're no
  // longer using _LegacyStringCore?
  var hasCocoaBuffer: Bool

  var sizeInBytes: Int {
    return count * byteWidth
  }

  var byteWidth: Int {
    return isSingleByte ? 1 : 2
  }

  init(
    baseAddress: UnsafeMutableRawPointer,
    count: Int,
    isSingleByte: Bool,
    hasCocoaBuffer: Bool
  ) {
    self.baseAddress = baseAddress
    self.count = count
    self.isSingleByte = isSingleByte
    self.hasCocoaBuffer = hasCocoaBuffer
  }

  var unsafeUTF16String: UnsafeBufferPointer<UInt16>? {
    guard !isSingleByte else { return nil }
    return UnsafeBufferPointer(
      start: baseAddress.assumingMemoryBound(to: UInt16.self),
      count: count)
  }
  var unsafeOneByteString: UnsafeBufferPointer<UInt8>? {
    guard isSingleByte else { return nil }
    return UnsafeBufferPointer(
      start: baseAddress.assumingMemoryBound(to: UInt8.self),
      count: count)
  }

  @_versioned
  internal
  func _copy(
    into dest: UnsafeMutableRawPointer,
    capacityEnd: UnsafeMutableRawPointer,
    accomodatingElementWidth width: Int
  ) {
    _sanityCheck(width == 1 || width == 2)
    let elementShift = width &- 1
    _precondition(capacityEnd >= dest + self.count &<< elementShift)
 
    if _fastPath(self.byteWidth == width) {
      _memcpy(
        dest: dest,
        src: self.baseAddress,
        size: UInt(self.sizeInBytes))
    } else if self.byteWidth == 1 && width == 2 {
      var dest = dest.assumingMemoryBound(to: UTF16.CodeUnit.self)
      for byte in self.unsafeOneByteString! {
        dest.pointee = UTF16.CodeUnit(byte)
        dest += 1
      }
    } else {
      _sanityCheck(self.byteWidth == 2 && width == 1)
      var dest = dest.assumingMemoryBound(to: UInt8.self)
      for unit in self.unsafeUTF16String! {
        _precondition(unit & ~0x7F == 0) // ASCII only
        dest.pointee = UInt8(truncatingIfNeeded: unit)
        dest += 1
      }
    }
  }
}

/*fileprivate*/ internal struct NativeString {
  // TODO: Use the extra 72 bits.
  //
  // StringGuts when representing a native Swift string should have an extra 72
  // bits *at least* to store whatever is most profitable (e.g. flags and a
  // count). x86_64 has at least 72 bits available due to:
  //
  // * 8 bits from BridgeObject: 11 spare bits from native references - 2
  //   (conservatively) for tagging - 1 to designate native vs objc reference.
  // * 64 bits from the second word

  var owner: _BuiltinNativeObject

  var stringBuffer: _StringBuffer {
      // TODO: Does this in practice incur overhead? Should we cast it?
      return _StringBuffer(_StringBuffer._Storage(_nativeObject: owner))
  }

  var unsafe: UnsafeString {
    return UnsafeString(
      baseAddress: self.baseAddress,
      count: self.count,
      isSingleByte: self.isSingleByte,
      hasCocoaBuffer: false)
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

  init(_ native: _BuiltinNativeObject) {
    self.owner = native
  }

  init(_ buffer: _StringBuffer) {
    self.init(buffer._nativeObject)
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

  init(_ owner: AnyObject) {
    self.owner = owner
  }
}

internal struct OpaqueCocoaString {
  let object: AnyObject
  let count: Int

  init(_ object: AnyObject) {
    self.object = object
    self.count = _stdlib_binary_CFStringGetLength(object)
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
      if _isNativePointer(_object) { return .native }
      if _isNonTaggedObjCPointer(_object) { return .nonTaggedCocoa }

      // Must be tagged
      _sanityCheck(_isTagged)
      if _objectBitPattern & _smallBit == 0 { return .unsafe }

      return .smallCocoa
    }
  }
}

extension _StringGuts {
  //
  // Native Swift Strings
  //
  /*fileprivate*/ internal // TODO: private in Swift 4
  var _isNative: Bool { return classification == .native }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _native: NativeString? {
    guard _isNative else { return nil }
    let nativeObject = _nativeObject(fromBridge: _object)
    return NativeString(nativeObject)
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  init(_ s: NativeString) {
    self.init(
      _unflagged: _bridgeObject(fromNativeObject: s.owner),
      isSingleByte: s.isSingleByte,
      hasCocoaBuffer: false,
      otherBits: 0)
  }

  //
  // Cocoa (non-tagged) Strings
  //
  /*fileprivate*/ internal // TODO: private in Swift 4
  var _isCocoa: Bool { return classification == .nonTaggedCocoa }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _cocoa: NonTaggedCocoaString? {
    guard _isCocoa else { return nil }
    return NonTaggedCocoaString(_bridgeObject(toNonTaggedObjC: _object))
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  init(_ s: NonTaggedCocoaString) {
    _sanityCheck(!_isObjCTaggedPointer(s.owner))
    self.init(_bridgeObject(fromNonTaggedObjC: s.owner), 0)
  }

  //
  // Unsafe Strings
  //
  var _isUnsafe: Bool { return classification == .unsafe }

  var _unsafeString: UnsafeString? {
    guard _isUnsafe else { return nil }

    // Unflag it, untag it, and go
    let pointer = UnsafeMutableRawPointer(
      bitPattern: self._untaggedUnflaggedBitPattern
    )._unsafelyUnwrappedUnchecked
    return UnsafeString(
      baseAddress: pointer,
      count: Int(self._otherBits),
      isSingleByte: self.isSingleByte,
      hasCocoaBuffer: self.hasCocoaBuffer)
  }

  init(_ s: UnsafeString) {
    // Tag it, flag it, and go
    let object = _bridgeObject(taggingPayload: UInt(bitPattern: s.baseAddress))
    self.init(
      _unflagged: object,
      isSingleByte: s.isSingleByte,
      hasCocoaBuffer: s.hasCocoaBuffer,
      otherBits: UInt(s.count))
  }

  //
  // Tagged Cocoa Strings
  //
  /*fileprivate*/ internal // TODO: private in Swift 4
  var _isSmallCocoa: Bool { return classification == .smallCocoa }

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
  /*fileprivate*/ internal // TODO: private in Swift 4
  var _isError: Bool { return classification == .error }

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
internal func internalDumpHex(
  _ x: UnsafeMutableRawPointer?,
  newline: Bool = true
) {
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
  internal var _isNativeSelfSlice: Bool {
    @inline(__always) get {
      guard let native = self.nativeBuffer else {
        return false
      }
      return self._baseAddress != native.start || self.count != native.usedCount
    }
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
        hasCocoaBuffer: unsafeString.hasCocoaBuffer,
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
    if let error = self._error {
      fatalError("I AM ERROR")
    }

    fatalError("TODO: unreachable")
  }

  @_versioned
  init(_ legacyCore: _LegacyStringCore) {
    guard !legacyCore.hasCocoaBuffer else {
      _sanityCheck(legacyCore._owner != nil,
        "how? is this yet another case we don't know about?")
      let owner = legacyCore._owner._unsafelyUnwrappedUnchecked

      // NOTE: Sometimes a _LegacyStringCore is a self-slice of a cocoa string
      // without having properly sliced the backing cocoa string itself. Detect
      // that situation in retrospect and form a proper _NSContiguousString to
      // work around this.
      let referenceCore = makeCocoaLegacyStringCore(_cocoaString: owner)
      guard referenceCore.count == legacyCore.count &&
        referenceCore._baseAddress == legacyCore._baseAddress
      else {
        Stats.numCocoaSelfSlice += 1
        let cocoa = NonTaggedCocoaString(_NSContiguousString(legacyCore))
        self.init(cocoa)
        return
      }

      if _isObjCTaggedPointer(owner) {
        self.init(SmallCocoaString(owner))
      } else {
        self.init(NonTaggedCocoaString(owner))
      }
      return
    }

    _sanityCheck(legacyCore._baseAddress != nil,
      "Non-cocoa, non-contiguous legacy String?")
    let baseAddress = legacyCore._baseAddress._unsafelyUnwrappedUnchecked

    guard let owner = legacyCore._owner else {
      // Immortal String
      _sanityCheck((0...2).contains(legacyCore.elementWidth))
      let immortal = UnsafeString(
        baseAddress: baseAddress,
        count: legacyCore.count,
        isSingleByte: legacyCore.elementWidth == 1,
        hasCocoaBuffer: legacyCore.hasCocoaBuffer)
      self.init(immortal)
      return
    }

    _sanityCheck(legacyCore.nativeBuffer != nil,
      "Native string not native-buffer backed?")

    // Check for a self-slice, in which cast we'll work around that temporarily
    // by creating a Cocoa string.
    //
    // TODO: Forbid this. We need to audit all uses of ephemeralString and
    // switch everything possible over to UnsafeString. This creates a brand new
    // object just for the slicing, which while preferable to copying a large
    // string, is very expensive. Besides ephemeralString, many of the views
    // still slice and dice _LegacyStringCore, need to audit those too.
    //
    guard !legacyCore._isNativeSelfSlice else {
      Stats.numNativeSelfSlice += 1
      let cocoa = NonTaggedCocoaString(_NSContiguousString(legacyCore))
      self.init(cocoa)
      return
    }

    let nativeString = NativeString(owner)
    self.init(nativeString)
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
      internalDumpHex(_nativeObject(toNative: native.owner), newline: false)
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
    } else if let smallCocoa = self._smallCocoa {
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
    if hasCocoaBuffer {
      print(" hasCocoaBuffer", terminator: "")
    }
    print(">")
  }
}

// TODO: We probably want to overhaul string storage, ala the "string-recore"
// branch. In addition to efficiency, such an overhaul can also guarantee nul-
// termination for all native strings. For now, this lets us bootstrap
// quicker.

//
// String API helpers
//
extension _StringGuts {
  // Return a _StringBuffer with the same contents as this string. Uses the
  // existing buffer if possible; otherwise copies the string into a new buffer.
  @_versioned
  internal
  func _extractStringBuffer() -> _StringBuffer {
    if let native = self._native {
      return native.stringBuffer
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
  
  @_versioned
  internal
  mutating func isUniqueNative() -> Bool {
    return _isNative && _isUnique(&_object)
  }

  @_versioned
  internal
  mutating func _ensureUniqueNative(
    minimumCapacity: Int,
    minimumByteWidth: Int
  ) {
    _sanityCheck(minimumByteWidth == 1 || minimumByteWidth == 2)
    let oldCapacity: Int
    let oldCount: Int
    let newWidth = Swift.max(self.byteWidth, minimumByteWidth)
    if _fastPath(self.byteWidth == newWidth && isUniqueNative()) {
      // TODO: width extension can be done in place if there's capacity
      var nativeBuffer = self._native._unsafelyUnwrappedUnchecked.stringBuffer
      oldCapacity = nativeBuffer.capacity
      oldCount = nativeBuffer.usedCount
      if _fastPath(oldCapacity >= minimumCapacity) {
        return
      }
    } else {
      oldCapacity = 0
      oldCount = self.count
    }

    let newCapacity = Swift.max(
      _growArrayCapacity(oldCapacity),
      minimumCapacity)
    let newBuffer = _copyToStringBuffer(
      capacity: newCapacity,
      byteWidth: newWidth)
    self = _StringGuts(NativeString(newBuffer))
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
    let unmangedSelfOpt = self._unmangedContiguous
    if _fastPath(unmangedSelfOpt != nil) {
      unmangedSelfOpt._unsafelyUnwrappedUnchecked._copy(
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
  var _unmangedContiguous: UnsafeString? {
    if let unsafe = self._unsafeString {
      return unsafe
    }
    if let native = self._native {
      return native.unsafe
    }
    if let cocoa = self._cocoa {
      // FIXME: Most of these are contiguous!
      return nil
    }
    return nil
  }

  // TODO(perf): guarantee this is a simple bitmask operation, and probably
  // make inlineable or inline allways
  @_versioned
  internal
  var _isOpaque: Bool {
      @inline(never) // TODO(perf): to inspect code quality
      get { return _unmangedContiguous == nil }
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
}

//
// String API
//
// TODO: Probably better to host top level API directly on the String type itself
extension _StringGuts {
  @_versioned
  internal
  var count: Int {
    let contigOpt = self._unmangedContiguous
    if _fastPath(contigOpt != nil) {
      return contigOpt._unsafelyUnwrappedUnchecked.count
    }
    return self.getOpaque().count
  }

  @_versioned
  internal
  var capacity: Int {
    if _fastPath(_isNative) {
      return self._native._unsafelyUnwrappedUnchecked.capacity
    }
    return 0
  }

  @_versioned
  internal
  var _isEmpty: Bool {
    // TODO: Could just have an empty bit pattern
    return count == 0
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
    self._native._unsafelyUnwrappedUnchecked._appendInPlace(other)
    _invariantCheck()
  }
}


