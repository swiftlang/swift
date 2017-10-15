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

  internal
  init(_ object: _BuiltinBridgeObject, _ otherBits: UInt) {
    self._storage.0 = object
    self._storage.1 = otherBits
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

  var baseAddress: UnsafeRawPointer
  var legacyCountAndFlags: UInt

  var isSingleByte: Bool {
    fatalError("unimplemented")
  }
  var count: Int {
    fatalError("unimplemented")
  }

  init(_ baseAddress: UnsafeRawPointer, _ legacyCountAndFlags: UInt) {
    self.baseAddress = baseAddress
    self.legacyCountAndFlags = legacyCountAndFlags
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
    return _StringBuffer(_StringBuffer._Storage(_nativeObject: owner))
  }

  var unsafe: UnsafeString {
    // return UnsafeString(stringBuffer._elementPointer)
    fatalError("unimplemented")
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

//
// Discriminators
//

// FIXME: Define this properly per ABI. For now, it's taking advantage of tag
// bits being highest (and possibly lowest but shifted away)
/*fileprivate*/ internal var _tagBig: UInt { return _objCTaggedPointerBits }
/*fileprivate*/ internal var _smallBit: UInt { return _tagBig >> 1 }
/*fileprivate*/ internal var _smallCocoaBit: UInt { return _smallBit >> 1 }

//
// Masks
//
/*fileprivate*/ internal var _smallCocoaStringMask: UInt {
  return _tagBig | _smallBit | _smallCocoaBit
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
      _sanityCheck(_isTaggedObject(_object))
      if _bitPattern(_object) & _smallBit == 0 { return .unsafe }

      // Must be smol
      if _bitPattern(_object) == _smallCocoaStringMask { return .smallCocoa }

      return .error
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
    self.init(_bridgeObject(fromNativeObject: s.owner), 0)
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
    return UnsafeString(
      UnsafeRawPointer(
        bitPattern: _bridgeObject(toTagPayload: _object
      ))._unsafelyUnwrappedUnchecked,
      UInt(_otherBits))
  }
  init(_ s: UnsafeString) {
    self.init(_bridgeObject(
      taggingPayload: UInt(bitPattern: s.baseAddress)), s.legacyCountAndFlags)
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
    self.init(_bridgeObject(fromTagged: _smallCocoaStringMask), s.taggedPointer)
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
internal func internalDumpHexImpl(_ x: UInt) {
  _swift_stdlib_print_hex(x)
}
internal func internalDumpHex(_ x: UInt) {
  internalDumpHexImpl(x)
}
internal func internalDumpHex(_ x: AnyObject) {
  internalDumpHexImpl(Builtin.reinterpretCast(x))
}
internal func internalDumpHex(_ x: UnsafeMutableRawPointer?) {
  internalDumpHexImpl(Builtin.reinterpretCast(x))
}
internal func internalDumpHex(_ x: Bool) {
  internalDumpHexImpl(x ? 1 : 0)
}


extension _StringGuts {
  // func getUnsafeContents() -> UnsafeString {
  //   if let unsafeString = self.unsafeString {
  //     return unsafeString
  //   }
  //   if let native = self._native {

  //   }
  // }
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
        _countAndFlags: unsafeString.legacyCountAndFlags,
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
    // TODO: Try to pack small strings
    guard true else {
      self.init(FatalErrorString())
      return
    }

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
      let immortal = UnsafeString(baseAddress, legacyCore._countAndFlags)
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
