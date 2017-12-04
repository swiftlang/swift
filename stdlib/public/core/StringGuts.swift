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

  @_inlineable
  public
  var _object: _BuiltinBridgeObject {
    @inline(__always) get { return _storage.0 }
    @inline(__always) set { _storage.0 = newValue }
  }
  @_inlineable
  public
  var _otherBits: UInt {
    @inline(__always) get { return _storage.1 }
    @inline(__always) set { _storage.1 = newValue }
  }
  @_inlineable
  public
  var _objectBitPattern: UInt {
    @inline(__always) get { return _bitPattern(_object) }
    @inline(__always) set { _object = Builtin.reinterpretCast(newValue) }
  }

  @_inlineable
  @inline(__always)
  public
  init(_ object: _BuiltinBridgeObject, _ otherBits: UInt) {
    self._storage.0 = object
    self._storage.1 = otherBits
  }
}

// //
// // Work around ARC
// //
// @inline(never)
// @_versioned
// internal
// func _unmanagedContiguousImpl(
//   bridgeObjectBits: UInt, otherBits: UInt
// ) -> UnsafeString? {
//   // TODO: This is super ugly and redundant. Organize and tidy up alongside
//   // Builtin.swift and _StringGuts local vars.

//   let isSingleByte = bridgeObjectBits & _twoByteCodeUnitBit == 0
//   let unflaggedBObject = (bridgeObjectBits & ~_twoByteCodeUnitBit)

//   // is already an unsafe string
//   if (unflaggedBObject & _objCTaggedPointerBits != 0) && (unflaggedBObject & _smallBit == 0) {
//     let ptr = UnsafeMutableRawPointer(
//       bitPattern: _bridgeObject(toTagPayload: Builtin.reinterpretCast(unflaggedBObject))
//     )._unsafelyUnwrappedUnchecked
//     let count = Int(bitPattern: otherBits)
//     _sanityCheck(count >= 0)
//     return UnsafeString(
//       baseAddress: ptr, count: count, isSingleByte: isSingleByte)
//   }

//   // is native
//   if unflaggedBObject & (_objCTaggedPointerBits | _objectPointerIsObjCBit) == 0 {
//     let ptr = _StringBuffer(_StringBuffer._Storage(
//       _nativeObject: _nativeObject(
//         fromBridge: Builtin.reinterpretCast(unflaggedBObject)))
//     ).start
//     let count = Int(bitPattern: otherBits)
//     _sanityCheck(count >= 0)
//     return UnsafeString(
//       baseAddress: ptr, count: count, isSingleByte: isSingleByte)
//   }

//   // non tagged cocoa
//   if _isNonTaggedObjCPointer(Builtin.reinterpretCast(unflaggedBObject)) {
//     if _slowPath(otherBits == 0) {
//       return nil
//     }
//     let count = _stdlib_binary_CFStringGetLength(
//       _bridgeObject(toNonTaggedObjC: Builtin.reinterpretCast(unflaggedBObject)))
//     _sanityCheck(count >= 0)
//     let ptr = UnsafeMutableRawPointer(
//       bitPattern: otherBits
//     )._unsafelyUnwrappedUnchecked
//     return UnsafeString(
//       baseAddress: ptr,
//       count: count,
//       isSingleByte: isSingleByte
//     )
//   }

//   return nil
// }

//
// Discriminators
//

// FIXME: Define this properly per ABI. For now, it's taking advantage of tag
// bits being highest (and possibly lowest but shifted away). Furthermore, these
// won't work out-of-the-box for 32bit platforms.

// The bit used to discriminate value or managed.
/*fileprivate*/
@_versioned
@_inlineable
internal var _tagBit: UInt {
  @inline(__always)
  get {
    return _objCTaggedPointerBits
  }
}

// When a value, the bit that discriminates a small string from an unsafe string
//
// NOTE: Until we wean ourselves off of _StringCore, the only "small" strings we
// can store are tagged NSStrings. For now, this is synonymous with a tagged
// NSString but this will be expanded in the future to store other forms.
//
/*fileprivate*/
@_versioned
@_inlineable
internal var _smallBit: UInt {
  @inline(__always)
  get { return _tagBit >> 1 }
}

// When managed, or an unsafe string, the bit used to discriminate two-byte or
// one-byte code units.
/*fileprivate*/
@_versioned
@_inlineable
internal var _twoByteCodeUnitBit: UInt {
  @inline(__always)
  get { return _smallBit >> 1 }
}

//
// Flags
//
// TODO: It would be more clear to wrap up BridgeObject in a
// "StringObject"-like struct, and provide this functionality on that.
//
extension _StringGuts {
  @_versioned
  @_inlineable
  /*private*/ internal var _flagsMask: UInt {
    @inline(__always)
    get { return _twoByteCodeUnitBit }
  }

  @_inlineable
  public // @testable
  var isSingleByte: Bool {
    @inline(__always) get {
      if _slowPath(_isSmallCocoa) { return false }
      _sanityCheck(_isNative || _isUnmanaged || _isNonTaggedCocoa)
      return (_objectBitPattern & _twoByteCodeUnitBit) == 0
    }
  }

  @_inlineable
  public // @testable
  var byteWidth: Int {
    @inline(__always) get { return isSingleByte ? 1 : 2 }
  }

  @_inlineable
  @_versioned
  var bitWidth: Int {
    @inline(__always) get { return byteWidth &<< 3 }
  }

  // NOTE: Currently, single byte representation is synonymous with ASCII. This
  // may change in the future in preference of either UTF8 or Latin1.
  @_inlineable
  public // @testable
  var isASCII: Bool {
    @inline(__always) get { return isSingleByte }
  }

  @_inlineable
  @_versioned
  internal
  var _unflaggedObject: _BuiltinBridgeObject {
    @inline(__always)
    get {
      _sanityCheck(!_isSmallCocoa, "TODO: drop small cocoa")
      return Builtin.reinterpretCast(_objectBitPattern & ~_flagsMask)
    }
  }

  @_versioned
  @_inlineable
  var _isTagged: Bool {
    @inline(__always)
    get { return _isTaggedObject(_object) }
  }

  @_versioned
  @_inlineable
  var _untaggedUnflaggedBitPattern: UInt {
    _sanityCheck(_isTagged)
    return _bridgeObject(toTagPayload: _unflaggedObject)
  }

  @_inlineable
  public // @testable
  var _owner: AnyObject? { // For testing only
    if _isNative {
      return _bridgeObject(toNative: _object)
    } else if _isNonTaggedCocoa {
      return _bridgeObject(toNonTaggedObjC: _object)
    }
    return nil
  }

  @_inlineable
  @_versioned
  @inline(__always)
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
}

extension _StringGuts {
  /// Create the guts of an empty string.
  @_inlineable
  public init() {
    self.init(
      _UnmanagedString(
        start: _emptyStringBase.assumingMemoryBound(to: UInt8.self),
        count: 0))
  }

  @_inlineable
  @_versioned
  internal
  var _isEmptyLiteral: Bool {
    return _isUnmanaged && _unmanagedRawStart == _emptyStringBase
  }
}

@_versioned // FIXME(sil-serialize-all)
internal var _emptyStringStorage: UInt32 = 0

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal var _emptyStringBase: UnsafeRawPointer {
  return UnsafeRawPointer(Builtin.addressof(&_emptyStringStorage))
}

extension _StringGuts {
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    if _isNative {
      _sanityCheck(UInt(_nativeRawStorage.count) == self._otherBits)
    } else if _isUnmanaged {
      _sanityCheck(_untaggedUnflaggedBitPattern != 0)
    } else if _isSmallCocoa {
      _sanityCheck(_otherBits != 0)
    } else if _isNonTaggedCocoa {
    } else {
      fatalError("Unimplemented string form")
    }
#endif
  }
}

extension _StringGuts {
  //
  // Native Swift Strings
  //
  // Bit layout (x64):
  //
  // _object:
  // +---+---+---+-------+--------------------------------------------+----+---+
  // + t | c | w | uuuuu | storage reference (53 bits)                | uu | t |
  // +---+---+---+-------+--------------------------------------------+----+---+
  //
  // _otherBits:
  // +----------+--------------------------------------------------------------+
  // | uuuuuuuu | count (56 bits)                                              |
  // +----------+--------------------------------------------------------------+
  // where t: tag bits (must be 0)
  //       c: Cocoa indicator bit (must be 0)
  //       w: width indicator bit (0: ASCII, 1: UTF-16)
  //       u: unused bits
  //
  // TODO: Find a use for the 15 unused bits.

  @_inlineable
  public // TODO(StringGuts): for testing only
  var _isNative: Bool {
    return _isNativePointer(_object)
  }

  @_inlineable
  @_versioned
  @inline(__always)
  internal
  func _nativeStorage<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _SwiftStringStorage<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_isNative)
    _sanityCheck(CodeUnit.bitWidth == self.bitWidth)
    return _bridgeObject(toNative: _object) as! _SwiftStringStorage<CodeUnit>
  }

  @_inlineable
  public // @testable
  var _nativeRawStorage: _SwiftRawStringStorage {
    @inline(__always) get {
      _sanityCheck(_isNative)
      return _bridgeObject(toNative: _object) as! _SwiftRawStringStorage
    }
  }

  @_inlineable
  @_versioned
  internal
  var _nativeCount: Int {
    @inline(__always) get {
      _sanityCheck(_isNative)
      return Int(bitPattern: _otherBits)
    }
    @inline(__always) set {
      _sanityCheck(_isNative)
      _otherBits = UInt(bitPattern: newValue)
    }
  }

  @_versioned
  @inline(__always)
  internal
  init<CodeUnit>(_ storage: _SwiftStringStorage<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    self.init(
      _unflagged: _bridgeObject(fromNative: storage),
      isSingleByte: CodeUnit.bitWidth == 8,
      otherBits: UInt(bitPattern: storage.count))
    _sanityCheck(_isNative)
    _invariantCheck()
  }

  //
  // Cocoa (non-tagged) Strings
  //
  // Bit layout (x64):
  //
  // _object:
  // +---+---+---+-------+--------------------------------------------+----+---+
  // + t | c | w | uuuuu | Cocoa object reference (53 bits)           | uu | t |
  // +---+---+---+-------+--------------------------------------------+----+---+
  //
  // _otherBits:
  // +----------+--------------------------------------------------------------+
  // | uuuuuuuu | start address or zero (56 bits)                              |
  // +----------+--------------------------------------------------------------+
  // where t: tag bits (must be 0)
  //       c: Cocoa indicator bit (must be 1)
  //       w: width indicator bit (0: ASCII, 1: UTF-16)
  //       u: unusued
  //
  // TODO: Use the extra 15 bits
  //

  @_inlineable
  public // @testable
  var _isNonTaggedCocoa: Bool {
    @inline(__always) get {
      return _isNonTaggedObjCPointer(_object)
    }
  }

  @_inlineable
  public // @testable
  var _isContiguousCocoa: Bool {
    @inline(__always) get {
      return _isNonTaggedCocoa && _nonTaggedCocoaRawStart != nil
    }
  }

  @_inlineable
  public // @testable
  var _isNoncontiguousCocoa: Bool {
    @inline(__always) get {
      return _isNonTaggedCocoa && _nonTaggedCocoaRawStart == nil
    }
  }

  @_inlineable
  @_versioned
  var _nonTaggedCocoaObject: _CocoaString {
    @inline(__always) get {
      _sanityCheck(_isNonTaggedCocoa)
      return _bridgeObject(toNonTaggedObjC: _object)
    }
  }

  @_versioned
  var _nonTaggedCocoaCount: Int {
    @inline(never) // Hide CF dependency
    get {
      _sanityCheck(_isNonTaggedCocoa)
      return _stdlib_binary_CFStringGetLength(_nonTaggedCocoaObject)
    }
  }

  @_inlineable
  @_versioned
  var _nonTaggedCocoaRawStart: UnsafeRawPointer? {
    @inline(__always) get {
      _sanityCheck(_isNonTaggedCocoa)
      return UnsafeRawPointer(bitPattern: _otherBits)
    }
  }

  @_inlineable
  @_versioned
  func _asContiguousCocoa<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _UnmanagedString<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_isContiguousCocoa)
    _sanityCheck(CodeUnit.bitWidth == self.bitWidth)
    let start = _nonTaggedCocoaRawStart
      ._unsafelyUnwrappedUnchecked
      .assumingMemoryBound(to: CodeUnit.self)
    return _UnmanagedString(start: start, count: _nonTaggedCocoaCount)
  }

  @inline(never)
  @_versioned
  internal
  init(
    _nonTaggedCocoaObject s: _CocoaString,
    count: Int,
    isSingleByte: Bool,
    start: UnsafeRawPointer?
  ) {
    _sanityCheck(!_isObjCTaggedPointer(s))
    guard count > 0 else {
      self.init()
      return
    }
    self.init(
      _unflagged: _bridgeObject(fromNonTaggedObjC: s),
      isSingleByte: isSingleByte,
      otherBits: UInt(bitPattern: start))
    _sanityCheck(_isNonTaggedCocoa)
    _invariantCheck()
  }

  //
  // Unmanaged Strings
  //
  // Bit layout (x64):
  //
  // _object:
  // +---+---+---+------+--------------------------------------------------+---+
  // + t | s | w | uuuu | start address (56 bits)                          | t |
  // +---+---+---+------+--------------------------------------------------+---+
  //
  // _otherBits:
  // +----------+--------------------------------------------------------------+
  // | uuuuuuuu | count (56 bits)                                              |
  // +----------+--------------------------------------------------------------+
  // where t: tag bits (must be 1)
  //       s: Small string indicator bit (must be 0)
  //       w: width indicator bit (0: ASCII, 1: UTF-16)
  //       u: unused bits
  //
  // TODO: Use the extra 12 bits

  ///*fileprivate*/ internal // TODO: private in Swift 4
  @_inlineable
  public // TODO(StringGuts): for testing only
  var _isUnmanaged: Bool {
    @inline(__always) get {
      return _isTagged && _objectBitPattern & _smallBit == 0
    }
  }

  @_inlineable
  @_versioned
  internal var _unmanagedCount: Int {
    @inline(__always) get {
      _sanityCheck(_isUnmanaged)
      return Int(bitPattern: _otherBits)
    }
  }

  @_inlineable
  public // @testable
  var _unmanagedRawStart: UnsafeRawPointer {
    @inline(__always) get {
      _sanityCheck(_isUnmanaged)
      return UnsafeRawPointer(
        bitPattern: _untaggedUnflaggedBitPattern)._unsafelyUnwrappedUnchecked
    }
  }

  @_inlineable
  @_versioned
  @inline(__always)
  internal
  func _asUnmanaged<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _UnmanagedString<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_isUnmanaged)
    _sanityCheck(CodeUnit.bitWidth == self.bitWidth)
    let start = _unmanagedRawStart.assumingMemoryBound(to: CodeUnit.self)
    let count = _unmanagedCount
    _sanityCheck(count >= 0)
    return _UnmanagedString(start: start, count: count)
  }

  @_inlineable
  @_versioned
  init<CodeUnit>(_ s: _UnmanagedString<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    // Tag it, flag it, and go
    self.init(
      _unflagged: _bridgeObject(taggingPayload: UInt(bitPattern: s.start)),
      isSingleByte: CodeUnit.bitWidth == 8,
      otherBits: UInt(s.count))
    _sanityCheck(_isUnmanaged)
    _invariantCheck()
    _sanityCheck(_unmanagedRawStart == s.rawStart)
    _sanityCheck(_unmanagedCount == s.count)
  }

  //
  // Tagged Cocoa Strings
  //
  // TODO: Replace with native small strings holding 15 bytes of data
  //
  // Bit layout (x64):
  //
  // _object:
  // +---+---+---+---------------------------------------------------------+---+
  // + t | s | w | count (60 bits)                                         | t |
  // +---+---+---+---------------------------------------------------------+---+
  //
  // _otherBits:
  // +-------------------------------------------------------------------------+
  // | Cocoa tagged pointer (64 bits)                                          |
  // +-------------------------------------------------------------------------+
  // where t: tag bits (must be 1)
  //       s: Small string indicator bit (must be 1)
  //       w: width indicator bit (0: ASCII, 1: UTF-16)
  //
  // Note that the count of tagged Cocoa strings always fits in 4 bits, so
  // allocating 60 bits for the count field is overkill.

  @_inlineable
  public // @testable
  var _isSmallCocoa: Bool {
    @inline(__always)
    get { return _isTagged && _objectBitPattern & _smallBit != 0 }
  }

  @_inlineable
  @_versioned
  internal var _taggedCocoaCount: Int {
    let countMask: UInt = (1 &<< 60) &- 1
    return Int(bitPattern:
      _bridgeObject(toTagPayload: _object) & countMask)
  }

  @_inlineable
  @_versioned
  internal var _taggedCocoaObject: _CocoaString {
    @inline(__always) get {
      _sanityCheck(_isSmallCocoa)
      return Builtin.reinterpretCast(_otherBits)
    }
  }

  @_versioned
  @inline(never) // Hide CF dependency
  internal init(_taggedCocoaObject object: _CocoaString) {
    _sanityCheck(_isObjCTaggedPointer(object))
    let count = _stdlib_binary_CFStringGetLength(object)
    self.init(
      _bridgeObject(taggingPayload: UInt(count)),
      Builtin.reinterpretCast(object))
    self._objectBitPattern |= _smallBit | _twoByteCodeUnitBit
    _sanityCheck(_isSmallCocoa)
    _invariantCheck()
  }
}

extension _StringGuts {
  @_inlineable
  @_versioned
  internal
  var _unmanagedASCIIView: _UnmanagedString<UInt8> {
    _sanityCheck(isASCII)
    if _isUnmanaged {
      return _asUnmanaged()
    } else if _isNative {
      return _nativeStorage(of: UInt8.self).unmanagedView
    } else {
      _sanityCheck(_isContiguousCocoa)
      return _asContiguousCocoa(of: UInt8.self)
    }
  }

  @_inlineable
  @_versioned
  internal
  var _unmanagedUTF16View: _UnmanagedString<UTF16.CodeUnit> {
    _sanityCheck(!isASCII)
    if _isUnmanaged {
      return _asUnmanaged()
    } else if _isNative {
      return _nativeStorage(of: UTF16.CodeUnit.self).unmanagedView
    } else if _isContiguousCocoa {
      return _asContiguousCocoa(of: UTF16.CodeUnit.self)
    } else {
      _sanityCheckFailure("String isn't contiguous")
    }
  }
}

extension _StringGuts {
  /// Return an NSString instance containing a slice of this string.
  /// The returned object may contain unmanaged pointers into the
  /// storage of this string; you are responsible for ensuring that
  /// it will not outlive `self`.
  @_inlineable
  @_versioned
  internal
  func _ephemeralCocoaString() -> _CocoaString {
    if _isNative {
      return _nativeRawStorage
    } else if _isNonTaggedCocoa {
      return _nonTaggedCocoaObject
    } else if _isSmallCocoa {
      return _taggedCocoaObject
    } else if isASCII {
      return _NSContiguousString(
        _StringGuts(_asUnmanaged(of: UInt8.self)))
    } else {
      return _NSContiguousString(
        _StringGuts(_asUnmanaged(of: UTF16.CodeUnit.self)))
    }
  }

  /// Return an NSString instance containing a slice of this string.
  /// The returned object may contain unmanaged pointers into the
  /// storage of this string; you are responsible for ensuring that
  /// it will not outlive `self`.
  @_inlineable
  @_versioned
  internal
  func _ephemeralCocoaString(_ range: Range<Int>) -> _CocoaString {
    if _slowPath(_isOpaque) {
      return _asOpaque()[range].cocoaSlice()
    } else {
      return _NSContiguousString(_unmanaged: self, range: range)
    }
  }
}

extension _StringGuts {
  public // @testable
  var _underlyingCocoaString: _CocoaString? {
    if _isNative {
      return _nativeRawStorage
    } else if _isNonTaggedCocoa {
      return _nonTaggedCocoaObject
    } else if _isSmallCocoa {
      return _taggedCocoaObject
    } else {
      return nil
    }
  }
}

extension _StringGuts {
  // Opaque strings

  @_inlineable
  @_versioned
  internal
  var _isContiguous: Bool {
    @inline(__always) get {
      return !_isOpaque
    }
  }

  @_inlineable
  @_versioned
  internal
  var _isOpaque: Bool {
    @inline(__always) get {
      return _isSmallCocoa || _isNoncontiguousCocoa
    }
  }

  @inline(never)
  @_versioned
  internal func _asOpaque() -> _UnmanagedOpaqueString {
    if _isSmallCocoa {
      return _UnmanagedOpaqueString(
        _taggedCocoaObject,
        count: _taggedCocoaCount)
    } else {
      _sanityCheck(_isNoncontiguousCocoa)
      return _UnmanagedOpaqueString(
        _nonTaggedCocoaObject,
        count: _nonTaggedCocoaCount)
    }
  }
}

// Internal hex dumping function. Useful because `print` is implemented in the
// stdlib through a series of very high level String calls, resulting in
// infinite recursion.
internal func internalDumpHexImpl(_ x: UInt, newline: Bool) {
  _swift_stdlib_print_hex(x, newline ? 1 : 0)
}
internal func internalDumpHex(_ x: _BuiltinNativeObject, newline: Bool) {
  internalDumpHexImpl(Builtin.reinterpretCast(x), newline: newline)
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


//
// Stats
//
extension _StringGuts {
  enum Stats {
    internal static var numNativeSelfSlice = 0
    internal static var numCocoaSelfSlice = 0
  }
}

extension _LegacyStringCore {
  @_versioned // FIXME(sil-serialize-all)
  internal func _copyToNativeStorage<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _SwiftStringStorage<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    let count = self.count
    let storage = _SwiftStringStorage<CodeUnit>.create(
      capacity: count,
      count: count)
    _LegacyStringCore._copyElements(
      _baseAddress!, srcElementWidth: elementWidth,
      dstStart: UnsafeMutableRawPointer(storage.start),
      dstElementWidth: CodeUnit.bitWidth >> 3,
      count: count)
    return storage
  }
}

// Conversions two/from legacy core
extension _StringGuts {
  @_versioned
  var _legacyCore: _LegacyStringCore {
    defer { _fixLifetime(self) }
    if _isUnmanaged {
      return _LegacyStringCore(
        baseAddress: UnsafeMutableRawPointer(mutating: _unmanagedRawStart),
        count: _unmanagedCount,
        elementShift: isSingleByte ? 0 : 1,
        hasCocoaBuffer: false,
        owner: nil)
    } else if _isNative {
      return makeCocoaLegacyStringCore(_cocoaString: _nativeRawStorage)
    } else if _isNonTaggedCocoa {
      return makeCocoaLegacyStringCore(_cocoaString: _nonTaggedCocoaObject)
    } else {
      _sanityCheck(_isSmallCocoa)
      return makeCocoaLegacyStringCore(_cocoaString: _taggedCocoaObject)
    }
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
      if legacyCore.elementWidth == 1 {
        let immortal = _UnmanagedString(
          start: baseAddress.assumingMemoryBound(to: UInt8.self),
          count: legacyCore.count)
        self.init(immortal)
        return
      } else {
        let immortal = _UnmanagedString(
          start: baseAddress.assumingMemoryBound(to: UTF16.CodeUnit.self),
          count: legacyCore.count)
        self.init(immortal)
        return
      }
    }

    if _fastPath(legacyCore.hasCocoaBuffer) {
      let guts = _makeCocoaStringGuts(owner)
      // NOTE: Sometimes a _LegacyStringCore is a self-slice of a Cocoa string
      // without having properly sliced the backing Cocoa string itself. Detect
      // that situation in retrospect and create a native copy.
      if _slowPath(guts.count != legacyCore.count) {
        Stats.numCocoaSelfSlice += 1
        self.init(legacyCore._copyToNativeStorage(of: UTF16.CodeUnit.self))
        return
      }
      self = guts
      return
    }

    let stringBuffer = unsafeBitCast(owner, to: _StringBuffer.self)
    if _slowPath(stringBuffer.usedCount != legacyCore.count) {
      Stats.numNativeSelfSlice += 1
    }
    if stringBuffer.elementWidth == 1 {
      self.init(legacyCore._copyToNativeStorage(of: UInt8.self))
    } else {
      self.init(legacyCore._copyToNativeStorage(of: UTF16.CodeUnit.self))
    }
  }
}

extension _StringGuts {
  public func _dump() {
    print("_StringGuts(", terminator: "")
    internalDumpHex(_objectBitPattern, newline: false)
    print(" ", terminator: "")
    internalDumpHex(_otherBits, newline: false)
    print(": ", terminator: "")
    if _isNative {
      let storage = _nativeRawStorage
      print("native ", terminator: "")
      internalDumpHex(storage, newline: false)
      print(" start: ", terminator: "")
      internalDumpHex(storage.rawStart, newline: false)
      print(" count: ", terminator: "")
      print(storage.count, terminator: "")
      print("/", terminator: "")
      print(storage.capacity, terminator: "")
    } else if _isNonTaggedCocoa {
      print("cocoa ", terminator: "")
      internalDumpHex(_nonTaggedCocoaObject, newline: false)
      print(" start: ", terminator: "")
      if let start = _nonTaggedCocoaRawStart {
        internalDumpHex(start, newline: false)
      } else {
        print("<opaque>", terminator: "")
      }
      print(" count: ", terminator: "")
      print(_nonTaggedCocoaCount, terminator: "")
    } else if _isUnmanaged {
      print("unmanaged ", terminator: "")
      internalDumpHex(_unmanagedRawStart, newline: false)
      print(" count: ", terminator: "")
      print(_unmanagedCount, terminator: "")
    } else if _isSmallCocoa {
      print("small cocoa ", terminator: "")
      internalDumpHex(_taggedCocoaObject, newline: false)
      print(" count: ", terminator: "")
      print(_taggedCocoaCount, terminator: "")
    } else {
      print("error", terminator: "")
    }
    if isASCII {
      print(" <ascii>", terminator: "")
    }
    else {
      print(" <utf16>", terminator: "")
    }
    print(")")
  }
}

//
// String API helpers
//
extension _StringGuts {
  // Return a native storage object with the same contents as this string.
  // Use the existing buffer if possible; otherwise copy the string into a
  // new buffer.
  @_versioned
  internal
  func _extractNativeStorage<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _SwiftStringStorage<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    if _fastPath(_isNative && CodeUnit.bitWidth == self.bitWidth) {
      return _bridgeObject(toNative: _object) as! _SwiftStringStorage<CodeUnit>
    }
    let count = self.count
    return _copyToNativeStorage(from: 0..<count, capacity: count)
  }

  @_specialize(where CodeUnit == UInt8)
  @_specialize(where CodeUnit == UInt16)
  @_specialize(where CodeUnit == UTF16.CodeUnit)
  @_versioned
  @_inlineable
  internal
  func _copyToNativeStorage<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self,
    from range: Range<Int>,
    capacity: Int = 0
  ) -> _SwiftStringStorage<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(capacity >= 0)
    let storage = _SwiftStringStorage<CodeUnit>.create(
      capacity: Swift.max(range.count, capacity),
      count: range.count)
    self._copy(range: range, into: storage.usedBuffer)
    return storage
  }

  @_inlineable
  public // @testable
  func _extractSlice(_ range: Range<Int>) -> _StringGuts {
    if range == 0..<count { return self }
    switch (isASCII, _isUnmanaged) {
    case (true, true):
        return _StringGuts(_asUnmanaged(of: UInt8.self)[range])
    case (true, false):
      return _StringGuts(_copyToNativeStorage(of: UInt8.self, from: range))
    case (false, true):
      return _StringGuts(_asUnmanaged(of: UTF16.CodeUnit.self)[range])
    case (false, false):
      return _StringGuts(
        _copyToNativeStorage(of: UTF16.CodeUnit.self, from: range))
    }
  }

  @_inlineable
  @inline(__always)
  public // @testable
  mutating func isUniqueNative() -> Bool {
    guard _isNative else { return false }
    // Note that the isUnique test must be in a separate statement;
    // `_isNative && _isUnique` always evaluates to false in debug builds,
    // because SILGen keeps the self reference in `_isNative` alive for the
    // duration of the expression.
    return _isUnique(&_storage.0)
  }

  @_inlineable
  @_versioned
  internal mutating func allocationParametersForMutableStorage<CodeUnit>(
    of type: CodeUnit.Type,
    unusedCapacity: Int
  ) -> (count: Int, capacity: Int)?
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    if _slowPath(!_isNative) {
      return (self.count, count + unusedCapacity)
    }
    unowned(unsafe) let storage = _nativeRawStorage
    defer { _fixLifetime(self) }
    if _slowPath(storage.unusedCapacity < unusedCapacity) {
      return (
        storage.count,
        Swift.max(
          _growArrayCapacity(storage.capacity),
          count + unusedCapacity))
    }
    if _fastPath(self.bitWidth == CodeUnit.bitWidth) {
      if _fastPath(isUniqueNative()) {
        return nil
      }
    }
    return (storage.count, storage.capacity)
  }

  // Convert ourselves (if needed) to a native string with the specified storage
  // parameters and call `body` on the resulting native storage.
  @_inlineable
  @_versioned
  internal
  mutating func withMutableStorage<CodeUnit, R>(
    of type: CodeUnit.Type = CodeUnit.self,
    unusedCapacity: Int,
    _ body: (Unmanaged<_SwiftStringStorage<CodeUnit>>) -> R
  ) -> R
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    let paramsOpt = allocationParametersForMutableStorage(
      of: CodeUnit.self,
      unusedCapacity: unusedCapacity)
    if _fastPath(paramsOpt == nil) {
      unowned(unsafe) let storage = _nativeStorage(of: CodeUnit.self)
      let result = body(Unmanaged.passUnretained(storage))
      self._nativeCount = storage.count
      _fixLifetime(self)
      return result
    }
    let params = paramsOpt._unsafelyUnwrappedUnchecked
    let unmanagedRef = Unmanaged.passRetained(
      self._copyToNativeStorage(
        of: CodeUnit.self,
        from: 0..<params.count,
        capacity: params.capacity))
    let result = body(unmanagedRef)
    self = _StringGuts(unmanagedRef.takeRetainedValue())
    _fixLifetime(self)
    return result
  }

  @_inlineable
  @_versioned
  @inline(__always)
  internal
  mutating func withMutableASCIIStorage<R>(
    unusedCapacity: Int,
    _ body: (Unmanaged<_ASCIIStringStorage>) -> R
  ) -> R {
    return self.withMutableStorage(
      of: UInt8.self, unusedCapacity: unusedCapacity, body)
  }

  @_inlineable
  @_versioned
  @inline(__always)
  internal
  mutating func withMutableUTF16Storage<R>(
    unusedCapacity: Int,
    _ body: (Unmanaged<_UTF16StringStorage>) -> R
  ) -> R {
    return self.withMutableStorage(
      of: UTF16.CodeUnit.self, unusedCapacity: unusedCapacity, body)
  }
}

//
// String API
//
extension _StringGuts {
  @_versioned
  @_inlineable
  internal var startIndex: Int {
    return 0
  }

  @_versioned
  @_inlineable
  internal var endIndex: Int {
    @inline(__always) get { return count }
  }

  @_inlineable
  public // @testable
  var count: Int {
    if _slowPath(_isSmallCocoa) {
      return _taggedCocoaCount
    } else if _slowPath(_isNonTaggedCocoa) {
      return _nonTaggedCocoaCount
    }
    _sanityCheck(Int(self._otherBits) >= 0)
    return Int(bitPattern: self._otherBits)
  }

  @_inlineable
  public // @testable
  var capacity: Int {
    if _fastPath(_isNative) {
      return self._nativeRawStorage.capacity
    }
    return 0
  }

  /// Get the UTF-16 code unit stored at the specified position in this string.
  @_inlineable // FIXME(sil-serialize-all)
  public // @testable
  subscript(position: Int) -> UTF16.CodeUnit {
    if isASCII {
      return _unmanagedASCIIView[position]
    } else if _slowPath(_isOpaque) {
      return _asOpaque()[position]
    } else {
      return _unmanagedUTF16View[position]
    }
  }

  // Copy code units from a slice of this string into a buffer.
  @_versioned
  internal func _copy<CodeUnit>(
    range: Range<Int>,
    into dest: UnsafeMutableBufferPointer<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(CodeUnit.bitWidth == 8 || CodeUnit.bitWidth == 16)
    _sanityCheck(dest.count >= range.count)
    if isASCII {
      _unmanagedASCIIView[range]._copy(into: dest)
    } else if _slowPath(_isOpaque) {
      _asOpaque()[range]._copy(into: dest)
    } else {
      _unmanagedUTF16View[range]._copy(into: dest)
    }
  }

  public // TODO(StringGuts): for testing
  mutating func reserveCapacity<CodeUnit>(
    _ capacity: Int,
    of codeUnit: CodeUnit.Type = CodeUnit.self)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    if _fastPath(isUniqueNative()) {
      if _fastPath(bitWidth == CodeUnit.bitWidth) {
        if _fastPath(_nativeRawStorage.capacity >= capacity) {
          return
        }
      }
    }
    let count = self.count
    let storage = _copyToNativeStorage(
      of: CodeUnit.self,
      from: 0..<count,
      capacity: Swift.max(capacity, count))
    self = _StringGuts(storage)
    _invariantCheck()
  }

  @_inlineable
  @_versioned
  internal
  mutating func append(_ other: _UnmanagedASCIIString) {
    guard other.count > 0 else { return  }
    if isSingleByte {
      withMutableASCIIStorage(unusedCapacity: other.count) { storage in
        storage._value._appendInPlace(other)
      }
    } else {
      withMutableUTF16Storage(unusedCapacity: other.count) { storage in
        storage._value._appendInPlace(other)
      }
    }
  }

  @_inlineable
  @_versioned
  internal
  mutating func append(_ other: _UnmanagedUTF16String) {
    guard other.count > 0 else { return  }
    withMutableUTF16Storage(unusedCapacity: other.count) { storage in
      storage._value._appendInPlace(other)
    }
  }

  @_inlineable
  @_versioned
  internal
  mutating func append(_ other: _UnmanagedOpaqueString) {
    guard other.count > 0 else { return  }
    withMutableUTF16Storage(unusedCapacity: other.count) { storage in
      storage._value._appendInPlace(other)
    }
  }

  @_inlineable
  public // TODO(StringGuts): for testing only
  mutating func append(_ other: _StringGuts) {
    if _isEmptyLiteral {
      self = other
      return
    }
    if other.isASCII {
      self.append(other._unmanagedASCIIView)
    } else if _slowPath(other._isOpaque) {
      self.append(other._asOpaque())
    } else {
      self.append(other._unmanagedUTF16View)
    }
    _fixLifetime(other)
  }

  @_inlineable
  public // TODO(StringGuts): for testing only
  mutating func append(_ other: _StringGuts, range: Range<Int>) {
    _sanityCheck(range.lowerBound >= 0 && range.upperBound <= other.count)
    guard range.count > 0 else { return }
    if _isEmptyLiteral && range.count == other.count {
      self = other
      return
    }
    if other.isASCII {
      self.append(other._unmanagedASCIIView[range])
    } else if _slowPath(other._isOpaque) {
      self.append(other._asOpaque()[range])
    } else {
      self.append(other._unmanagedUTF16View[range])
    }
    _fixLifetime(other)
  }

  public // @testable
  mutating func append<C : Collection>(contentsOf other: C)
  where C.Element == UTF16.CodeUnit {
    if isSingleByte && !other.contains(where: { $0 > 0x7f }) {
      withMutableASCIIStorage(
        unusedCapacity: numericCast(other.count)) { storage in
        storage._value._appendInPlaceUTF16(contentsOf: other)
      }
      return
    }
    withMutableUTF16Storage(
      unusedCapacity: numericCast(other.count)) { storage in
      storage._value._appendInPlaceUTF16(contentsOf: other)
    }
  }
}

extension _StringGuts {
  @_versioned
  mutating func _replaceSubrange<C, CodeUnit>(
    _ bounds: Range<Int>,
    with newElements: C,
    of codeUnit: CodeUnit.Type
  ) where C : Collection, C.Element == UTF16.CodeUnit,
  CodeUnit : FixedWidthInteger & UnsignedInteger {
    _precondition(bounds.lowerBound >= 0,
      "replaceSubrange: subrange start precedes String start")

    let newCount: Int = numericCast(newElements.count)
    let deltaCount = newCount - bounds.count
    let paramsOpt = allocationParametersForMutableStorage(
      of: CodeUnit.self,
      unusedCapacity: Swift.max(0, deltaCount))

    if _fastPath(paramsOpt == nil) {
      // We have unique native storage of the correct code unit,
      // with enough capacity to do the replacement inline.
      unowned(unsafe) let storage = _nativeStorage(of: CodeUnit.self)
      _sanityCheck(storage.unusedCapacity >= deltaCount)
      let tailCount = storage.count - bounds.upperBound
      _precondition(tailCount >= 0,
        "replaceSubrange: subrange extends past String end")
      var dst = storage.start + bounds.lowerBound
      if deltaCount != 0 && tailCount > 0 {
        // Move tail to make space for new data
        (dst + newCount).moveInitialize(
          from: dst + bounds.count,
          count: tailCount)
      }
      // Copy new elements in place
      var it = newElements.makeIterator()
      for p in dst ..< (dst + newCount) {
        p.pointee = CodeUnit(it.next()!)
      }
      _precondition(it.next() == nil, "Collection misreported its count")
      storage.count += deltaCount
      _nativeCount += deltaCount
      _invariantCheck()
      _fixLifetime(self)
      return
    }

    // Allocate new storage.
    let params = paramsOpt._unsafelyUnwrappedUnchecked
    _precondition(bounds.upperBound <= params.count,
        "replaceSubrange: subrange extends past String end")
    let storage = _SwiftStringStorage<CodeUnit>.create(
      capacity: params.capacity,
      count: params.count + deltaCount)
    var dst = storage.start
    // Copy prefix up to replaced range
    let prefixRange: Range<Int> = 0..<bounds.lowerBound
    _copy(
      range: prefixRange,
      into: UnsafeMutableBufferPointer(start: dst, count: prefixRange.count))
    dst += prefixRange.count

    // Copy new data
    var it = newElements.makeIterator()
    for p in dst ..< (dst + newCount) {
      p.pointee = CodeUnit(it.next()!)
    }
    _precondition(it.next() == nil, "Collection misreported its count")
    dst += newCount

    // Copy suffix from end of replaced range
    let suffixRange: Range<Int> = bounds.upperBound..<params.count
    _copy(
      range: suffixRange,
      into: UnsafeMutableBufferPointer(start: dst, count: suffixRange.count))
    _sanityCheck(dst + suffixRange.count == storage.end)
    self = _StringGuts(storage)
    _invariantCheck()
  }

  public mutating func replaceSubrange<C>(
    _ bounds: Range<Int>,
    with newElements: C
  ) where C : Collection, C.Element == UTF16.CodeUnit {
    if isASCII && !newElements.contains(where: {$0 > 0x7f}) {
      self._replaceSubrange(bounds, with: newElements, of: UInt8.self)
    } else {
      self._replaceSubrange(bounds, with: newElements, of: UTF16.CodeUnit.self)
    }
  }
}

//
// String API
//

// Some CharacterView operations
extension String {
  /// Accesses the character at the given position.
  ///
  /// You can use the same indices for subscripting a string and its substring.
  /// For example, this code finds the first letter after the first space:
  ///
  ///     let str = "Greetings, friend! How are you?"
  ///     let firstSpace = str.index(of: " ") ?? str.endIndex
  ///     let substr = str[firstSpace...]
  ///     if let nextCapital = substr.index(where: { $0 >= "A" && $0 <= "Z" }) {
  ///         print("Capital after a space: \(str[nextCapital])")
  ///     }
  ///     // Prints "Capital after a space: H"
  ///
  /// - Parameter i: A valid index of the string. `i` must be less than the
  ///   string's end index.
  @_inlineable // FIXME(sil-serialize-all)
  public subscript(i: Index) -> Character {
    return _guts.character(at: i)
  }
}

extension _StringGuts {
  @_inlineable
  @_versioned
  internal
  func character(at i: String.Index) -> Character {
    if isASCII {
      return _unmanagedASCIIView.character(at: i)
    } else if _slowPath(_isOpaque) {
      return _asOpaque().character(at: i)
    } else {
      return _unmanagedUTF16View.character(at: i)
    }
  }

  @_inlineable
  @_versioned
  internal
  func characterIndex(after i: String.Index) -> String.Index {
    if isASCII {
      return _unmanagedASCIIView.characterIndex(after: i)
    } else if _slowPath(_isOpaque) {
      return _asOpaque().characterIndex(after: i)
    } else {
      return _unmanagedUTF16View.characterIndex(after: i)
    }
  }

  @_inlineable
  @_versioned
  internal
  func characterIndex(before i: String.Index) -> String.Index {
    if isASCII {
      return _unmanagedASCIIView.characterIndex(before: i)
    } else if _slowPath(_isOpaque) {
      return _asOpaque().characterIndex(before: i)
    } else {
      return _unmanagedUTF16View.characterIndex(before: i)
    }
  }

  // @_inlineable
  // @_versioned
  // internal
  // func characterIndex(
  //   _ i: String.Index, offsetBy n: String.IndexDistance
  // ) -> String.Index {
  //   if isASCII {
  //     return _unmanagedASCIIView.characterIndex(i, offsetBy: n)
  //   } else if _slowPath(_isOpaque) {
  //     return _asOpaque().characterIndex(i, offsetBy: n) // TODO
  //   } else {
  //     return _unmanagedUTF16View.characterIndex(i, offsetBy: n)
  //   }
  // }
}

extension _UnmanagedString {
  @_inlineable
  @_versioned
  internal
  func characterStride(from i: String.Index) -> Int {
    // TODO: should _fastPath the case somehow
    if case .character(let _stride) = i._cache {
      return Int(truncatingIfNeeded: _stride)
    } else {
      return self._measureExtendedGraphemeClusterForward(from: i.encodedOffset)
    }
  }

  @_inlineable
  @_versioned
  internal
  func character(at i: String.Index) -> Character {
    let offset = i.encodedOffset
    _precondition(offset < self.count, "String index is out of range")
    _precondition(offset >= 0, "String index cannot be negative")

    let stride = self.characterStride(from: i)
    if _fastPath(stride == 1) {
      return Character(_singleCodeUnit: self[offset])
    }
    _precondition(offset + stride <= self.count, "String index is out of range")
    return Character(self[offset..<offset+stride])
  }

  @_versioned
  internal
  func characterIndex(after i: String.Index) -> String.Index {
    // TODO: Is this the right place to do precondition checking?
    _precondition(i.encodedOffset >= 0, "cannot increment invalid index")
    _precondition(i.encodedOffset < count, "cannot increment beyond endIndex")

    let stride = self.characterStride(from: i)
    let newOffset = i.encodedOffset + stride
    _sanityCheck(newOffset <= count, "walked off the end")

    // Calculate and cache the next grapheme distance
    let newStride = self._measureExtendedGraphemeClusterForward(from: newOffset)
    _sanityCheck(newStride >= 0 && newStride <= UInt16.max)

    return String.Index(
      encodedOffset: newOffset,
      .character(stride: UInt16(truncatingIfNeeded: newStride)))
  }

  @inline(never) // @_inlineable
  @_versioned
  internal
  func characterIndex(before i: String.Index) -> String.Index {
    // TODO: implement directly
    return String.CharacterView(_StringGuts(self)).index(before: i)
  }

  // @inline(never) // @_inlineable
  // @_versioned
  // internal
  // func characterIndex(_ i: String.Index, offsetBy n: String.IndexDistance) -> String.Index {
  //   // TODO: implement directly
  //   return String.CharacterView(_StringGuts(self)).index(i, offsetBy: n)
  // }

  @_versioned
  @_inlineable
  internal
  func _measureExtendedGraphemeClusterForward(from idx: Int) -> Int {
    let startOffset = idx
    let endOffset = self.endIndex

    // No more graphemes
    if startOffset == count {
      return 0
    }

    // Last code unit means final grapheme length of 1
    if startOffset == count - 1 {
      return 1
    }

    // Grapheme breaking is much simpler if known ASCII
    if CodeUnit.bitWidth == 8 {
      _onFastPath() // Please aggressively inline

      // With the exception of CR-LF, ASCII graphemes are single-scalar. Check
      // for that one exception.
      if _slowPath(
        self.start[startOffset] == _CR &&
        self.start[startOffset+1] == _LF // See bounds check above
      ) {
        return 2
      }

      return 1
    }

    // Perform a quick single-code-unit grapheme check.
    _sanityCheck(CodeUnit.self == UTF16.CodeUnit.self)
    if _fastPath(String.CharacterView._quickCheckGraphemeBreakBetween(
        UTF16.CodeUnit(self.start[startOffset]),
        UTF16.CodeUnit(self.start[startOffset+1]))
    ) {
      return 1
    }

    return _measureExtendedGraphemeClusterForwardSlow(startOffset: startOffset)
  }

  @inline(never)
  @_versioned
  internal
  func _measureExtendedGraphemeClusterForwardSlow(startOffset: Int) -> Int {
    // TODO: implement directly
    return String.CharacterView(
      _StringGuts(self)
    )._measureExtendedGraphemeClusterForwardSlow(startOffset: startOffset)
  }
}

extension _UnmanagedOpaqueString {
  // FIXME: Remove
  var characterView: String.CharacterView {
    return String.CharacterView(_makeCocoaStringGuts(self.cocoaSlice()))
  }

  @_versioned
  internal
  func characterIndex(after i: String.Index) -> String.Index {
    // TODO: implement directly
    return characterView.index(after: i)
  }

  @_versioned
  internal
  func characterIndex(before i: String.Index) -> String.Index {
    // TODO: implement directly
    return characterView.index(before: i)
  }

  @_versioned
  internal
  func character(at i: String.Index) -> Character {
    // TODO: implement directly
    return characterView[i]
  }
}

extension Character {
  @_inlineable
  @_versioned
  internal
  init(_singleCodeUnit cu: UInt16) {
    _representation = .smallUTF16(
      Builtin.zext_Int16_Int63(Builtin.reinterpretCast(cu)))
  }

  @_inlineable
  @_versioned
  internal
  init<CodeUnit>(_ unsafeStr: _UnmanagedString<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    if _fastPath(unsafeStr.count <= 4) {
      let b = _UIntBuffer<UInt64, Unicode.UTF16.CodeUnit>(unsafeStr)
      if _fastPath(Int64(bitPattern: b._storage) >= 0) {
        _representation = .smallUTF16(
          Builtin.trunc_Int64_Int63(b._storage._value))
        return
      }
    }
    self = Character(_largeRepresentationString: String(_StringGuts(unsafeStr)))
  }
}
