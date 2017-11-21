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
    get { return _storage.0 }
    set { _storage.0 = newValue }
  }
  @_inlineable
  public
  var _otherBits: UInt {
    @inline(__always)
    get { return _storage.1 }
    set { _storage.1 = newValue }
  }
  @_inlineable
  public
  var _objectBitPattern: UInt {
    @inline(__always)
    get { return _bitPattern(_object) }
    set { _object = Builtin.reinterpretCast(newValue) }
  }

  @_inlineable
  public
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
// "StringObject"-like struct, and provide this funcitonality on that.
//
extension _StringGuts {
  @_versioned
  @_inlineable
  /*private*/ internal var _flagsMask: UInt {
    @inline(__always)
    get { return _twoByteCodeUnitBit }
  }

  @_versioned
  var isSingleByte: Bool {
    if _isSmallCocoa {
      return false
    }
    _sanityCheck(_isNative || _isUnsafe || _isNonTaggedCocoa)
    return (_objectBitPattern & _twoByteCodeUnitBit) == 0
  }

  @_versioned
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

  var _untaggedUnflaggedBitPattern: UInt {
    _sanityCheck(_isTagged)
    return _bridgeObject(toTagPayload: _unflaggedObject)
  }

  @_inlineable
  @_versioned
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

  @_versioned
  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    if let native = self._native {
      _sanityCheck(self.isSingleByte == native.isSingleByte)
      _sanityCheck(self.count == native.count)
      _sanityCheck(UInt(self.count) == self._otherBits)
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
internal
struct UnsafeString {
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
  internal
  var count: Int

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

  @inline(__always)
  @_inlineable
  @_versioned
  internal
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
  var utf16Start: UnsafePointer<UInt16> {
    _sanityCheck(!isSingleByte)
    return baseAddress.assumingMemoryBound(to: UInt16.self)
  }
  @_versioned
  var utf16Buffer: UnsafeBufferPointer<UInt16> {
    _sanityCheck(!isSingleByte)
    return UnsafeBufferPointer(start: utf16Start, count: count)
  }

  @_versioned
  var asciiStart: UnsafePointer<UInt8> {
    _sanityCheck(isSingleByte)
    return baseAddress.assumingMemoryBound(to: UInt8.self)
  }
  @_versioned
  var asciiBuffer: UnsafeBufferPointer<UInt8> {
    _sanityCheck(isSingleByte)
    return UnsafeBufferPointer(start: asciiStart, count: count)
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

extension UnsafeString: RandomAccessCollection {
  typealias Index = Int
  typealias Element = UInt16
  typealias SubSequence = UnsafeString
  typealias Indices = CountableRange<Int>

  @_inlineable
  @_versioned
  internal
  var startIndex: Int { return 0 }

  @_inlineable
  @_versioned
  internal
  var endIndex: Int { return count }

  @_inlineable
  @_versioned
  internal
  func index(after i: Int) -> Int {
    return i+1
  }

  @_inlineable
  @_versioned
  internal
  func index(before i: Int) -> Int {
    return i-1
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
  var nativeObject: _BuiltinNativeObject {
    return stringBuffer._nativeObject
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

  var count: Int

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
  init(capacity: Int, byteWidth: Int) {
    _sanityCheck(byteWidth == 1 || byteWidth == 2)
    self.stringBuffer = _StringBuffer(
      capacity: capacity,
      initialSize: 0,
      elementWidth: byteWidth)
    self.count = 0
  }

  @_versioned
  init(nativeObject: _BuiltinNativeObject, count: Int) {
    self.stringBuffer = _StringBuffer(
      _StringBuffer._Storage(_nativeObject: nativeObject))
    self.count = count
    _invariantCheck()
  }

  @_versioned
  internal
  mutating
  func _appendInPlace(_ u: UTF16.CodeUnit) {
    _sanityCheck(self.capacity >= self.count + 1)
    _sanityCheck(u <= 0x7f || byteWidth == 2)
    if _fastPath(byteWidth == 1) {
      stringBuffer.usedEnd
        .assumingMemoryBound(to: UInt8.self).pointee = UInt8(u)
      stringBuffer.usedEnd += 1
    } else {
      stringBuffer.usedEnd
        .assumingMemoryBound(to: UTF16.CodeUnit.self).pointee = u
      stringBuffer.usedEnd += 2
    }
    self.count += 1
  }

  @_versioned
  internal
  mutating
  func _appendInPlace(_ u0: UTF16.CodeUnit, _ u1: UTF16.CodeUnit) {
    _sanityCheck(self.capacity >= self.count + 2)
    _sanityCheck(byteWidth == 2)
    let d = stringBuffer.usedEnd.assumingMemoryBound(to: UTF16.CodeUnit.self)
    d[0] = u0
    d[1] = u1
    stringBuffer.usedEnd += 4
    self.count += 2
  }

  // Append a range of code units from `other` directly to the end of
  // this string, which must have uniquely referenced storage with
  // large enough capacity of a suitable element width.
  @_versioned
  internal
  mutating
  func _appendInPlace(_ other: _StringGuts, range: Range<Int>) {
    _sanityCheck(self.capacity >= self.count + range.count)

    other._copy(
      range: range,
      into: stringBuffer.usedEnd,
      capacityEnd: stringBuffer.capacityEnd,
      accomodatingElementWidth: stringBuffer.elementWidth)
    stringBuffer.usedEnd += range.count &<< stringBuffer.elementShift
    self.count = stringBuffer.usedCount
  }

  // Ensure that this string has enough capacity for at least `minimumCapacity`
  // number of code units of `minimumByteWidth` width, copying contents
  // into a newly allocated buffer if necessary.
  //
  // If `forcingRellocation` is true, a new buffer is allocated regardless
  // of the properties of current storage.
  @_versioned
  internal mutating func _ensureStorage(
    minimumCapacity: Int,
    minimumByteWidth: Int,
    forcingReallocation: Bool = false
  ) {
    _sanityCheck(minimumByteWidth == 1 || minimumByteWidth == 2)
    _sanityCheck(minimumCapacity >= 0)
    let oldCapacity = self.capacity
    var newCapacity = minimumCapacity
    let newWidth = Swift.max(self.byteWidth, minimumByteWidth)
    if _slowPath(oldCapacity < minimumCapacity) {
      newCapacity = Swift.max(_growArrayCapacity(oldCapacity), minimumCapacity)
    } else if _fastPath(!forcingReallocation && self.byteWidth == newWidth) {
      return
    }
    // TODO: width extension can be done in place if there's capacity
    let buffer = _StringBuffer(
      capacity: newCapacity,
      initialSize: count,
      elementWidth: newWidth)
    self.unsafe._copy(
      into: buffer.start,
      capacityEnd: buffer.capacityEnd,
      accomodatingElementWidth: newWidth)
    self.stringBuffer = buffer
  }

  // Append directly to the end of this string, whose buffer must be
  // uniquely referenced. Grow buffer if there isn't enough capacity
  // and widen storage when necessary.
  @_versioned
  internal mutating func append(_ other: _StringGuts, range: Range<Int>) {
    _sanityCheck(range.lowerBound >= 0 && range.upperBound <= other.count)
    guard !range.isEmpty else { return }
    _ensureStorage(
      minimumCapacity: count + range.count,
      minimumByteWidth: other.byteWidth,
      forcingReallocation: false)
    _appendInPlace(other, range: range)
  }

  // Append directly to the end of this string, whose buffer must be
  // uniquely referenced. Grow buffer if there isn't enough capacity.
  @_versioned
  internal mutating func append<S: StringProtocol>(_ other: S) {
    self.append(other._wholeString._guts, range: other._encodedOffsetRange)
  }

  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    _sanityCheck(self.count == self.stringBuffer.usedCount)
#endif
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
    @inline(never)
    get {
      guard let start = start else { return nil }
      let count = _stdlib_binary_CFStringGetLength(owner)
      return UnsafeString(
        baseAddress: UnsafeMutableRawPointer(mutating: start),
        count: count,
        isSingleByte: isSingleByte)
    }
  }
}

@_versioned
@_fixed_layout
internal struct OpaqueCocoaString {
  @_versioned
  let object: AnyObject

  @_versioned
  let count: Int

  @inline(never)
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
    let count = Int(truncatingIfNeeded: _otherBits)
    _sanityCheck(count >= 0)
    return NativeString(
      nativeObject: _nativeObject(fromBridge: _object),
      count: count)
  }

  /*fileprivate*/ internal // TODO: private in Swift 4
  var _native: NativeString? {
    guard _isNative else { return nil }
    return _asNative
  }

  @_versioned
  /*fileprivate*/ internal // TODO: private in Swift 4
  init(_ s: NativeString) {
    s._invariantCheck()
    self.init(
      _unflagged: _bridgeObject(fromNativeObject: s.nativeObject),
      isSingleByte: s.isSingleByte,
      otherBits: UInt(truncatingIfNeeded: s.count))
  }

  @_versioned
  init(_ buffer: _StringBuffer) {
    let count = buffer.usedCount
    _sanityCheck(count >= 0)
    self.init(
      _unflagged: _bridgeObject(fromNativeObject: buffer._nativeObject),
      isSingleByte: buffer.elementWidth == 1,
      otherBits: UInt(truncatingIfNeeded: count))
  }

  //
  // Cocoa (non-tagged) Strings
  //
  ///*fileprivate*/ internal // TODO: private in Swift 4
  @_versioned
  @_inlineable
  internal
  var _isNonTaggedCocoa: Bool {
    @inline(__always)
    get { return _isNonTaggedObjCPointer(_object) }
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
    return _asUnsafe
  }

  @_versioned
  internal
  var _asUnsafe: UnsafeString {
    _sanityCheck(_isUnsafe)
    // Unflag it, untag it, and go
    let pointer = UnsafeMutableRawPointer(
      bitPattern: self._untaggedUnflaggedBitPattern
    )._unsafelyUnwrappedUnchecked
    let count = Int(truncatingIfNeeded: self._otherBits)
    _sanityCheck(count >= 0)
    return UnsafeString(
      baseAddress: pointer,
      count: count,
      isSingleByte: self.isSingleByte)
  }

  @_inlineable
  @_versioned
  init(_ s: UnsafeString) {
    // Tag it, flag it, and go
    let object = _bridgeObject(taggingPayload: UInt(bitPattern: s.baseAddress))
    self.init(
      _unflagged: object,
      isSingleByte: s.isSingleByte,
      otherBits: UInt(s.count))
  }

  @inline(__always)
  @_inlineable
  @_versioned
  init(_asciiPointer ptr: UnsafeRawPointer, codeUnitCount: Int) {
    _sanityCheck(codeUnitCount >= 0)
    self.init(
      _bridgeObject(taggingPayload: UInt(bitPattern: ptr)),
      UInt(bitPattern: codeUnitCount))
  }

  //
  // Tagged Cocoa Strings
  //

  ///*fileprivate*/ internal // TODO: private in Swift 4
  @_versioned
  @_inlineable
  internal
  var _isSmallCocoa: Bool {
    @inline(__always)
    get { return _isTagged && _objectBitPattern & _smallBit != 0 }
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
  @_versioned
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
      range: 0..<self.count,
      into: buffer.start,
      capacityEnd: buffer.capacityEnd,
      accomodatingElementWidth: byteWidth)
    return buffer
  }

  @_inlineable
  @_transparent
  public // TODO(StringGuts): for testing only
  mutating func isUniqueNative() -> Bool {
    guard _isNative else { return false }
    // Note that the isUnique test must be in a separate statement;
    // `_isNative && _isUnique` always evaluates to false in debug builds,
    // because SILGen keeps the self reference in `_isNative` alive for the
    // duration of the expression.
    return _isUnique(&_storage.0)
  }

  // Convert ourselves (if needed) to a native string with the specified storage
  // parameters. After this call, self is ready to be memcpy-ed into. Does not
  // affect count. Returns the extra capacity.
  @inline(never)
  @_versioned
  internal
  mutating func _ensureUniqueNative(
    minimumCapacity: Int, minimumByteWidth: Int
  ) -> (
    capBegin: UnsafeMutableRawPointer, capEnd: UnsafeMutableRawPointer
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
        return (nativeBuffer.usedEnd, nativeBuffer.capacityEnd)
      }
    }

    let newBuffer = _copyToStringBuffer(
      capacity: newCapacity,
      byteWidth: newWidth)
    let (capBegin, capEnd) = (newBuffer.usedEnd, newBuffer.capacityEnd)
    self = _StringGuts(newBuffer)
    return (capBegin, capEnd)
  }
  @inline(never)
  @_versioned
  internal
  mutating func _ensureUniqueNative(
    increasingCountBy otherCount: Int, minimumByteWidth: Int
  ) -> (
    capBegin: UnsafeMutableRawPointer, capEnd: UnsafeMutableRawPointer
  ) {
    _sanityCheck(otherCount >= 0)
    let (capBegin, capEnd) = _ensureUniqueNative(
      minimumCapacity: self.count + otherCount,
      minimumByteWidth: minimumByteWidth)

    // Adjust the count
    //
    // TODO: probably better to find a way to do in place, i.e. bitcast the
    // references as we know lifetime
    _sanityCheck(_isNative)
    var nativeBuffer = self._asNative.stringBuffer
    nativeBuffer.usedEnd += otherCount &<< nativeBuffer.elementShift
    self._otherBits += UInt(truncatingIfNeeded: otherCount)

    _sanityCheck(
      capBegin + otherCount == self._asNative.stringBuffer.usedEnd
      && capEnd == self._asNative.stringBuffer.capacityEnd)
    return (capBegin, capEnd)
  }

  // Copy code units from a slice of this string into a buffer.
  @_versioned
  internal
  func _copy(
    range: Range<Int>,
    into dest: UnsafeMutableRawPointer,
    capacityEnd: UnsafeMutableRawPointer,
    accomodatingElementWidth width: Int
  ) {
    _sanityCheck(byteWidth == 1 || byteWidth == 2)
    _sanityCheck(capacityEnd >= dest + (range.count &<< (byteWidth &- 1)))

    // TODO: Eventual small form check on other. We could even do this now for
    // the tagged cocoa strings.
    let unmanagedSelfOpt = self._unmanagedContiguous
    if _fastPath(unmanagedSelfOpt != nil) {
      let unsafe = unmanagedSelfOpt._unsafelyUnwrappedUnchecked
      unsafe[range]._copy(
        into: dest, capacityEnd: capacityEnd, accomodatingElementWidth: width)
      _fixLifetime(self)
      return
    }

    _sanityCheck(width == 2) // TODO: CFStringGetBytes
    let opaque = getOpaque()
    _cocoaStringCopyCharacters(
      from: opaque.object,
      range: range,
      into: dest.assumingMemoryBound(to: UTF16.CodeUnit.self))
  }

  @inline(__always)
  @_versioned
  internal
  func produceUnsafeFromNative() -> UnsafeString {
    _sanityCheck(_isNative)
    let ptr = _StringBuffer(_StringBuffer._Storage(
      _nativeObject: _nativeObject(fromBridge: _object))
    ).start
    let count = Int(truncatingIfNeeded: self._otherBits)
    _sanityCheck(count >= 0)
    return UnsafeString(
      baseAddress: ptr,
      count: count,
      isSingleByte: self.isSingleByte
    )
  }

  @inline(never)
  @_versioned
  internal
  func produceUnsafeFromCocoa() -> UnsafeString {
    _sanityCheck(_isNonTaggedCocoa && self._otherBits != 0)
    let count = _stdlib_binary_CFStringGetLength(
      _bridgeObject(toNonTaggedObjC: _object))
    let ptr = UnsafeMutableRawPointer(
      bitPattern: _otherBits)._unsafelyUnwrappedUnchecked
    return UnsafeString(
      baseAddress: ptr,
      count: count,
      isSingleByte: self.isSingleByte
    )
  }

  // NOTE: Follow up calls to this with _fixLifetime(self) after the last use of
  // the return value.
  @_versioned
  internal
  var _unmanagedContiguous: UnsafeString? {
    if _isUnsafe {
      return _asUnsafe
    }
    if _isNative {
      return produceUnsafeFromNative()
    }
    if _isNonTaggedCocoa {
      // Check for an opaque cocoa string
      if self._otherBits == 0 {
        return nil
      }
      return produceUnsafeFromCocoa()
    }
    return nil
  }

  @_versioned
  internal
  var _isOpaque: Bool {
    return _unmanagedContiguous == nil
  }

  @inline(never)
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
  init(_ s: OpaqueCocoaString) {
    self = _makeCocoaStringGuts(s.object)
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
  @_inlineable
  internal
  var startIndex: Int { return 0 }

  @_versioned
  @_inlineable
  internal
  var endIndex: Int { return count }

  @_versioned
  @_inlineable
  internal
  var count: Int {
    if _isSmallCocoa {
      return _stdlib_binary_CFStringGetLength(
        Builtin.reinterpretCast(self._otherBits))
    }
    if _isNonTaggedCocoa {
      return _stdlib_binary_CFStringGetLength(
        _bridgeObject(toNonTaggedObjC: _object))
    }

    _sanityCheck(Int(self._otherBits) >= 0)
    return Int(truncatingIfNeeded: self._otherBits)
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

  @inline(never)
  @_versioned
  internal
  mutating func assignTo(_ other: _StringGuts) {
    self = other
    return
  }

  @_inlineable
  public // TODO(StringGuts): for testing only
  mutating func append(_ other: /* @shared */ _StringGuts) {
    // TODO(perf): We just need to check for being the interned native empty
    // string
    if self._isEmpty && self.capacity == 0 {
      self.assignTo(other)
      return
    }

    let otherContigOpt = other._unmanagedContiguous
    if _slowPath(otherContigOpt == nil) {
      return self.append(opaque: other)
    }
    self.append(otherContigOpt._unsafelyUnwrappedUnchecked)
    _fixLifetime(other)
  }

  @inline(never)
  @_versioned
  internal
  mutating func append(opaque other: _StringGuts) {
    // TODO: implement more directly. Small cocoa strings are common and should
    // be fast-pathed here.
    _ = self._ensureUniqueNative(
      minimumCapacity: self.count + other.count,
      minimumByteWidth: other.byteWidth)
    var nativeSelf = self._asNative
    nativeSelf._appendInPlace(other, range: 0..<other.count)
    self = _StringGuts(nativeSelf)
    _invariantCheck()
  }

  @_versioned
  internal
  mutating func append(_ other: UnsafeString) {
    let (capBegin, capEnd) = self._ensureUniqueNative(
      increasingCountBy: other.count,
      minimumByteWidth: other.byteWidth)
    _sanityCheck(capEnd - capBegin >= other.count)

    other._copy(
      into: capBegin,
      capacityEnd: capEnd,
      accomodatingElementWidth: self.byteWidth
    )
    _invariantCheck()
  }


  // @_inlineable // TODO: internal-inlineable, if that's possible
  // TODO: @_versioned
  // TODO: internal
  public // TODO(StringGuts): for testing only
  mutating func append(_ other: _StringGuts, range: Range<Int>) {
    _sanityCheck(range.lowerBound >= 0 && range.upperBound <= other.count)
    guard range.count > 0 else { return }
    if _isEmpty &&
      capacity == 0 && // Don't discard reserved capacity, if any
      range.count == other.count {
      self = other
      return
    }

    // TODO: Eventual small form check on self and other. We could even do this
    // now for the tagged cocoa strings.

    self._ensureUniqueNative(
      minimumCapacity: self.count + range.count,
      minimumByteWidth: other.byteWidth)
    var nativeSelf = self._asNative
    nativeSelf._appendInPlace(other, range: range)
    self = _StringGuts(nativeSelf)
    _invariantCheck()
  }

  @_versioned
  internal mutating func append<C : Collection>(contentsOf other: C)
    where C.Element == UTF16.CodeUnit {
    var byteWidth = self.byteWidth
    // Widen string when needed
    if byteWidth == 1 && other.contains(where: { $0 > 0x7f }) {
      byteWidth = 2
    }
    self._ensureUniqueNative(
      minimumCapacity: self.count + numericCast(other.count),
      minimumByteWidth: byteWidth)
    var nativeSelf = self._asNative
    for codeunit in other {
      nativeSelf._appendInPlace(codeunit)
    }
    self = _StringGuts(nativeSelf)
    _invariantCheck()
  }

  @_versioned
  internal mutating func append(_ c: Unicode.Scalar) {
    let width = UTF16.width(c)
    if _fastPath(width == 1) {
      append(UTF16.CodeUnit(c.value))
    } else {
      append(UTF16.leadSurrogate(c), UTF16.trailSurrogate(c))
    }
  }

  @_versioned
  internal mutating func append(_ u: UTF16.CodeUnit) {
    _ensureUniqueNative(
      minimumCapacity: count + 1,
      minimumByteWidth: (u <= 0x7f ? 1 : 2))
    var nativeSelf = self._asNative
    nativeSelf._appendInPlace(u)
    _invariantCheck()
  }

  @_versioned
  internal mutating func append(_ u0: UTF16.CodeUnit, _ u1: UTF16.CodeUnit) {
    _ensureUniqueNative(
      minimumCapacity: count + 2,
      minimumByteWidth: 2)
    var nativeSelf = self._asNative
    nativeSelf._appendInPlace(u0, u1)
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
  internal
  var _unmanagedContiguous : UnsafeString? {
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
    return self._wholeString._unmanagedContiguous?[self._encodedOffsetRange]
  }
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
      return (self as? Substring)?._unmanagedContiguous
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

  @_inlineable // FIXME(sil-serialize-all)
  public static func < <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    let lhsContigOpt = lhs._unmanagedContiguous
    let rhsContigOpt = rhs._unmanagedContiguous
    if _slowPath(lhsContigOpt == nil || rhsContigOpt == nil) {
      return lhs._ephemeralString._compareString(rhs._ephemeralString) < 0
    }
    let result = lhsContigOpt._unsafelyUnwrappedUnchecked.less(
      than: rhsContigOpt._unsafelyUnwrappedUnchecked)
    _fixLifetime(lhs)
    _fixLifetime(rhs)
    return result
  }
}
extension String : Equatable, Comparable {
  // FIXME: Why do I need this? If I drop it, I get "ambiguous use of operator"
  @_inlineable // FIXME(sil-serialize-all)
  public static func ==(lhs: String, rhs: String) -> Bool {
    // Bitwise equality implies string equality
    if _slowPath(lhs._guts._bitwiseEqualTo(rhs._guts)) {
      return true
    }

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

  // FIXME: Why do I need this? If I drop it, I get "ambiguous use of operator"
  @_inlineable // FIXME(sil-serialize-all)
  public static func < (lhs: String, rhs: String) -> Bool {
    let lhsContigOpt = lhs._unmanagedContiguous
    let rhsContigOpt = rhs._unmanagedContiguous
    if _slowPath(lhsContigOpt == nil || rhsContigOpt == nil) {
      return lhs._ephemeralString._compareString(rhs._ephemeralString) < 0
    }
    let result = lhsContigOpt._unsafelyUnwrappedUnchecked.less(
      than: rhsContigOpt._unsafelyUnwrappedUnchecked)
    _fixLifetime(lhs)
    _fixLifetime(rhs)
    return result
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
  internal func compare(to other: UnsafeString) -> Int {
    if self.isASCII && other.isASCII {
      if self.baseAddress == other.baseAddress {
        return self.count - other.count
      }
      let cmp = _swift_stdlib_memcmp(
        self.baseAddress, other.baseAddress,
        Swift.min(self.count, other.count))
      if cmp == 0 {
        return self.count - other.count
      }
      return Int(truncatingIfNeeded: cmp)
    }
    return _compareDeterministicUnicodeCollation(other)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  internal func equal(to other: UnsafeString) -> Bool {
    return compare(to: other) == 0
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  internal func less(than other: UnsafeString) -> Bool {
    return compare(to: other) < 0
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

  // TODO: comparison too!
}

// Hashing, originally from StringHashable.swift
#if arch(i386) || arch(arm)
    private let stringHashOffset = Int(bitPattern: 0x88dd_cc21)
#else
    private let stringHashOffset = Int(bitPattern: 0x429b_1266_88dd_cc21)
#endif // arch(i386) || arch(arm)
extension UnsafeString {
  @_versioned
  @inline(never) // Hide the CF dependency
  internal func _computeHashValue() -> Int {
#if _runtime(_ObjC)
    if self.isASCII {
      return stringHashOffset ^ _stdlib_CFStringHashCString(
                              OpaquePointer(self.baseAddress), self.count)
    }
    let stackAllocated = _NSContiguousString(_StringGuts(self))
    return stringHashOffset ^ stackAllocated._unsafeWithNotEscapedSelfPointer {
      return _stdlib_NSStringHashValuePointer($0, false)
    }
#else
    if self.isASCII {
      return Unicode.hashASCII(UnsafeBufferPointer(
        start: self.baseAddress,
        count: self.count))
    }
    return Unicode.hashUTF16(self.utf16Buffer)
#endif // _runtime(_ObjC)
  }
}
extension _StringGuts {
  @_versioned
  @inline(never) // Hide the CF dependency
  internal func _computeHashValue() -> Int {
    // Mix random bits into NSString's hash so that clients don't rely on
    // Swift.String.hashValue and NSString.hash being the same.
    // If we have a contiguous string then we can use the stack optimization.
    let contigOpt = self._unmanagedContiguous
    if _slowPath(contigOpt == nil) {
      let cocoaString = unsafeBitCast(
        String(self)._bridgeToObjectiveCImpl(), to: _NSStringCore.self)
      return stringHashOffset ^ _stdlib_NSStringHashValue(
        cocoaString, self.isASCII)
    }
    let result = contigOpt._unsafelyUnwrappedUnchecked._computeHashValue()
    _fixLifetime(self)
    return result
  }
}

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
  @inline(__always)
  @_inlineable
  public func _bitwiseEqualTo(_ other: _StringGuts) -> Bool {
    return self._objectBitPattern == other._objectBitPattern
      && self._otherBits == other._otherBits
  }
}

extension _StringGuts {
  @_inlineable
  @_versioned
  internal
  func character(at i: String.Index) -> Character {
    let contigOpt = self._unmanagedContiguous
    if _slowPath(contigOpt == nil) {
      return getOpaque().character(at: i)
    }
    return contigOpt._unsafelyUnwrappedUnchecked.character(at: i)
  }

  @_inlineable
  @_versioned
  internal
  func characterIndex(after i: String.Index) -> String.Index {
    let contigOpt = self._unmanagedContiguous
    if _slowPath(contigOpt == nil) {
      return getOpaque().characterIndex(after: i)
    }
    return contigOpt._unsafelyUnwrappedUnchecked.characterIndex(after: i)
  }

  @_inlineable
  @_versioned
  internal
  func characterIndex(before i: String.Index) -> String.Index {
    let contigOpt = self._unmanagedContiguous
    if _slowPath(contigOpt == nil) {
      return getOpaque().characterIndex(before: i)
    }
    return contigOpt._unsafelyUnwrappedUnchecked.characterIndex(before: i)
  }

  // @_inlineable
  // @_versioned
  // internal
  // func characterIndex(
  //   _ i: String.Index, offsetBy n: String.IndexDistance
  // ) -> String.Index {
  //   let contigOpt = self._unmanagedContiguous
  //   if _slowPath(contigOpt == nil) {
  //     return String.CharacterView(self).index(i, offsetBy: n) // TODO: opaque string
  //   }
  //   return contigOpt._unsafelyUnwrappedUnchecked.characterIndex(i, offsetBy: n)
  // }
}

extension UnsafeString {
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
    _precondition(i.encodedOffset >= startIndex,
      "cannot increment invalid index")
    _precondition(i.encodedOffset < endIndex,
      "cannot increment beyond endIndex")

    let stride = self.characterStride(from: i)
    let newOffset = i.encodedOffset + stride
    _sanityCheck(newOffset <= endIndex, "walked off the end")

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
    if startOffset == endOffset {
      return 0
    }

    // Last code unit means final grapheme length of 1
    if startOffset == endOffset - 1 {
      return 1
    }

    // Grapheme breaking is much simpler if known ASCII
    if self.isASCII{
      _onFastPath() // Please aggressively inline

      // We already checked bounds above, use pointer
      let asciiBuffer = self.asciiStart

      // With the exception of CR-LF, ASCII graphemes are single-scalar. Check
      // for that one exception.
      if _slowPath(
        asciiBuffer[startOffset] == _CR &&
        asciiBuffer[startOffset+1] == _LF
      ) {
        return 2
      }

      return 1
    }

    // Perform a quick single-code-unit grapheme check.
    let utf16Buffer = self.utf16Start
    if _fastPath(String.CharacterView._quickCheckGraphemeBreakBetween(
        utf16Buffer[startOffset],
        utf16Buffer[startOffset+1])
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

extension OpaqueCocoaString {
  var characterView: String.CharacterView {
    return String.CharacterView(_StringGuts(self))
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
  init(_ unsafeStr: UnsafeString) {
    if _fastPath(unsafeStr.count <= 4) {
      let b = _UIntBuffer<UInt64, Unicode.UTF16.CodeUnit>(unsafeStr)
      if _fastPath(Int64(truncatingIfNeeded: b._storage) >= 0) {
        _representation = .smallUTF16(
          Builtin.trunc_Int64_Int63(b._storage._value))
        return
      }
    }
    self = Character(_largeRepresentationString: String(_StringGuts(unsafeStr)))
  }
}


