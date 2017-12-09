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
  var _storage: (_StringObject, UInt)

  @_inlineable
  public
  var _object: _StringObject {
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
  @inline(__always)
  public
  init(object: _StringObject, otherBits: UInt) {
    self._storage.0 = object
    self._storage.1 = otherBits
    _invariantCheck()
  }
}

extension _StringGuts {
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    if _object.isNative {
      _sanityCheck(UInt(_object.nativeRawStorage.count) == self._otherBits)
    } else if _object.isUnmanaged {
    } else if _object.isCocoa {
      if _object.isContiguous {
        _sanityCheck(_otherBits != 0) // TODO: in ABI's address space
      } else {
        _sanityCheck(_otherBits == 0)
      }
    } else if _object.isSmall {
    } else {
      fatalError("Unimplemented string form")
    }
#endif
  }

  @_inlineable
  @inline(__always)
  public // @testable
  mutating func isUniqueNative() -> Bool {
    guard _isNative else { return false }
    // Note that the isUnique test must be in a separate statement;
    // `isNative && _isUnique` always evaluates to false in debug builds,
    // because SILGen keeps the self reference in `isNative` alive for the
    // duration of the expression.

    // Note that we have to perform this operation here, and not as a (even
    // mutating) method on our _StringObject to avoid all chances of a semantic
    // copy.
    //
    // FIXME: Super hacky. Is there a better way?
    defer { _fixLifetime(self) }
    var bitPattern = _object.payloadBits
    return _isUnique_native(&bitPattern)
  }

}

extension _StringGuts {
  @_inlineable
  public // @testable
  var isASCII: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isContiguousASCII
  }

  @_inlineable
  public // @testable
  var _isNative: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isNative
  }

  @_inlineable
  public // @testable
  var _isCocoa: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isCocoa
  }

  @_inlineable
  public // @testable
  var _isUnmanaged: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isUnmanaged
  }

  @_inlineable
  public // @testable
  var _isSmall: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isSmall
  }

  @_inlineable
  public // @testable
  var _owner: AnyObject? {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.owner
  }

  @_inlineable
  public // @testable
  var isSingleByte: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isSingleByte
  }

  @_versioned
  @_inlineable
  internal
  var _isEmptyLiteral: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isEmptyLiteral
  }

  @_inlineable
  public // @testable
  var byteWidth: Int {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.byteWidth
  }

  @_versioned
  @_inlineable
  internal
  var _nativeCount: Int {
    @inline(__always) get {
      _sanityCheck(_object.isNative)
      return Int(bitPattern: _otherBits)
    }
    @inline(__always) set {
      _sanityCheck(_object.isNative)
      _sanityCheck(newValue >= 0)
      _otherBits = UInt(bitPattern: newValue)
    }
  }

  @_versioned
  @inline(__always)
  internal
  init<CodeUnit>(_ storage: _SwiftStringStorage<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(storage.count >= 0)
    self.init(
      object: _StringObject(storage),
      otherBits: UInt(bitPattern: storage.count))
  }

  //
  // HACK HACK HACK: Work around for ARC :-(
  //  @inline(never) // Hide CF dependency
  @_versioned
  internal static func getCocoaLength(_unsafeBitPattern: UInt) -> Int {
    return _stdlib_binary_CFStringGetLength(
      Builtin.reinterpretCast(_unsafeBitPattern))
  }

  @_versioned
  @_inlineable
  var _cocoaCount: Int {
    @inline(__always)
    get {
      _sanityCheck(_object.isCocoa)
      defer { _fixLifetime(self) }
      return _StringGuts.getCocoaLength(_unsafeBitPattern: _object.payloadBits)
      // _stdlib_binary_CFStringGetLength(_object.asCocoaObject)
    }
  }

  @_versioned
  @_inlineable
  var _cocoaRawStart: UnsafeRawPointer {
    @inline(__always)
    get {
      _sanityCheck(_object.isContiguousCocoa)
      _sanityCheck(Int(bitPattern: _otherBits) >= 1) // TODO: ABI's min address
      return UnsafeRawPointer(
        bitPattern: _otherBits
      )._unsafelyUnwrappedUnchecked
    }
  }

  @_versioned
  @_inlineable
  func _asContiguousCocoa<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _UnmanagedString<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_object.isContiguousCocoa)
    _sanityCheck(CodeUnit.bitWidth == _object.bitWidth)
    let start = _cocoaRawStart.assumingMemoryBound(to: CodeUnit.self)
    return _UnmanagedString(start: start, count: _cocoaCount)
  }

  @_inlineable
  @inline(__always)
  public // @testable
  init() {
    self.init(object: _StringObject(), otherBits: 0)
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
      object: _StringObject(
        cocoaObject: s, isSingleByte: isSingleByte, isContiguous: start != nil),
      otherBits: UInt(bitPattern: start))
    if start == nil {
      _sanityCheck(_object.isOpaque)
    } else {
      _sanityCheck(_object.isContiguous)
    }
  }

  @_versioned
  @_inlineable
  internal var _unmanagedCount: Int {
    @inline(__always) get {
      _sanityCheck(_object.isUnmanaged)
      return Int(bitPattern: _otherBits)
    }
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  func _asUnmanaged<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _UnmanagedString<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_object.isUnmanaged)
    _sanityCheck(CodeUnit.bitWidth == _object.bitWidth)
    let start = _object.asUnmanagedRawStart.assumingMemoryBound(to: CodeUnit.self)
    let count = _unmanagedCount
    _sanityCheck(count >= 0)
    return _UnmanagedString(start: start, count: count)
  }

  @_versioned
  @_inlineable
  init<CodeUnit>(_ s: _UnmanagedString<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(s.count >= 0)
    self.init(
      object: _StringObject(unmanaged: s.start),
      otherBits: UInt(bitPattern: s.count))
    _sanityCheck(_object.isUnmanaged)
    _sanityCheck(_object.asUnmanagedRawStart == s.rawStart)
    _sanityCheck(_unmanagedCount == s.count)
  }

  //
  // NOTE: For now, small strings are tagged cocoa strings
  //
  @_versioned
  @_inlineable
  internal var _taggedCocoaCount: Int {
    _sanityCheck(_object.isSmall)
    return Int(bitPattern: _object.payloadBits)
  }

  @_versioned
  @_inlineable
  internal var _taggedCocoaObject: _CocoaString {
    @inline(__always) get {
      _sanityCheck(_object.isSmall)
      return Builtin.reinterpretCast(_otherBits)
    }
  }

  @_versioned
  @inline(never) // Hide CF dependency
  internal init(_taggedCocoaObject object: _CocoaString) {
    _sanityCheck(_isObjCTaggedPointer(object))
    let count = _stdlib_binary_CFStringGetLength(object)
    self.init(
      object: _StringObject(
        smallStringPayload: UInt(count), isSingleByte: false),
      otherBits: Builtin.reinterpretCast(object))
    _sanityCheck(_object.isSmall)
  }
}

extension _StringGuts {
  @_versioned
  @_inlineable
  internal
  var _unmanagedASCIIView: _UnmanagedString<UInt8> {
    _sanityCheck(_object.isContiguousASCII)
    if _object.isUnmanaged {
      return _asUnmanaged()
    } else if _object.isNative {
      return _object.nativeStorage(of: UInt8.self).unmanagedView
    } else {
      _sanityCheck(_object.isContiguousCocoa)
      return _asContiguousCocoa(of: UInt8.self)
    }
  }

  @_versioned
  @_inlineable
  internal
  var _unmanagedUTF16View: _UnmanagedString<UTF16.CodeUnit> {
    _sanityCheck(_object.isContiguousUTF16)
    if _object.isUnmanaged {
      return _asUnmanaged()
    } else if _object.isNative {
      return _object.nativeStorage(of: UTF16.CodeUnit.self).unmanagedView
    } else if _object.isCocoa {
      return _asContiguousCocoa(of: UTF16.CodeUnit.self)
    } else {
      fatalError("Small strings aren't contiguous")
    }
  }
}

extension _StringGuts {
  /// Return an NSString instance containing a slice of this string.
  /// The returned object may contain unmanaged pointers into the
  /// storage of this string; you are responsible for ensuring that
  /// it will not outlive `self`.
  @_versioned
  @_inlineable
  internal
  func _ephemeralCocoaString() -> _CocoaString {
    if _object.isNative {
      return _object.asNativeObject
    }
    if _object.isCocoa {
      return _object.asCocoaObject
    }
    if _object.isSmall {
      return _taggedCocoaObject
    }
    _sanityCheck(_object.isUnmanaged)
    if _object.isSingleByte {
      return _NSContiguousString(_StringGuts(_asUnmanaged(of: UInt8.self)))
    }

    return _NSContiguousString(
      _StringGuts(_asUnmanaged(of: UTF16.CodeUnit.self)))
  }

  @_versioned
  @_inlineable
  internal
  var _isOpaque: Bool {
    @inline(__always)
    get { return _object.isOpaque }
  }

  @_versioned
  @_inlineable
  internal
  var _isContiguous: Bool {
    @inline(__always)
    get { return _object.isContiguous }
  }


  /// Return an NSString instance containing a slice of this string.
  /// The returned object may contain unmanaged pointers into the
  /// storage of this string; you are responsible for ensuring that
  /// it will not outlive `self`.
  @_versioned
  @_inlineable
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
    if _object.isNative {
      return _object.nativeRawStorage
    } 
    if _object.isCocoa {
      return _object.asCocoaObject
    } 
    if _object.isSmall {
      return _taggedCocoaObject
    }

    return nil
  }
}

extension _StringGuts {
  @inline(never)
  @_versioned
  internal func _asOpaque() -> _UnmanagedOpaqueString {
    if _object.isSmall {
      return _UnmanagedOpaqueString(
        _taggedCocoaObject, count: _taggedCocoaCount)
    }

    _sanityCheck(_object.isNoncontiguousCocoa)
    return _UnmanagedOpaqueString(_object.asCocoaObject, count: _cocoaCount)
  }
}

// Internal hex dumping function. Useful because `print` is implemented in the
// stdlib through a series of very high level String calls, resulting in
// infinite recursion.
@_versioned
internal func internalDumpHexImpl(_ x: UInt, newline: Bool) {
  _swift_stdlib_print_hex(x, newline ? 1 : 0)
}
@_versioned
internal func internalDumpHex(_ x: _BuiltinNativeObject, newline: Bool) {
  internalDumpHexImpl(Builtin.reinterpretCast(x), newline: newline)
}
@_versioned
internal func internalDumpHex(_ x: UInt, newline: Bool = true) {
  internalDumpHexImpl(x, newline: newline)
}
@_versioned
internal func internalDumpHex(_ x: AnyObject, newline: Bool = true) {
  internalDumpHexImpl(Builtin.reinterpretCast(x), newline: newline)
}
@_versioned
internal func internalDumpHex(_ x: UnsafeRawPointer?, newline: Bool = true) {
  internalDumpHexImpl(Builtin.reinterpretCast(x), newline: newline)
}
@_versioned
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
    if _object.isUnmanaged {
      return _LegacyStringCore(
        baseAddress: UnsafeMutableRawPointer(mutating: _object.asUnmanagedRawStart),
        count: _unmanagedCount,
        elementShift: _object.isSingleByte ? 0 : 1,
        hasCocoaBuffer: false,
        owner: nil)
    } else if _object.isNative {
      return makeCocoaLegacyStringCore(_cocoaString: _object.nativeRawStorage)
    } else if _object.isCocoa {
      return makeCocoaLegacyStringCore(_cocoaString: _object.asCocoaObject)
    } else {
      _sanityCheck(_object.isSmall)
      return makeCocoaLegacyStringCore(_cocoaString: _taggedCocoaObject)
    }
  }

  @_versioned
  init(_ legacyCore: _LegacyStringCore) {
    if UnsafeRawPointer(legacyCore._baseAddress) == _emptyStringBase {
      self.init()
      return
    }

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
    internalDumpHex(_object.rawBits, newline: false)
    print(" ", terminator: "")
    internalDumpHex(_otherBits, newline: false)
    print(": ", terminator: "")
    if _object.isNative {
      let storage = _object.nativeRawStorage
      print("native ", terminator: "")
      internalDumpHex(storage, newline: false)
      print(" start: ", terminator: "")
      internalDumpHex(storage.rawStart, newline: false)
      print(" count: ", terminator: "")
      print(storage.count, terminator: "")
      print("/", terminator: "")
      print(storage.capacity, terminator: "")
    } else if _object.isCocoa {
      print("cocoa ", terminator: "")
      internalDumpHex(_object.asCocoaObject, newline: false)
      print(" start: ", terminator: "")
      if _object.isContiguous {
        internalDumpHex(_cocoaRawStart, newline: false)
      } else {
        print("<opaque>", terminator: "")
      }
      print(" count: ", terminator: "")
      print(_cocoaCount, terminator: "")
    } else if _object.isUnmanaged {
      print("unmanaged ", terminator: "")
      internalDumpHex(_object.asUnmanagedRawStart, newline: false)
      print(" count: ", terminator: "")
      print(_unmanagedCount, terminator: "")
    } else if _object.isSmall {
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
    if _fastPath(_object.isNative && CodeUnit.bitWidth == _object.bitWidth) {
      return _object.nativeStorage()
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
    if range.isEmpty { return _StringGuts() }
    if range == 0..<count { return self }
    switch (isASCII, _object.isUnmanaged) {
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

  @_versioned
  @_inlineable
  internal mutating func allocationParametersForMutableStorage<CodeUnit>(
    of type: CodeUnit.Type,
    unusedCapacity: Int
  ) -> (count: Int, capacity: Int)?
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    if _slowPath(!_object.isNative) {
      return (self.count, count + unusedCapacity)
    }
    unowned(unsafe) let storage = _object.nativeRawStorage
    defer { _fixLifetime(self) }
    if _slowPath(storage.unusedCapacity < unusedCapacity) {
      return (
        storage.count,
        Swift.max(
          _growArrayCapacity(storage.capacity),
          count + unusedCapacity))
    }
    if _fastPath(_object.bitWidth == CodeUnit.bitWidth) {
      if _fastPath(isUniqueNative()) {
        return nil
      }
    }
    return (storage.count, storage.capacity)
  }

  // Convert ourselves (if needed) to a native string with the specified storage
  // parameters and call `body` on the resulting native storage.
  @_versioned
  @_inlineable
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
      unowned(unsafe) let storage = _object.nativeStorage(of: CodeUnit.self)
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

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  mutating func withMutableASCIIStorage<R>(
    unusedCapacity: Int,
    _ body: (Unmanaged<_ASCIIStringStorage>) -> R
  ) -> R {
    return self.withMutableStorage(
      of: UInt8.self, unusedCapacity: unusedCapacity, body)
  }

  @_versioned
  @_inlineable
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
    if _slowPath(_object.isSmall) {
      return _taggedCocoaCount
    } else if _slowPath(_object.isCocoa) {
      return _cocoaCount
    }
    _sanityCheck(Int(self._otherBits) >= 0)
    return Int(bitPattern: self._otherBits)
  }

  @_inlineable
  public // @testable
  var capacity: Int {
    if _fastPath(_object.isNative) {
      return _object.nativeRawStorage.capacity
    }
    return 0
  }

  /// Get the UTF-16 code unit stored at the specified position in this string.
  @_inlineable // FIXME(sil-serialize-all)
  public // @testable
  subscript(position: Int) -> UTF16.CodeUnit {
    if _slowPath(_isOpaque) {
      return _asOpaque()[position]
    }

    if isASCII {
      return _unmanagedASCIIView[position]
    }

    return _unmanagedUTF16View[position]
  }

  // Copy code units from a slice of this string into a buffer.
  @_versioned
  internal func _copy<CodeUnit>(
    range: Range<Int>,
    into dest: UnsafeMutableBufferPointer<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(CodeUnit.bitWidth == 8 || CodeUnit.bitWidth == 16)
    _sanityCheck(dest.count >= range.count)
    if _slowPath(_isOpaque) {
      _asOpaque()[range]._copy(into: dest)
      return
    }

    if isASCII {
      _unmanagedASCIIView[range]._copy(into: dest)
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
      if _fastPath(_object.bitWidth == CodeUnit.bitWidth) {
        if _fastPath(_object.nativeRawStorage.capacity >= capacity) {
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

  @_versioned
  @_inlineable
  internal
  mutating func append(_ other: _UnmanagedASCIIString) {
    guard other.count > 0 else { return  }
    if _object.isSingleByte {
      withMutableASCIIStorage(unusedCapacity: other.count) { storage in
        storage._value._appendInPlace(other)
      }
    } else {
      withMutableUTF16Storage(unusedCapacity: other.count) { storage in
        storage._value._appendInPlace(other)
      }
    }
  }

  @_versioned
  @_inlineable
  internal
  mutating func append(_ other: _UnmanagedUTF16String) {
    guard other.count > 0 else { return  }
    withMutableUTF16Storage(unusedCapacity: other.count) { storage in
      storage._value._appendInPlace(other)
    }
  }

  @_versioned
  @_inlineable
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
    if _object.isEmptyLiteral {
      self = other
      return
    }
    if _slowPath(other._isOpaque) {
      self.append(other._asOpaque())
      return
    }

    defer { _fixLifetime(other) }
    if other.isASCII {
      self.append(other._unmanagedASCIIView)
    } else {
      self.append(other._unmanagedUTF16View)
    }
  }

  @_inlineable
  public // TODO(StringGuts): for testing only
  mutating func append(_ other: _StringGuts, range: Range<Int>) {
    _sanityCheck(range.lowerBound >= 0 && range.upperBound <= other.count)
    guard range.count > 0 else { return }
    if _object.isEmptyLiteral && range.count == other.count {
      self = other
      return
    }
    if _slowPath(other._isOpaque) {
      self.append(other._asOpaque()[range])
      return
    }

    defer { _fixLifetime(other) }
    if other.isASCII {
      self.append(other._unmanagedASCIIView[range])
    } else {
      self.append(other._unmanagedUTF16View[range])
    }
  }

  public // @testable
  mutating func append<C : Collection>(contentsOf other: C)
  where C.Element == UTF16.CodeUnit {
    if _object.isSingleByte && !other.contains(where: { $0 > 0x7f }) {
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
      unowned(unsafe) let storage = _object.nativeStorage(of: CodeUnit.self)
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

// UnicodeScalarView operations
extension _StringGuts {
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  func _unicodeScalarWidth(startingAt offset: Int) -> Int {
    if _slowPath(_isOpaque) {
      return _asOpaque()._unicodeScalarWidth(startingAt: offset)
    }
    if isASCII { return 1 }
    return _unmanagedUTF16View._unicodeScalarWidth(startingAt: offset)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  func _unicodeScalarWidth(endingAt offset: Int) -> Int {
    if _slowPath(_isOpaque) {
      return _asOpaque()._unicodeScalarWidth(endingAt: offset)
    }
    if isASCII { return 1 }
    return _unmanagedUTF16View._unicodeScalarWidth(endingAt: offset)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  func _decodeUnicodeScalar(startingAt offset: Int) -> UnicodeDecodingResult {
    if _slowPath(_isOpaque) {
      return _asOpaque()._decodeUnicodeScalar(startingAt: offset)
    }
    if isASCII {
      return _unmanagedASCIIView._decodeUnicodeScalar(startingAt: offset)
    }
    return _unmanagedUTF16View._decodeUnicodeScalar(startingAt: offset)
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
  @_versioned
  @_inlineable
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

  @_versioned
  @_inlineable
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

  @_versioned
  @_inlineable
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
  @_versioned
  @_inlineable
  internal
  func characterStride(from i: String.Index) -> Int {
    // TODO: should _fastPath the case somehow
    if case .character(let _stride) = i._cache {
      return Int(truncatingIfNeeded: _stride)
    } else {
      return self._measureExtendedGraphemeClusterForward(from: i.encodedOffset)
    }
  }

  @_versioned
  @_inlineable
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
  @_versioned
  @_inlineable
  internal
  init(_singleCodeUnit cu: UInt16) {
    _representation = .smallUTF16(
      Builtin.zext_Int16_Int63(Builtin.reinterpretCast(cu)))
  }

  @_versioned
  @_inlineable
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
