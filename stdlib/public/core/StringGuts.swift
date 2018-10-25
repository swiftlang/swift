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
  public // FIXME for testing only
  var _object: _StringObject

  public // FIXME for testing only
  var _otherBits: UInt // (Mostly) count or inline storage

  @inlinable
  @inline(__always)
  public
  init(object: _StringObject, otherBits: UInt) {
    self._object = object
    self._otherBits = otherBits
    _invariantCheck()
  }

  public typealias _RawBitPattern = (_StringObject._RawBitPattern, UInt)

  @inlinable
  internal var rawBits: _RawBitPattern {
    @inline(__always)
    get {
      return (_object.rawBits, _otherBits)
    }
  }

  init(rawBits: _RawBitPattern) {
    self.init(
      object: _StringObject(noReallyHereAreTheRawBits: rawBits.0),
      otherBits: rawBits.1)
  }
}

extension _StringGuts {
  @inlinable // FIXME(sil-serialize-all)
  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    _object._invariantCheck()
    if _object.isNative {
      _sanityCheck(UInt(_object.nativeRawStorage.count) == self._otherBits)
    } else if _object.isUnmanaged {
    } else if _object.isCocoa {
      if _object.isContiguous {
        _sanityCheck(_isValidAddress(_otherBits))
      } else {
        _sanityCheck(_otherBits == 0)
      }
    } else if _object.isSmall {
      _smallUTF8String._invariantCheck()
    } else {
      fatalError("Unimplemented string form")
    }

#if arch(i386) || arch(arm)
  _sanityCheck(MemoryLayout<String>.size == 12, """
    the runtime is depending on this, update Reflection.mm and \
    this if you change it
    """)
#else
  _sanityCheck(MemoryLayout<String>.size == 16, """
    the runtime is depending on this, update Reflection.mm and \
    this if you change it
    """)
#endif
#endif // INTERNAL_CHECKS_ENABLED
  }

  @inlinable
  @inline(__always)
  internal mutating func _isUniqueNative() -> Bool {
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
    var bitPattern = _object.referenceBits
    return _isUnique_native(&bitPattern)
  }
}

extension _StringGuts {
  @inlinable
  internal var isASCII: Bool {
    @inline(__always) get { return _object.isContiguousASCII }
  }

  @inlinable
  internal
  var _isASCIIOrSmallASCII: Bool {
    @inline(__always) get {
      return isASCII || _isSmall && _smallUTF8String.isASCII
    }
  }

  @inlinable
  internal var _isNative: Bool {
    return _object.isNative
  }

  @inlinable
  internal var _isCocoa: Bool {
    return _object.isCocoa
  }

  @inlinable
  internal var _isUnmanaged: Bool {
    return _object.isUnmanaged
  }

  @inlinable
  internal var _isSmall: Bool {
    return _object.isSmall
  }

  @inlinable
  internal var _owner: AnyObject? {
    return _object.owner
  }

  @inlinable
  internal var isSingleByte: Bool {
    // FIXME: Currently used to sometimes mean contiguous ASCII
    return _object.isSingleByte
  }

  @inlinable
  internal
  var _isEmptySingleton: Bool {
    return _object.isEmptySingleton
  }

  @inlinable
  internal var byteWidth: Int {
    return _object.byteWidth
  }

  @inlinable
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

  // TODO(SSO): consider a small-checking variant
  @inlinable
  @inline(__always)
  internal
  init<CodeUnit>(_large storage: _SwiftStringStorage<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(storage.count >= 0)
    self.init(
      object: _StringObject(storage),
      otherBits: UInt(bitPattern: storage.count))
  }
}

extension _StringGuts {
  @inlinable
  @inline(__always)
  internal init() {
    self.init(object: _StringObject(), otherBits: 0)
    _invariantCheck()
  }
}

#if _runtime(_ObjC)
extension _StringGuts {
  //
  // FIXME(TODO: JIRA): HACK HACK HACK: Work around for ARC :-(
  //
  @usableFromInline
  @_effects(readonly)
  internal static func getCocoaLength(_unsafeBitPattern: UInt) -> Int {
    return _stdlib_binary_CFStringGetLength(
      Builtin.reinterpretCast(_unsafeBitPattern))
  }

  @inlinable
  var _cocoaCount: Int {
    @inline(__always)
    get {
      _sanityCheck(_object.isCocoa)
      defer { _fixLifetime(self) }
      return _StringGuts.getCocoaLength(
        _unsafeBitPattern: _object.referenceBits)
      // _stdlib_binary_CFStringGetLength(_object.asCocoaObject)
    }
  }

  @inlinable
  var _cocoaRawStart: UnsafeRawPointer {
    @inline(__always)
    get {
      _sanityCheck(_object.isContiguousCocoa)
      _sanityCheck(_isValidAddress(_otherBits))
      return UnsafeRawPointer(
        bitPattern: _otherBits
      )._unsafelyUnwrappedUnchecked
    }
  }

  @inlinable
  func _asContiguousCocoa<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _UnmanagedString<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_object.isContiguousCocoa)
    _sanityCheck(CodeUnit.bitWidth == _object.bitWidth)
    let start = _cocoaRawStart.assumingMemoryBound(to: CodeUnit.self)
    return _UnmanagedString(start: start, count: _cocoaCount)
  }

  // TODO(SSO): consider a small-checking variant
  @usableFromInline
  internal
  init(
    _largeNonTaggedCocoaObject s: _CocoaString,
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
        cocoaObject: s,
        isSingleByte: isSingleByte,
        isContiguous: start != nil),
      otherBits: UInt(bitPattern: start))
    if start == nil {
      _sanityCheck(_object.isOpaque)
    } else {
      _sanityCheck(_object.isContiguous)
    }
  }
}
#else // !_runtime(_ObjC)
extension _StringGuts {
  // TODO(SSO): consider a small-checking variant
  @inline(never)
  @usableFromInline
  internal
  init<S: _OpaqueString>(_large opaqueString: S) {
    self.init(
      object: _StringObject(opaqueString: opaqueString),
      otherBits: UInt(bitPattern: opaqueString.length))
  }
}
#endif // _runtime(_ObjC)

extension _StringGuts {
  @inlinable
  internal var _unmanagedRawStart: UnsafeRawPointer {
    @inline(__always) get {
      _sanityCheck(_object.isUnmanaged)
      return _object.asUnmanagedRawStart
    }
  }

  @inlinable
  internal var _unmanagedCount: Int {
    @inline(__always) get {
      _sanityCheck(_object.isUnmanaged)
      return Int(bitPattern: _otherBits)
    }
  }

  @inlinable
  @inline(__always)
  internal
  func _asUnmanaged<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _UnmanagedString<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_object.isUnmanaged)
    _sanityCheck(CodeUnit.bitWidth == _object.bitWidth)
    let start = _unmanagedRawStart.assumingMemoryBound(to: CodeUnit.self)
    let count = _unmanagedCount
    _sanityCheck(count >= 0)
    return _UnmanagedString(start: start, count: count)
  }

  // TODO(SSO): consider a small-checking variant
  @inlinable
  init<CodeUnit>(_large s: _UnmanagedString<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(s.count >= 0)
    self.init(
      object: _StringObject(unmanaged: s.start),
      otherBits: UInt(bitPattern: s.count))
    _sanityCheck(_object.isUnmanaged)
    _sanityCheck(_unmanagedRawStart == s.rawStart)
    _sanityCheck(_unmanagedCount == s.count)
    _invariantCheck()
  }
}

// Small strings
extension _StringGuts {
  @inlinable
  internal var _smallUTF8Count: Int {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
      return _object.smallUTF8Count
#endif
    }
  }

  @inlinable
  internal var _smallUTF8String: _SmallUTF8String {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
      return _SmallUTF8String(
        _rawBits: (low: _otherBits, high: _object.asSmallUTF8SecondWord))
#endif
    }
  }

  @inlinable
  @inline(__always)
  internal init(_ small: _SmallUTF8String) {
#if arch(i386) || arch(arm)
      unsupportedOn32bit()
#else
    self.init(
      object: _StringObject(_smallUTF8SecondWord: small._rawBits.high),
      otherBits: small._rawBits.low)
#endif
  }
}

extension _StringGuts {
  @inlinable
  internal
  var _unmanagedASCIIView: _UnmanagedString<UInt8> {
    @_effects(readonly)
    get {
      _sanityCheck(_object.isContiguousASCII)
      if _object.isUnmanaged {
        return _asUnmanaged()
      } else if _object.isNative {
        return _object.nativeStorage(of: UInt8.self).unmanagedView
      } else {
#if _runtime(_ObjC)
        _sanityCheck(_object.isContiguousCocoa)
        return _asContiguousCocoa(of: UInt8.self)
#else
        Builtin.unreachable()
#endif
      }
    }
  }

  @inlinable
  internal
  var _unmanagedUTF16View: _UnmanagedString<UTF16.CodeUnit> {
    @_effects(readonly)
    get {
      _sanityCheck(_object.isContiguousUTF16)
      if _object.isUnmanaged {
        return _asUnmanaged()
      } else if _object.isNative {
        return _object.nativeStorage(of: UTF16.CodeUnit.self).unmanagedView
      } else {
#if _runtime(_ObjC)
        _sanityCheck(_object.isContiguousCocoa)
        return _asContiguousCocoa(of: UTF16.CodeUnit.self)
#else
        Builtin.unreachable()
#endif
      }
    }
  }
}

extension _StringGuts {
  @inlinable
  internal
  var _isOpaque: Bool {
    @inline(__always)
    get { return _object.isOpaque }
  }

  @inlinable
  internal
  var _isContiguous: Bool {
    @inline(__always)
    get { return _object.isContiguous }
  }
}

#if _runtime(_ObjC)
extension _StringGuts {
  @usableFromInline
  var _underlyingCocoaString: _CocoaString? {
    if _object.isNative {
      return _object.nativeRawStorage
    }
    if _object.isCocoa {
      return _object.asCocoaObject
    }
    return nil
  }
}
#endif

extension _StringGuts {
  /// Return the object identifier for the reference counted heap object
  /// referred to by this string (if any). This is useful for testing allocation
  /// behavior.
  @usableFromInline
  internal var _objectIdentifier: ObjectIdentifier? {
    if _object.isNative {
      return ObjectIdentifier(_object.nativeRawStorage)
    }
#if _runtime(_ObjC)
    if _object.isCocoa {
      return ObjectIdentifier(_object.asCocoaObject)
    }
#endif
    return nil
  }
}

extension _StringGuts {
  // @opaque
  internal func _asOpaque() -> _UnmanagedOpaqueString {
#if _runtime(_ObjC)
    if _object.isSmall {
      fatalError("Invariant violated: opaque small strings")
    }
    _sanityCheck(_object.isNoncontiguousCocoa)
    return _UnmanagedOpaqueString(_object.asCocoaObject, count: _cocoaCount)
#else
    _sanityCheck(_object.isOpaque)
    return _UnmanagedOpaqueString(_object.asOpaqueObject, count: _opaqueCount)
#endif
  }

  @usableFromInline
  internal var _opaqueCount: Int {
    fatalError("TODO: non-cocoa opaque string support")
  }
}

extension _StringGuts {
  internal
  func _dump() {
#if INTERNAL_CHECKS_ENABLED
    func printHex<U: UnsignedInteger>(_ uint: U, newline: Bool = true) {
      print(String(uint, radix: 16), terminator: newline ? "\n" : "")
    }
    func fromAny(_ x: AnyObject) -> UInt {
      return Builtin.reinterpretCast(x)
    }
    func fromPtr(_ x: UnsafeMutableRawPointer) -> UInt {
      return Builtin.reinterpretCast(x)
    }

    print("_StringGuts(", terminator: "")
    defer { print(")") }
    printHex(rawBits.0, newline: false)
    print(" ", terminator: "")
    printHex(rawBits.1, newline: false)
    print(": ", terminator: "")
    if _object.isNative {
      let storage = _object.nativeRawStorage
      print("native ", terminator: "")
      printHex(Builtin.reinterpretCast(storage) as UInt, newline: false)
      print(" start: ", terminator: "")
      printHex(
        Builtin.reinterpretCast(storage.rawStart) as UInt, newline: false)
      print(" count: ", terminator: "")
      print(storage.count, terminator: "")
      print("/", terminator: "")
      print(storage.capacity, terminator: "")
      return
    }
    if _object.isSmall {
      self._smallUTF8String._dump()
      return
    }
#if _runtime(_ObjC)
    if _object.isCocoa {
      print("cocoa ", terminator: "")
      printHex(
        Builtin.reinterpretCast(_object.asCocoaObject) as UInt, newline: false)
      print(" start: ", terminator: "")
      if _object.isContiguous {
        printHex(
          Builtin.reinterpretCast(_cocoaRawStart) as UInt, newline: false)
      } else {
        print("<opaque>", terminator: "")
      }
      print(" count: ", terminator: "")
      print(_cocoaCount, terminator: "")
      return
    }
#else // no ObjC
    if _object.isOpaque {
      print("opaque ", terminator: "")
      printHex(
        Builtin.reinterpretCast(_object.asOpaqueObject) as UInt, newline: false)
      print(" count: ", terminator: "")
      print(_opaqueCount, terminator: "")
      return
    }
#endif // ObjC
    if _object.isUnmanaged {
      print("unmanaged ", terminator: "")
      printHex(
        Builtin.reinterpretCast(_unmanagedRawStart) as UInt, newline: false)
      print(" count: ", terminator: "")
      print(_unmanagedCount, terminator: "")
      return
    }
    print("error", terminator: "")
    if isASCII {
      print(" <ascii>", terminator: "")
    }
    else {
      print(" <utf16>", terminator: "")
    }
#endif // INTERNAL_CHECKS_ENABLED
  }
}

//
// String API helpers
//
extension _StringGuts {
  // Return a contiguous _StringGuts with the same contents as this one.
  // Use the existing guts if possible; otherwise copy the string into a
  // new buffer.
  @usableFromInline
  internal
  func _extractContiguous<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _StringGuts
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    if _fastPath(
      _object.isContiguous && CodeUnit.bitWidth == _object.bitWidth) {
      return self
    }

    // TODO (TODO: JIRA): check if we're small, extract that.

    let count = self.count
    return _StringGuts(
      _large: _copyToNativeStorage(of: CodeUnit.self, from: 0..<count))
  }

  @usableFromInline
  internal
  func _extractContiguousUTF16() -> _StringGuts {
    return _extractContiguous(of: UTF16.CodeUnit.self)
  }

  @usableFromInline
  internal
  func _extractContiguousASCII() -> _StringGuts {
    return _extractContiguous(of: UInt8.self)
  }

  // Return a native storage object with the same contents as this string.
  // Use the existing buffer if possible; otherwise copy the string into a
  // new buffer.
  @usableFromInline
  @_specialize(where CodeUnit == UInt8)
  @_specialize(where CodeUnit == UInt16)
  internal
  func _extractNativeStorage<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _SwiftStringStorage<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    if _fastPath(_object.isNative && CodeUnit.bitWidth == _object.bitWidth) {
      return _object.nativeStorage()
    }
    let count = self.count
    return _copyToNativeStorage(of: CodeUnit.self, from: 0..<count)
  }

  @_specialize(where CodeUnit == UInt8)
  @_specialize(where CodeUnit == UInt16)
  internal
  func _copyToNativeStorage<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self,
    from range: Range<Int>,
    unusedCapacity: Int = 0
  ) -> _SwiftStringStorage<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(unusedCapacity >= 0)
    let storage = _SwiftStringStorage<CodeUnit>.create(
      capacity: range.count + unusedCapacity,
      count: range.count)
    self._copy(range: range, into: storage.usedBuffer)
    return storage
  }

  @usableFromInline // @testable
  func _extractSlice(_ range: Range<Int>) -> _StringGuts {
    if range.isEmpty { return _StringGuts() }
    if range == 0..<count { return self }

    if self._isSmall {
      return _StringGuts(self._smallUTF8String[range])
    }

    if self.isASCII {
      defer { _fixLifetime(self) }
      let ascii = self._unmanagedASCIIView[range]
      if let small = _SmallUTF8String(ascii.buffer) {
        return _StringGuts(small)
      }
      if _object.isUnmanaged {
        return _StringGuts(_large: ascii)
      }
      return _StringGuts(
        _large: _copyToNativeStorage(of: UInt8.self, from: range))
    }

    // TODO(SSO): small UTF-16 strings
    if _object.isUnmanaged {
      return _StringGuts(_large: _unmanagedUTF16View[range])
    }
    return _StringGuts(
      _large: _copyToNativeStorage(of: UTF16.CodeUnit.self, from: range))
  }

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
      // Need more space; borrow Array's exponential growth curve.
      return (
        storage.count,
        Swift.max(
          _growArrayCapacity(storage.capacity),
          count + unusedCapacity))
    }
    // We have enough space; check if it's unique and of the correct width.
    if _fastPath(_object.bitWidth == CodeUnit.bitWidth) {
      if _fastPath(_isUniqueNative()) {
        return nil
      }
    }
    // If not, allocate new storage, but keep existing capacity.
    return (storage.count, storage.capacity)
  }

  // Convert ourselves (if needed) to a native string with the specified storage
  // parameters and call `body` on the resulting native storage.
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
        unusedCapacity: params.capacity - params.count))
    let result = body(unmanagedRef)
    self = _StringGuts(_large: unmanagedRef.takeRetainedValue())
    _fixLifetime(self)
    return result
  }

  @inline(__always)
  internal
  mutating func withMutableASCIIStorage<R>(
    unusedCapacity: Int,
    _ body: (Unmanaged<_ASCIIStringStorage>) -> R
  ) -> R {
    return self.withMutableStorage(
      of: UInt8.self, unusedCapacity: unusedCapacity, body)
  }

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
  @inlinable
  internal var _hasStoredCount: Bool {
    @inline(__always) get { return !_object.isSmallOrCocoa }
  }

  @inlinable
  internal var startIndex: Int {
    return 0
  }

  @inlinable
  internal var endIndex: Int {
    @inline(__always) get { return count }
  }

  @inlinable
  internal var count: Int {
    if _slowPath(!_hasStoredCount) {
      return _nonStoredCount
    }

    // TODO(StringObject): Mask off the high bits
    _sanityCheck(Int(self._otherBits) >= 0)
    return Int(bitPattern: self._otherBits)
  }

  @usableFromInline
  internal
  var _nonStoredCount: Int {
    @_effects(readonly)
    get {
      if _object.isSmall {
        return _object.smallUTF8Count
      }
#if _runtime(_ObjC)
      _sanityCheck(_object.isCocoa)
      return _cocoaCount
#else
      _sanityCheck(_object.isOpaque)
      return _opaqueCount
#endif
    }
  }

  @inlinable
  internal var capacity: Int {
    if _fastPath(_object.isNative) {
      return _object.nativeRawStorage.capacity
    }
    return 0
  }

  /// Get the UTF-16 code unit stored at the specified position in this string.
  @inlinable // FIXME(sil-serialize-all)
  func codeUnit(atCheckedOffset offset: Int) -> UTF16.CodeUnit {
    if _slowPath(_isOpaque) {
      return _opaqueCodeUnit(atCheckedOffset: offset)
    } else if isASCII {
      return _unmanagedASCIIView.codeUnit(atCheckedOffset: offset)
    } else {
      return _unmanagedUTF16View.codeUnit(atCheckedOffset: offset)
    }
  }

  @usableFromInline // @opaque
  func _opaqueCodeUnit(atCheckedOffset offset: Int) -> UTF16.CodeUnit {
    _sanityCheck(_isOpaque)
    // TODO: ascii fast path, and reconsider this whole API anyways
    if self._isSmall {
      return self._smallUTF8String.withUnmanagedASCII {
        $0.codeUnit(atCheckedOffset: offset)
      }
    }

    defer { _fixLifetime(self) }
    return _asOpaque().codeUnit(atCheckedOffset: offset)
  }


  // Copy code units from a slice of this string into a buffer.
  internal func _copy<CodeUnit>(
    range: Range<Int>,
    into dest: UnsafeMutableBufferPointer<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(CodeUnit.bitWidth == 8 || CodeUnit.bitWidth == 16)
    _sanityCheck(dest.count >= range.count)
    if _slowPath(_isOpaque) {
      _opaqueCopy(range: range, into: dest)
      return
    }

    defer { _fixLifetime(self) }
    if isASCII {
      _unmanagedASCIIView[range]._copy(into: dest)
    } else {
      _unmanagedUTF16View[range]._copy(into: dest)
    }
  }

  internal func _opaqueCopy<CodeUnit>(
    range: Range<Int>,
    into dest: UnsafeMutableBufferPointer<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(_isOpaque)
    if _fastPath(self._isSmall) {
      var slice = self._smallUTF8String[range]
      slice._copy(into: dest)
      return
    }

    defer { _fixLifetime(self) }
    _asOpaque()[range]._copy(into: dest)
  }

  @usableFromInline
  mutating func reserveUnusedCapacity(
    _ unusedCapacity: Int,
    ascii: Bool = false
  ) {
    if _fastPath(_isUniqueNative()) {
      if _fastPath(
        ascii == (_object.bitWidth == 8) &&
        _object.nativeRawStorage.unusedCapacity >= unusedCapacity) {
        return
      }
    }

    // TODO (TODO: JIRA): check if we're small and still within capacity

    if ascii {
      let storage = _copyToNativeStorage(
        of: UInt8.self,
        from: 0..<self.count,
        unusedCapacity: unusedCapacity)
      self = _StringGuts(_large: storage)
    } else {
      let storage = _copyToNativeStorage(
        of: UTF16.CodeUnit.self,
        from: 0..<self.count,
        unusedCapacity: unusedCapacity)
      self = _StringGuts(_large: storage)
    }
    _invariantCheck()
  }

  @inlinable // @testable
  mutating func reserveCapacity(_ capacity: Int) {
    // Small strings can accomodate small capacities
    if capacity <= _SmallUTF8String.capacity {
      return
    }
    reserveCapacitySlow(capacity)
  }

  @usableFromInline
  mutating func reserveCapacitySlow(_ capacity: Int) {
    if _fastPath(_isUniqueNative()) {
      if _fastPath(_object.nativeRawStorage.capacity >= capacity) {
        return
      }
    }

    let selfCount = self.count
    if isASCII {
      let storage = _copyToNativeStorage(
        of: UInt8.self,
        from: 0..<selfCount,
        unusedCapacity: Swift.max(capacity - count, 0))
      self = _StringGuts(_large: storage)
    } else {
      let storage = _copyToNativeStorage(
        of: UTF16.CodeUnit.self,
        from: 0..<selfCount,
        unusedCapacity: Swift.max(capacity - count, 0))
      self = _StringGuts(_large: storage)
    }
    _invariantCheck()
  }

  internal
  mutating func append(_ other: _UnmanagedASCIIString) {
    guard other.count > 0 else { return  }

    if self._isSmall {
      if let result = self._smallUTF8String._appending(other.buffer) {
        self = _StringGuts(result)
        return
      }
    }
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

  internal
  mutating func append(_ other: _UnmanagedUTF16String) {
    guard other.count > 0 else { return  }
    withMutableUTF16Storage(unusedCapacity: other.count) { storage in
      storage._value._appendInPlace(other)
    }
  }

  internal
  mutating func append(_ other: _UnmanagedOpaqueString) {
    guard other.count > 0 else { return  }
    withMutableUTF16Storage(unusedCapacity: other.count) { storage in
      storage._value._appendInPlace(other)
    }
  }

  internal
  mutating func append<S: StringProtocol>(_ other: S) {
    self.append(other._wholeString._guts, range: other._encodedOffsetRange)
  }

  @inlinable // @testable
  internal
  mutating func append(_ other: _StringGuts) {
    // We inline only the _isEmptySingleton check because it can often be
    // proven or disproven at compile time. A full length check is often
    // inconclusive.
    if _isEmptySingleton {
      self = other
      return
    }
    _appendSlow(other)
  }

  @usableFromInline
  internal
  mutating func _appendSlow(_ other: _StringGuts) {
    // FIXME(TODO: JIRA): shouldn't _isEmptySingleton be sufficient?
    if self.count == 0 && !_object.isNative {
      // We must be careful not to discard any capacity that
      // may have been reserved for the append -- this is why
      // we check for the empty string singleton rather than
      // a zero `count` above.
      self = other
      return
    }
    if _slowPath(other._isOpaque) {
      _opaqueAppend(opaqueOther: other)
      return
    }

    defer { _fixLifetime(other) }
    if other.isASCII {
      self.append(other._unmanagedASCIIView)
    } else {
      self.append(other._unmanagedUTF16View)
    }
  }

  mutating func _opaqueAppend(opaqueOther other: _StringGuts) {
    if other._isSmall {
      // TODO: Fix the visitation pattern for append here. For now, we funnel
      // through _UnmanagedASCIIString.
      other._smallUTF8String.withUnmanagedASCII {
        self.append($0)
      }
      return
    }

    _sanityCheck(other._isOpaque)
    defer { _fixLifetime(other) }
    self.append(other._asOpaque())
  }

  @usableFromInline
  internal
  mutating func append(_ other: _StringGuts, range: Range<Int>) {
    _sanityCheck(range.lowerBound >= 0 && range.upperBound <= other.count)
    guard range.count > 0 else { return }
    if _isEmptySingleton && range.count == other.count {
      self = other
      return
    }
    if _slowPath(other._isOpaque) {
      _opaqueAppend(opaqueOther: other, range: range)
      return
    }

    defer { _fixLifetime(other) }
    if other.isASCII {
      self.append(other._unmanagedASCIIView[range])
    } else {
      self.append(other._unmanagedUTF16View[range])
    }
  }

  mutating func _opaqueAppend(opaqueOther other: _StringGuts, range: Range<Int>) {
    if other._isSmall {
      other._smallUTF8String.withUnmanagedASCII {
        self.append($0[range])
      }
      return
    }

    _sanityCheck(other._isOpaque)
    defer { _fixLifetime(other) }
    self.append(other._asOpaque()[range])
  }

  //
  // FIXME (TODO JIRA): Appending a character onto the end of a string should
  // really have a less generic implementation, then we can drop @specialize.
  //
  @usableFromInline
  @_specialize(where C == Character._SmallUTF16)
  mutating func append<C : RandomAccessCollection>(contentsOf other: C)
  where C.Element == UInt16 {
    if self._isSmall {
      if let result = self._smallUTF8String._appending(other) {
        self = _StringGuts(result)
        return
      }
    }

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
  @usableFromInline
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
      let dst = storage.start + bounds.lowerBound
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
    self = _StringGuts(_large: storage)
    _invariantCheck()
  }

  @usableFromInline
  mutating func replaceSubrange<C>(
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

extension _StringGuts {
  // TODO: Drop or unify with String._fromCodeUnits
  internal
  static func fromCodeUnits<Encoding : _UnicodeEncoding>(
    _ input: UnsafeBufferPointer<Encoding.CodeUnit>,
    encoding: Encoding.Type,
    repairIllFormedSequences: Bool,
    minimumCapacity: Int = 0
  ) -> (_StringGuts?, hadError: Bool) {
    // Determine how many UTF-16 code units we'll need
    guard let (utf16Count, isASCII) = UTF16.transcodedLength(
      of: input.makeIterator(),
      decodedAs: Encoding.self,
      repairingIllFormedSequences: repairIllFormedSequences) else {
      return (nil, true)
    }
    if isASCII {
      if let small = _SmallUTF8String(
        _fromCodeUnits: input,
        utf16Length: utf16Count,
        isASCII: true,
        Encoding.self
      ) {
        return (_StringGuts(small), false)
      }

      let storage = _SwiftStringStorage<UTF8.CodeUnit>.create(
        capacity: Swift.max(minimumCapacity, utf16Count),
        count: utf16Count)
      let hadError = storage._initialize(
        fromCodeUnits: input,
        encoding: Encoding.self)
      return (_StringGuts(_large: storage), hadError)
    }
    let storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
      capacity: Swift.max(minimumCapacity, utf16Count),
      count: utf16Count)
    let hadError = storage._initialize(
      fromCodeUnits: input,
      encoding: Encoding.self)
    return (_StringGuts(_large: storage), hadError)
  }
}

extension _SwiftStringStorage {
  /// Initialize a piece of freshly allocated storage instance from a sequence
  /// of code units, which is assumed to contain exactly as many code units as
  /// fits in the current storage count.
  ///
  /// Returns true iff `input` was found to contain invalid code units in the
  /// specified encoding. If any invalid sequences are found, they are replaced
  /// with REPLACEMENT CHARACTER (U+FFFD).
  internal
  func _initialize<Encoding: _UnicodeEncoding>(
    fromCodeUnits input: UnsafeBufferPointer<Encoding.CodeUnit>,
    encoding: Encoding.Type
  ) -> Bool {
    var p = self.start
    let hadError = transcode(
      input.makeIterator(),
      from: Encoding.self,
      to: UTF16.self,
      stoppingOnError: false) { cu in
      _sanityCheck(p < end)
      p.pointee = CodeUnit(cu)
      p += 1
    }
    _sanityCheck(p == end)
    return hadError
  }
}

extension _StringGuts {
  // FIXME: Remove. Still used by swift-corelibs-foundation
  @available(*, deprecated)
  public var startASCII: UnsafeMutablePointer<UTF8.CodeUnit> {
    return UnsafeMutablePointer(mutating: _unmanagedASCIIView.start)
  }

  // FIXME: Remove. Still used by swift-corelibs-foundation
  @available(*, deprecated)
  public var startUTF16: UnsafeMutablePointer<UTF16.CodeUnit> {
    return UnsafeMutablePointer(mutating: _unmanagedUTF16View.start)
  }
}

extension _StringGuts {
  @available(*, deprecated)
  public // SPI(Foundation)
  var _isContiguousASCII: Bool {
    return _object.isContiguousASCII
  }

  @available(*, deprecated)
  public // SPI(Foundation)
  var _isContiguousUTF16: Bool {
    return _object.isContiguousUTF16
  }

  @available(*, deprecated)
  public // SPI(Foundation)
  func _withUnsafeUTF8CodeUnitsIfAvailable<Result>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> Result
  ) rethrows -> Result? {
    guard _object.isContiguousASCII else { return nil }
    return try f(_unmanagedASCIIView.buffer)
  }

  @available(*, deprecated)
  public // SPI(Foundation)
  func _withUnsafeUTF16CodeUnitsIfAvailable<Result>(
    _ f: (UnsafeBufferPointer<UInt16>) throws -> Result
  ) rethrows -> Result? {
    guard _object.isContiguousUTF16 else { return nil }
    return try f(_unmanagedUTF16View.buffer)
  }
}

