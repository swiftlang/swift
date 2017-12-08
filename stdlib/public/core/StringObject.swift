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

// TODO: Comments. Supposed to abstract bit-twiddling operations. Meant to be a
// completely transparent struct. That is, it's just a trivial encapsulation to
// host many operations that would otherwise be scattered throughout StringGuts
// implementation.
//
@_fixed_layout
public // @testable
struct _StringObject {
  // TODO: Proper built-in string object support. For now, we use BridgeObject
  // which might be very slightly suboptimal and different than our bit
  // patterns, but provides us the runtime functionality we want.
  @_versioned
  internal
  var _object: _BuiltinBridgeObject

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init(_ object: _BuiltinBridgeObject) {
    self._object = object
    _invariantCheck()
  }

  @_versioned
  @_inlineable
  internal
  var rawBits: UInt {
    @inline(__always)
    get { return _bitPattern(_object) }
  }
}

//
// Constant bits and masks
//
//
// Bit layout (x86-64 and arm64):
//
// _StringObject:
// +---+---+---|---+------+----------------------------------------------------+
// + t | v | o | w | uuuu | payload (54 bits)                                  |
// +---+---+---|---+------+----------------------------------------------------+
// where t: is-a-value, i.e. a tag bit that says not to perform ARC
//       v: sub-variant bit, i.e. set for isCocoa or isSmall
//       o: is-opaque, i.e. opaque vs contiguously stored strings
//       w: width indicator bit (0: ASCII, 1: UTF-16)
//       u: unused bits
//
// payload is:
//   isNative: the native StringStorage object
//   isCocoa: the Cocoa object
//   isUnmanaged: the pointer to code units
//   isSmall: opaque bits used for inline storage // TODO: use them!
//
// TODO: 32-bit platforms!
//
extension _StringObject {
  // NOTE: deviating from ObjC tagged pointer bits, as we just want to avoid
  // swift runtime management, and top bit suffices for that.
  @_versioned
  @_inlineable
  internal
  static var _isValueBit: UInt {
    @inline(__always)
    get { return 0x80_00_0000_0000_0000 }
  }

  // After deciding isValue, which of the two variants (on both sides) are we.
  // That is, native vs objc or unsafe vs small.
  @_versioned
  @_inlineable
  internal
  static var _subVariantBit: UInt {
    @inline(__always)
    get { return 0x40_00_0000_0000_0000 }
  }

  @_versioned
  @_inlineable
  internal
  static var _isOpaqueBit: UInt {
    @inline(__always)
    get { return 0x20_00_0000_0000_0000 }
  }

  @_versioned
  @_inlineable
  internal
  static var _twoByteBit: UInt {
    @inline(__always)
    get { return 0x10_00_0000_0000_0000 }
  }

  // Which of the 4 sub-variants are we depends on the top two bits
  @_versioned
  @_inlineable
  internal
  static var _variantMask: UInt {
    @inline(__always)
    get { return _isValueBit | _subVariantBit }
  }

  @_versioned
  @_inlineable
  internal
  static var _payloadMask: UInt {
    @inline(__always)
    get { return 0x00FF_FFFF_FFFF_FFFF  }
  }

  @_versioned
  @_inlineable
  internal
  static var _emptyLiteralBitPattern: UInt {
    @inline(__always)
    get { return _isValueBit | UInt(bitPattern: _emptyStringBase) }
  }
}

//
// Private convenience helpers to layer on top of BridgeObject
//
// TODO: private!
//
extension _StringObject {
  @_versioned
  @_inlineable
  internal // TODO: private!
  var asNativeObject: AnyObject {
    @inline(__always)
    get {
      _sanityCheck(isNative)
      _sanityCheck(
        _usesNativeSwiftReferenceCounting(
          type(of: Builtin.reinterpretCast(payloadBits) as AnyObject)))

      return Builtin.reinterpretCast(payloadBits)
    }
  }

  @_versioned
  @_inlineable
  internal // TODO: private!
  var asCocoaObject: _CocoaString {
    @inline(__always)
    get {
      _sanityCheck(isCocoa)
      _sanityCheck(
        !_usesNativeSwiftReferenceCounting(
          type(of: Builtin.reinterpretCast(payloadBits) as AnyObject)))
      return Builtin.reinterpretCast(payloadBits)
    }
  }

  @_versioned
  @_inlineable
  internal
  var asUnmanagedRawStart: UnsafeRawPointer {
    @inline(__always)
    get {
      _sanityCheck(isUnmanaged)
      return UnsafeRawPointer(
        bitPattern: payloadBits
      )._unsafelyUnwrappedUnchecked
    }
  }
}

//
// Queries on a StringObject
//
extension _StringObject {
  @_versioned
  @_inlineable
  internal
  var payloadBits: UInt {
    @inline(__always)
    get { return rawBits & _StringObject._payloadMask }
  }

  public // @testable
  var owner: AnyObject? { // For testing only
    if _fastPath(isNative || isCocoa) {
      return Builtin.reinterpretCast(payloadBits)
    }
    return nil
  }

  //
  // Determine which of the 4 major variants we are
  //
  @_versioned
  @_inlineable
  internal
  var isNative: Bool {
    @inline(__always)
    get { return rawBits & _StringObject._variantMask == 0 }
  }

  @_versioned
  @_inlineable
  internal
  var isCocoa: Bool {
    @inline(__always)
    get { return rawBits & _StringObject._variantMask == _StringObject._subVariantBit }
  }

  @_versioned
  @_inlineable
  internal
  var isUnmanaged: Bool {
    @inline(__always)
    get { return rawBits & _StringObject._variantMask == _StringObject._isValueBit }
  }

  @_versioned
  @_inlineable
  internal
  var isSmall: Bool {
    @inline(__always)
    get { return rawBits & _StringObject._variantMask == _StringObject._variantMask }
  }

  //
  // Frequently queried properties
  //
  @_versioned
  @_inlineable
  internal
  var isContiguous: Bool {
    @inline(__always)
    get { return rawBits & _StringObject._isOpaqueBit == 0 }
  }

  @_versioned
  @_inlineable
  internal
  var isOpaque: Bool {
    @inline(__always)
    get { return !isContiguous }
  }

  @_versioned
  @_inlineable
  internal
  var isContiguousCocoa: Bool {
    @inline(__always)
    get { return isContiguous && isCocoa }
  }

  @_versioned
  @_inlineable
  internal
  var isNoncontiguousCocoa: Bool {
    @inline(__always)
    get { return isCocoa && isOpaque }
  }

  @_inlineable
  public // @testable
  var isSingleByte: Bool {
    @inline(__always)
    get { return rawBits & _StringObject._twoByteBit == 0 }
  }

  @_inlineable
  public // @testable
  var byteWidth: Int {
    @inline(__always)
    get { return isSingleByte ? 1 : 2 }
  }

  @_versioned
  @_inlineable
  var bitWidth: Int {
    @inline(__always)
    get { return byteWidth &<< 3 }
  }

  @_inlineable
  public // @testable
  var isContiguousASCII: Bool {
    @inline(__always)
    get { return isContiguous && isSingleByte }
  }

  @_inlineable
  public // @testable
  var isContiguousUTF16: Bool {
    @inline(__always)
    get { return isContiguous && !isSingleByte }
  }

  @_versioned
  @_inlineable
  internal
  var isEmptyLiteral: Bool {
    @inline(__always)
    get { return rawBits == _StringObject._emptyLiteralBitPattern }
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  func nativeStorage<CodeUnit>(
    of codeUnit: CodeUnit.Type = CodeUnit.self
  ) -> _SwiftStringStorage<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(isNative)
    _sanityCheck(CodeUnit.bitWidth == self.bitWidth)
    // TODO: Is this the way to do it?
    return unsafeDowncast(
      asNativeObject, to: _SwiftStringStorage<CodeUnit>.self)
  }

  @_inlineable
  public // @testable
  var nativeRawStorage: _SwiftRawStringStorage {
    @inline(__always) get {
      _sanityCheck(isNative)
      return unsafeDowncast(asNativeObject, to: _SwiftRawStringStorage.self)
    }
  }
}

extension _StringObject {
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _invariantCheck() {
#if INTERNAL_CHECKS_ENABLED
    _sanityCheck(isContiguous || isOpaque)
    _sanityCheck(isOpaque || isContiguousASCII || isContiguousUTF16)
    if isNative {
      _sanityCheck(isContiguous)
      if isSingleByte {
        _sanityCheck(isContiguousASCII)
        _sanityCheck(asNativeObject is _SwiftStringStorage<UInt8>)
      } else {
        _sanityCheck(asNativeObject is _SwiftStringStorage<UInt16>)
      }
    } else if isUnmanaged {
      _sanityCheck(isContiguous)
      _sanityCheck(payloadBits > 0) // TODO: inside address space
    } else if isCocoa {
    } else if isSmall {
      _sanityCheck(isOpaque)
    } else {
      fatalError("Unimplemented string form")
    }
#endif
  }
}

//
// Conveniently construct, tag, flag, etc. StringObjects
//
extension _StringObject {
  @_versioned
  @_inlineable
  @inline(__always)
  // TODO: private
  internal
  init(rawBits: UInt) {
    self.init(Builtin.reinterpretCast(rawBits))
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init(
    _payloadBits: UInt,
    isValue: Bool,
    isSmallOrObjC: Bool, // TODO: better name here?
    isOpaque: Bool,
    isTwoByte: Bool
  ) {
    var rawBits = _payloadBits
    if isValue {
      rawBits |= _StringObject._isValueBit
    }
    if isSmallOrObjC {
      rawBits |= _StringObject._subVariantBit
    }
    if isOpaque {
      rawBits |= _StringObject._isOpaqueBit
    }
    if isTwoByte {
      rawBits |= _StringObject._twoByteBit
    }
    self.init(rawBits: rawBits)
    _sanityCheck(isSmall == (isValue && isSmallOrObjC))
    _sanityCheck(isUnmanaged == (isValue && !isSmallOrObjC))
    _sanityCheck(isCocoa == (!isValue && isSmallOrObjC))
    _sanityCheck(isNative == (!isValue && !isSmallOrObjC))
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init(
    _someObject: AnyObject,
    isCocoa: Bool,
    isContiguous: Bool,
    isSingleByte: Bool
  ) {
    defer { _fixLifetime(_someObject) }
    self.init(
      _payloadBits: Builtin.reinterpretCast(_someObject),
      isValue: false,
      isSmallOrObjC: isCocoa,
      isOpaque: !isContiguous,
      isTwoByte: !isSingleByte)
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init() {
    self.init(rawBits: _StringObject._emptyLiteralBitPattern)
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init(nativeObject: AnyObject, isSingleByte: Bool) {
    self.init(
      _someObject: nativeObject,
      isCocoa: false,
      isContiguous: true,
      isSingleByte: isSingleByte)
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init(cocoaObject: AnyObject, isSingleByte: Bool, isContiguous: Bool) {
    // TODO: is it possible to sanity check? maybe `is NSObject`?
    self.init(
      _someObject: cocoaObject,
      isCocoa: true,
      isContiguous: isContiguous,
      isSingleByte: isSingleByte)
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init(smallStringPayload: UInt, isSingleByte: Bool) {
    self.init(
      _payloadBits: smallStringPayload,
      isValue: true,
      isSmallOrObjC: true,
      isOpaque: true,
      isTwoByte: !isSingleByte)
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init<CodeUnit>(
    unmanaged: UnsafePointer<CodeUnit>
  ) where CodeUnit : FixedWidthInteger & UnsignedInteger {
    self.init(
        _payloadBits: Builtin.reinterpretCast(unmanaged),
        isValue: true,
        isSmallOrObjC: false,
        isOpaque: false,
        isTwoByte: CodeUnit.bitWidth == 16)
    _sanityCheck(isSingleByte == (CodeUnit.bitWidth == 8))
  }

  @_versioned
  @_inlineable
  @inline(__always)
  internal
  init<CodeUnit>(
    _ storage: _SwiftStringStorage<CodeUnit>
  ) where CodeUnit : FixedWidthInteger & UnsignedInteger {
    self.init(nativeObject: storage, isSingleByte: CodeUnit.bitWidth == 8)
    _sanityCheck(isSingleByte == (CodeUnit.bitWidth == 8))
  }
}

@_versioned // FIXME(sil-serialize-all)
internal var _emptyStringStorage: UInt32 = 0

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal var _emptyStringBase: UnsafeRawPointer {
  return UnsafeRawPointer(Builtin.addressof(&_emptyStringStorage))
}
