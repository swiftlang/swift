//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

#if _runtime(_ObjC)

internal var _cocoaASCIIEncoding:UInt { 1 } /* NSASCIIStringEncoding */
internal var _cocoaUTF8Encoding:UInt { 4 } /* NSUTF8StringEncoding */
internal var _cocoaUTF16Encoding:UInt { 10 } /* NSUTF16StringEncoding and NSUnicodeStringEncoding*/
internal var _cocoaMacRomanEncoding:UInt { 30 } /* NSMacOSRomanStringEncoding */

extension String {
  @available(SwiftStdlib 5.6, *)
  @_spi(Foundation)
  public init?(_nativeStorage: AnyObject) {
    let knownOther = _KnownCocoaString(_nativeStorage)
    switch knownOther {
    case .storage:
      self = unsafe _unsafeUncheckedDowncast(
        _nativeStorage,
        to: __StringStorage.self
      ).asString
    case .shared:
      self = unsafe _unsafeUncheckedDowncast(
        _nativeStorage,
        to: __SharedStringStorage.self
      ).asString
    default:
      return nil
    }
  }
}

// ObjC interfaces.
extension _AbstractStringStorage {
  
  @inline(__always)
  @_effects(releasenone)
  internal func _getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>, _ aRange: _SwiftNSRange
  ) {
    let range = unsafe Range(
      _uncheckedBounds: (aRange.location, aRange.location+aRange.length))
    unsafe utf16._nativeCopy(
      into: UnsafeMutableBufferPointer(start: buffer, count: range.count),
      offsetRange: range)
  }

  @inline(__always)
  @_effects(releasenone)
  internal func _getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, _ maxLength: Int, _ encoding: UInt
  ) -> Int8 {
    switch (encoding, isASCII) {
    case (_cocoaASCIIEncoding, true),
         (_cocoaMacRomanEncoding, true),
         (_cocoaUTF8Encoding, _):
      guard maxLength >= count + 1 else { return 0 }
      unsafe outputPtr.initialize(from: start, count: count)
      unsafe outputPtr[count] = 0
      return 1
    default:
      return unsafe _cocoaGetCStringTrampoline(self, outputPtr, maxLength, encoding)
    }
  }

  @inline(__always)
  @_effects(readonly)
  internal func _cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    switch (encoding, isASCII) {
    case (_cocoaASCIIEncoding, true),
         (_cocoaMacRomanEncoding, true),
         (_cocoaUTF8Encoding, _):
      return unsafe start
    default:
      return unsafe _cocoaCStringUsingEncodingTrampoline(self, encoding)
    }
  }
  
  @_effects(readonly)
  internal func _lengthOfBytes(using encoding: UInt) -> UInt {
    switch encoding {
    case _cocoaASCIIEncoding:
      if unsafe isASCII || _allASCII(UnsafeBufferPointer(start: start, count: count)) {
        return UInt(count)
      }
      return 0
    case _cocoaUTF8Encoding:
      return UInt(count)
    case _cocoaUTF16Encoding:
      return UInt(UTF16Length) * 2
    case _cocoaMacRomanEncoding:
      if unsafe isASCII || _allASCII(UnsafeBufferPointer(start: start, count: count)) {
        return UInt(count)
      }
      fallthrough
    default:
      return _cocoaLengthOfBytesInEncodingTrampoline(self, encoding)
    }
  }
  
  // The caller info isn't useful here anyway because it's never client code,
  // so this makes sure that _character(at:) doesn't have inlined assertion bits
  @inline(never)
  internal func _characterAtIndexOutOfBounds() -> Never {
    _preconditionFailure("String index is out of bounds")
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _character(at offset: Int) -> UInt16 {
    if _fastPath(isASCII) {
      if (_fastPath(offset < count && offset >= 0)) {
        return unsafe UInt16((start + offset).pointee)
      }
      _characterAtIndexOutOfBounds()
    } else {
      return utf16[nativeNonASCIIOffset: offset]
    }
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _isEqualToBuffer(
    ptr: UnsafeRawPointer,
    count otherByteCount: Int,
    encoding: UInt
  ) -> Bool {
    if count == 0 {
      return otherByteCount == 0
    }
    if otherByteCount == 0 {
      return false
    }
    let selfBytes = unsafe Span(_unsafeStart: start, count: count).bytes
    let otherBytes = unsafe RawSpan(_unsafeStart: ptr, byteCount: otherByteCount)
    switch encoding {
    case _cocoaASCIIEncoding, _cocoaUTF8Encoding:
      if selfBytes.isIdentical(to: otherBytes) {
        return true
      }
      return isEqual(bytes: selfBytes, bytes: otherBytes)
    case _cocoaUTF16Encoding:
      if isASCII {
        return isEqual(asciiBytes: selfBytes, utf16Bytes: otherBytes)
      } else {
        return isEqual(utf8Bytes: selfBytes, utf16Bytes: otherBytes)
      }
    default:
      fatalError("Unsupported encoding")
    }
  }

  @inline(__always)
  @_effects(readonly)
  internal func _isEqual(_ other: AnyObject?) -> Int8 {
    guard let other = other else {
      return 0
    }

    if self === other {
      return 1
    }
    
    // Handle the case where both strings were bridged from Swift.
    // We can't use String.== because it doesn't match NSString semantics.
    let knownOther = _KnownCocoaString(other)
    switch knownOther {
    case .storage:
      let nativeOther = unsafe _unsafeUncheckedDowncast(other, to: __StringStorage.self)
      return unsafe nativeOther._isEqualToBuffer(
        ptr: start,
        count: count,
        encoding: isASCII ? _cocoaASCIIEncoding : _cocoaUTF8Encoding
      ) ? 1 : 0
    case .shared:
      let nativeOther = unsafe _unsafeUncheckedDowncast(other, to: __SharedStringStorage.self)
      return unsafe nativeOther._isEqualToBuffer(
        ptr: start,
        count: count,
        encoding: isASCII ? _cocoaASCIIEncoding : _cocoaUTF8Encoding
      ) ? 1 : 0
    default:
      // We're allowed to crash, but for compatibility reasons NSCFString allows
      // non-strings here.
      if !_isNSString(other) {
        return 0
      }
      
      let otherUTF16Count = _stdlib_binary_CFStringGetLength(other)
      if isASCII || hasBreadcrumbs  {
        // UTF16Length is O(1) in these two cases
        if otherUTF16Count != UTF16Length {
          return 0
        }
      } else {
        // Otherwise we have to do an imprecise check to avoid transcoding costs
        if shouldEarlyOut(
          lhsByteCount: count,
          lhsEncoding: _cocoaUTF8Encoding,
          rhsByteCount: otherUTF16Count &* 2,
          rhsEncoding: _cocoaUTF16Encoding
        ) {
          return 0
        }
      }
      
      return unsafe _NSStringIsEqualToBytes(
        other,
        bytes: start,
        count: count,
        encoding: isASCII ? _cocoaASCIIEncoding : _cocoaUTF8Encoding
      )
    }
  }
}

extension __StringStorage {
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) @inline(__always) get {
      // UTF16View does this, but there's still a little overhead
      if isASCII {
        return count
      }
      // Using _nativeGetOffset skips checking for foreign string nature
      let utf16View = utf16
      return utf16View._nativeGetOffset(for: utf16View.endIndex)
    }
  }

  @objc
  final internal var hash: UInt {
    @_effects(readonly) get {
      if isASCII {
        return unsafe _cocoaHashASCIIBytes(start, length: count)
      }
      return _cocoaHashString(self)
    }
  }

  @objc(characterAtIndex:)
  @_effects(readonly)
  final internal func character(at offset: Int) -> UInt16 {
    _character(at: offset)
  }

  @objc(getCharacters:range:)
  @_effects(releasenone)
  final internal func getCharacters(
   _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  ) {
    unsafe _getCharacters(buffer, aRange)
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return unsafe start._asCChar
    }
    return nil
  }
  
  @objc(_fastUTF8StringContents:utf8Length:)
  @_effects(readonly)
  final internal func _fastUTF8StringContents(
    _ requiresNulTermination: Int8,
    _ outUTF8Length: UnsafeMutablePointer<UInt>
  ) -> UnsafePointer<UInt8>? {
    unsafe outUTF8Length.pointee = UInt(count)
    return unsafe start
  }

  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8>? {
    return unsafe start
  }

  @objc(cStringUsingEncoding:)
  @_effects(readonly)
  final internal func cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    return unsafe _cString(encoding: encoding)
  }

  @objc(getCString:maxLength:encoding:)
  @_effects(releasenone)
  final internal func getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, maxLength: Int, encoding: UInt
  ) -> Int8 {
    return unsafe _getCString(outputPtr, maxLength, encoding)
  }

  @objc
  final internal var fastestEncoding: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaASCIIEncoding
      }
      return _cocoaUTF8Encoding
    }
  }
  
  @objc(lengthOfBytesUsingEncoding:)
  @_effects(readonly)
  final internal func lengthOfBytes(using encoding: UInt) -> UInt {
    _lengthOfBytes(using: encoding)
  }

  @objc(isEqualToString:)
  @_effects(readonly)
  final internal func isEqualToString(to other: AnyObject?) -> Int8 {
    return _isEqual(other)
  }

  @objc(isEqual:)
  @_effects(readonly)
  final internal func isEqual(to other: AnyObject?) -> Int8 {
    return _isEqual(other)
  }
  
  @objc(_isEqualToBytes:count:encoding:)
  @_effects(readonly)
  final internal func isEqual(
    ptr: UnsafeRawPointer,
    count: Int,
    encoding: UInt
  ) -> Int8 {
    return unsafe _isEqualToBuffer(
      ptr: ptr,
      count: count,
      encoding: encoding
    ) ? 1 : 0
  }
  
  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While __StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }
}

extension __SharedStringStorage {
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) get {
      // UTF16View does this, but there's still a little overhead
      if isASCII {
        return count
      }
      // Using _nativeGetOffset skips checking for foreign string nature
      let utf16View = utf16
      return utf16View._nativeGetOffset(for: utf16View.endIndex)
    }
  }

  @objc
  final internal var hash: UInt {
    @_effects(readonly) get {
      if isASCII {
        return unsafe _cocoaHashASCIIBytes(start, length: count)
      }
      return _cocoaHashString(self)
    }
  }

  @objc(characterAtIndex:)
  @_effects(readonly)
  final internal func character(at offset: Int) -> UInt16 {
    _character(at: offset)
  }

  @objc(getCharacters:range:)
  @_effects(releasenone)
  final internal func getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  ) {
    unsafe _getCharacters(buffer, aRange)
  }

  @objc
  final internal var fastestEncoding: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaASCIIEncoding
      }
      return _cocoaUTF8Encoding
    }
  }
  
  @objc(lengthOfBytesUsingEncoding:)
  @_effects(readonly)
  final internal func lengthOfBytes(using encoding: UInt) -> UInt {
    _lengthOfBytes(using: encoding)
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return unsafe start._asCChar
    }
    return nil
  }
  
  @objc(_fastUTF8StringContents:utf8Length:)
  @_effects(readonly)
  final internal func _fastUTF8StringContents(
    _ requiresNulTermination: Int8,
    _ outUTF8Length: UnsafeMutablePointer<UInt>
  ) -> UnsafePointer<UInt8>? {
    unsafe outUTF8Length.pointee = UInt(count)
    return unsafe start
  }

  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8>? {
    return unsafe start
  }

  @objc(cStringUsingEncoding:)
  @_effects(readonly)
  final internal func cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    return unsafe _cString(encoding: encoding)
  }

  @objc(getCString:maxLength:encoding:)
  @_effects(releasenone)
  final internal func getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, maxLength: Int, encoding: UInt
  ) -> Int8 {
    return unsafe _getCString(outputPtr, maxLength, encoding)
  }

  @objc(isEqualToString:)
  @_effects(readonly)
  final internal func isEqualToString(to other: AnyObject?) -> Int8 {
    return _isEqual(other)
  }

  @objc(isEqual:)
  @_effects(readonly)
  final internal func isEqual(to other: AnyObject?) -> Int8 {
    return _isEqual(other)
  }
  
  @objc(_isEqualToBytes:count:encoding:)
  @_effects(readonly)
  final internal func isEqual(
    ptr: UnsafeRawPointer,
    count: Int,
    encoding: UInt
  ) -> Int8 {
    return unsafe _isEqualToBuffer(
      ptr: ptr,
      count: count,
      encoding: encoding
    ) ? 1 : 0
  }

  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While __StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }
}

fileprivate extension RawSpan {
  @inline(__always)
  func decodeUTF8(ofByteWidth width: Int, at index: Int) -> UInt32? {
    guard index <= self.byteOffsets.upperBound &- width else {
      return nil
    }
    var l1 = unsafe UInt32(self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 0, as: UInt8.self))
    switch width {
    case 1:
      break
    case 2:
      let l2 = unsafe UInt32(self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 1, as: UInt8.self))
      l1 =  (l1 & 0b00011111) << 6
      l1 |= (l2 & 0b00111111)
    case 3:
      let l2 = unsafe UInt32(self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 1, as: UInt8.self))
      let l3 = unsafe UInt32(self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 2, as: UInt8.self))
      l1 =  (l1 & 0b00001111) << 12
      l1 |= (l2 & 0b00111111) << 6
      l1 |= (l3 & 0b00111111)
    default:
      let l2 = unsafe UInt32(self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 1, as: UInt8.self))
      let l3 = unsafe UInt32(self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 2, as: UInt8.self))
      let l4 = unsafe UInt32(self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 3, as: UInt8.self))
      l1 =  (l1 & 0b00000111) << 18
      l1 |= (l2 & 0b00111111) << 12
      l1 |= (l3 & 0b00111111) << 6
      l1 |= (l4 & 0b00111111)
    }
    return l1
  }
  
  @inline(__always)
  func decodeUTF16(ofByteWidth width: Int, at index: Int) -> UInt32? {
    _debugPrecondition(width == 2 || width == 4)
    guard index <= self.byteOffsets.upperBound &- width else {
      return nil
    }
    let r1 = unsafe self.unsafeLoadUnaligned(fromUncheckedByteOffset: index, as: UInt16.self)
    if width == 2 {
      return UInt32(r1)
    }
    if !Unicode.UTF16.isLeadSurrogate(r1) {
      return nil
    }
    let r2 = unsafe self.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 2, as: UInt16.self)
    if !Unicode.UTF16.isTrailSurrogate(r2) {
      return nil
    }
    return Unicode.UTF16._decodeSurrogates(r1, r2).value
  }
  
  @inline(__always)
  func contentsAreTriviallyIdentical(to rhs: RawSpan, possibleUTF16EdgeCase: Bool) -> Bool? {
    if self.byteCount == 0 {
      return rhs.byteCount == 0
    }
    if rhs.byteCount == 0 {
      return false //already covered self.byteCount == 0 above
    }
    /*
     Bizarre edge case: an ASCII buffer with embedded null bytes could be
     bitwise-equal to a UTF16 buffer without embedded null bytes while not
     being semantically equal
     */
    if possibleUTF16EdgeCase {
      return nil
    }
    if self.isIdentical(to: rhs) {
      return true
    }
    return nil //can't decide trivially
  }
}

@inline(always)
fileprivate func shouldEarlyOut(
  lhsUTF8ByteCount: Int,
  rhsUTF16ByteCount: Int,
) -> Bool {
  // If the UTF8 buffer is entirely 3 byte characters, that will be 2 bytes in UTF16
  let minimumUTF16CountForUTF8 = (lhsUTF8ByteCount * 2) / 3
  if rhsUTF16ByteCount < minimumUTF16CountForUTF8 {
    return true
  }
  // On the other hand if it's entirely 1 byte characters, that'll double in size
  let maximumUTF16CountForUTF8 = lhsUTF8ByteCount * 2
  if rhsUTF16ByteCount > maximumUTF16CountForUTF8 {
    return true
  }
  return false
}

@inline(always)
fileprivate func shouldEarlyOut(
  lhsByteCount: Int,
  lhsEncoding: UInt,
  rhsByteCount: Int,
  rhsEncoding: UInt
) -> Bool {
  if lhsByteCount == 0 {
    return rhsByteCount != 0
  }
  if rhsByteCount == 0 {
    return true
  }
  return switch (lhsEncoding, rhsEncoding) {
  case (_cocoaUTF8Encoding, _cocoaUTF8Encoding),
       (_cocoaUTF16Encoding, _cocoaUTF16Encoding),
       (_cocoaASCIIEncoding, _cocoaASCIIEncoding),
       (_cocoaASCIIEncoding, _cocoaUTF8Encoding),
       (_cocoaUTF8Encoding, _cocoaASCIIEncoding):
    lhsByteCount != rhsByteCount
  case (_cocoaUTF16Encoding, _cocoaASCIIEncoding):
    lhsByteCount != rhsByteCount &* 2
  case (_cocoaASCIIEncoding, _cocoaUTF16Encoding):
    lhsByteCount &* 2 != rhsByteCount
  case (_cocoaUTF8Encoding, _cocoaUTF16Encoding):
    shouldEarlyOut(
      lhsUTF8ByteCount: lhsByteCount, rhsUTF16ByteCount: rhsByteCount)
  case (_cocoaUTF16Encoding, _cocoaUTF8Encoding):
    shouldEarlyOut(
      lhsUTF8ByteCount: rhsByteCount, rhsUTF16ByteCount: lhsByteCount)
  default:
    fatalError("Unsupported encoding")
  }
}

@_effects(readonly)
fileprivate func isEqual(
  utf8Bytes utf8: RawSpan,
  utf16Bytes utf16: RawSpan
) -> Bool {
    if shouldEarlyOut(
      lhsByteCount: utf8.byteCount,
      lhsEncoding: _cocoaUTF8Encoding,
      rhsByteCount: utf16.byteCount,
      rhsEncoding: _cocoaUTF16Encoding
    ) {
      return false
    }
    
    var utf8Idx = 0
    var utf16Idx = 0
    
    while true {
      guard let firstUTF8Byte = utf8.decodeUTF8(ofByteWidth: 1, at: utf8Idx) else {
        // If we've consumed everything in both buffers without finding an inequality, we're equal
        return utf8Idx == utf8.byteOffsets.upperBound && utf16Idx == utf16.byteOffsets.upperBound
      }
      let utf8Width: Int
      let utf16Width: Int
      if firstUTF8Byte < 128 {
        utf8Width = 1
        utf16Width = 2
        guard let r = utf16.decodeUTF16(ofByteWidth: utf16Width, at: utf16Idx) else {
          return false
        }
        if r != firstUTF8Byte {
          return false
        }
      } else {
        utf8Width = _utf8ScalarLength(UInt8(truncatingIfNeeded: firstUTF8Byte))
        utf16Width = utf8Width == 4 ? 4 : 2
        guard
          let l = utf8.decodeUTF8(ofByteWidth: utf8Width, at: utf8Idx),
          let r = utf16.decodeUTF16(ofByteWidth: utf16Width, at: utf16Idx) else {
          return false
        }
        if (l != r) {
          return false
        }
      }
      utf8Idx &+= utf8Width
      utf16Idx &+= utf16Width
    }
}

@inline(__always) @_effects(readonly)
fileprivate func isEqual(
  bytes lhs: RawSpan,
  bytes rhs: RawSpan
) -> Bool {
  _debugPrecondition(lhs.byteCount > 0)
  if lhs.byteCount != rhs.byteCount {
    return false
  }
  return lhs.withUnsafeBytes { lhsBuffer in
    return rhs.withUnsafeBytes { rhsBuffer in
      return unsafe 0 == _swift_stdlib_memcmp(
        lhsBuffer.baseAddress.unsafelyUnwrapped,
        rhsBuffer.baseAddress.unsafelyUnwrapped,
        lhs.byteCount
      )
    }
  }
}

@_effects(readonly)
fileprivate func isEqual(
  asciiBytes ascii: RawSpan,
  utf16Bytes utf16: RawSpan
) -> Bool {
  if shouldEarlyOut(
    lhsByteCount: ascii.byteCount,
    lhsEncoding: _cocoaASCIIEncoding,
    rhsByteCount: utf16.byteCount,
    rhsEncoding: _cocoaUTF16Encoding
  ) {
    return false
  }
  for asciiIdx in ascii.byteOffsets {
    /*
     Promote ascii to 2 byte rather than truncating utf16 to 1 byte to match
     NSString's behavior, which guards against invalid utf16 contents.
     
     Bounds checking handled by looping over `byteOffsets`
     */
    let l = unsafe UInt16(ascii.unsafeLoadUnaligned(
      fromUncheckedByteOffset: asciiIdx,
      as: UInt8.self
    ))
    // Bounds checking handled by the count * 2 verification earlier
    let r = unsafe utf16.unsafeLoadUnaligned(
      fromUncheckedByteOffset: asciiIdx &* 2,
      as: UInt16.self
    )
    if l != r {
      return false
    }
  }
  return true
}

@_effects(readonly)
@c @_spi(Foundation) public func _swift_unicodeBuffersEqual_nonNormalizing(
  bytes rawLHS: UnsafeRawPointer,
  count lhsCount: Int,
  encoding lhsEnc: UInt,
  bytes rawRHS: UnsafeRawPointer,
  count rhsCount: Int,
  encoding rhsEnc: UInt
) -> Bool {
  let lhs = unsafe RawSpan(_unsafeStart: rawLHS, byteCount: lhsCount)
  let rhs = unsafe RawSpan(_unsafeStart: rawRHS, byteCount: rhsCount)
  let possibleUTF16EdgeCase = lhsEnc != rhsEnc &&
      (lhsEnc == _cocoaUTF16Encoding || rhsEnc == _cocoaUTF16Encoding)
  if let trivialCheck = lhs.contentsAreTriviallyIdentical(
    to: rhs,
    possibleUTF16EdgeCase: possibleUTF16EdgeCase) {
    return trivialCheck
  }
  // ASCII == UTF8 can just use memcmp
  if (lhsEnc == rhsEnc) ||
     (lhsEnc == _cocoaASCIIEncoding && rhsEnc == _cocoaUTF8Encoding) ||
     (lhsEnc == _cocoaUTF8Encoding && rhsEnc == _cocoaASCIIEncoding) {
    return isEqual(bytes: lhs, bytes: rhs)
  }
  if lhsEnc == _cocoaUTF8Encoding && rhsEnc == _cocoaUTF16Encoding {
    return isEqual(utf8Bytes: lhs, utf16Bytes: rhs)
  }
  if lhsEnc == _cocoaUTF16Encoding && rhsEnc == _cocoaUTF8Encoding {
    return isEqual(utf8Bytes: rhs, utf16Bytes: lhs)
  }
  if lhsEnc == _cocoaASCIIEncoding && rhsEnc == _cocoaUTF16Encoding {
    return isEqual(asciiBytes: lhs, utf16Bytes: rhs)
  }
  if lhsEnc == _cocoaUTF16Encoding && rhsEnc == _cocoaASCIIEncoding {
    return isEqual(asciiBytes: rhs, utf16Bytes: lhs)
  }
  fatalError("Unsupported combination of encodings")
}

// See swift_stdlib_connectNSBaseClasses. This method is installed onto
// NSString in Foundation via ObjC runtime shenanigans
@_effects(readonly)
@c internal func _swift_NSStringIsEqualToBytesImpl(
  _ rawSelf: UnsafeRawPointer,
  _ _cmd: UInt, //SEL
  _ otherPtr: UnsafeRawPointer,
  _ otherByteCount: Int,
  _ otherEncoding: UInt
) -> Int8 {
  let selfNS = unsafe unsafeBitCast(rawSelf, to: _CocoaString.self)
  
  let selfCount = _stdlib_binary_CFStringGetLength(selfNS)
  // Even if self isn't UTF16, we got its length in UTF16 already
  if shouldEarlyOut(
    lhsByteCount: otherByteCount,
    lhsEncoding: otherEncoding,
    rhsByteCount: selfCount &* 2,
    rhsEncoding: _cocoaUTF16Encoding
  ) {
    return 0
  }
  
  let result = unsafe withCocoaASCIIPointer(selfNS) { (selfPtr) -> Int8? in
    //We know self is ASCII at this point
    let otherBytes = unsafe RawSpan(_unsafeStart: otherPtr, byteCount: otherByteCount)
    let selfBytes = unsafe Span(_unsafeStart: selfPtr, count: selfCount).bytes
    switch otherEncoding {
    case _cocoaASCIIEncoding, _cocoaUTF8Encoding:
      if otherBytes.isIdentical(to: selfBytes) {
        return 1
      }
      return isEqual(bytes: otherBytes, bytes: selfBytes) ? 1 : 0
    case _cocoaUTF16Encoding:
      return isEqual(asciiBytes: selfBytes, utf16Bytes: otherBytes) ? 1 : 0
    default:
      fatalError("Unsupported combination of encodings")
    }
  }
  if let result {
    return result
  }
  
  if let selfPtr = unsafe _stdlib_binary_CFStringGetCharactersPtr(selfNS) {
    //We know self is UTF16 at this point
    let otherBytes = unsafe RawSpan(_unsafeStart: otherPtr, byteCount: otherByteCount)
    let selfBytes = unsafe Span(_unsafeStart: selfPtr, count: selfCount).bytes
    switch otherEncoding {
    case _cocoaASCIIEncoding:
      return isEqual(asciiBytes: otherBytes, utf16Bytes: selfBytes) ? 1 : 0
    case _cocoaUTF8Encoding:
      return isEqual(utf8Bytes: otherBytes, utf16Bytes: selfBytes) ? 1 : 0
    case _cocoaUTF16Encoding:
      if otherBytes.isIdentical(to: selfBytes) {
        return 1
      }
      return isEqual(bytes: otherBytes, bytes: selfBytes) ? 1 : 0
    default:
      fatalError("Unsupported combination of encodings")
    }
  }
  
  var remainingOtherByteCount = otherByteCount
  var offset = 0

  while true {
    /*
     Larger chunk sizes mean we read more out of `self` even if the difference
     is early, which can be a bit expensive if it has to transcode or doesn't
     do bulk access (the latter is unusual but appears in our benchmark suite).

     Smaller chunk sizes mean more call overhead.
     */
    let chunkSize = Swift.min(64, remainingOtherByteCount)
    // nil = keep looping; .some(equal) = terminate with that equality result
    let chunkResult: Bool? = withUnsafeTemporaryAllocation(
      of: UInt8.self,
      capacity: chunkSize
    ) { tmpBuffer in
      let buffer = UnsafeMutableRawBufferPointer(tmpBuffer)
      var remainingRange = 0 ..< 0
      /*
       Ask self to start transcoding from `offset` until it's either
       run out of contents or filled `chunkSize` bytes of output buffer
       */
      guard let selfChunkByteCount = unsafe _cocoaStringCopyBytes(
        selfNS,
        encoding: otherEncoding,
        into: buffer,
        options: 0,
        UTF16Range: offset ..< selfCount, //the remaining tail of self
        remainingRange: &remainingRange
      ) else {
        return false
      }
      /*
       selfChunkByteCount is now the number of bytes occupied by the next N
       UTF16 code units from self, when transcoded into `otherEncoding`
       */
      if selfChunkByteCount > remainingOtherByteCount {
        return false
      }
      if remainingRange.isEmpty && selfChunkByteCount != remainingOtherByteCount {
        //We've processed all of self. If we don't have enough bytes now we never will
        return false
      }

      let cmpResult = unsafe _swift_stdlib_memcmp(
        otherPtr + (otherByteCount &- remainingOtherByteCount),
        buffer.baseAddress.unsafelyUnwrapped,
        selfChunkByteCount
      )
      if cmpResult != 0 {
        return false
      }
      remainingOtherByteCount &-= selfChunkByteCount
      offset = remainingRange.lowerBound
      if remainingOtherByteCount == 0 {
        return remainingRange.isEmpty
      }
      return nil
    }
    if let chunkResult {
      return chunkResult ? 1 : 0
    }
  }
}

#endif // _runtime(_ObjC)
