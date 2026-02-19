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

  @_effects(readonly)
  internal func _nativeIsEqual<T:_AbstractStringStorage>(
    _ nativeOther: T
  ) -> Int8 {
    if count != nativeOther.count {
      return 0
    }
    return unsafe (start == nativeOther.start ||
      (memcmp(start, nativeOther.start, count) == 0)) ? 1 : 0
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _isEqualToBuffer(
    ptr: UnsafeRawPointer,
    count otherCount: Int,
    encoding: UInt
  ) -> Bool {
    let ourEncoding = if isASCII {
      _cocoaASCIIEncoding
    } else {
      _cocoaUTF8Encoding
    }
    return unsafe _swift_unicodeBuffersEqual_nonNormalizing(
      bytes: start,
      count: count,
      encoding: ourEncoding,
      bytes: ptr,
      count: otherCount,
      encoding: encoding
    )
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
      return unsafe _nativeIsEqual(
        _unsafeUncheckedDowncast(other, to: __StringStorage.self))
    case .shared:
      return unsafe _nativeIsEqual(
        _unsafeUncheckedDowncast(other, to: __SharedStringStorage.self))
    default:
      // We're allowed to crash, but for compatibility reasons NSCFString allows
      // non-strings here.
      if !_isNSString(other) {
        return 0
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
      return utf16.count
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
      return utf16.count
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

// See swift_stdlib_connectNSBaseClasses. This method is installed onto
// NSString in Foundation via ObjC runtime shenanigans
@_effects(releasenone)
@c internal func _swift_NSStringIsEqualToBytesImpl(
  _ ns: UnsafeRawPointer,
  _ _cmd: UInt, //SEL
  _ lhsPtr: UnsafeRawPointer,
  _ lhsByteCount: Int,
  _ lhsEncoding: UInt
) -> Int8 {
  let ns = unsafe unsafeBitCast(ns, to: _CocoaString.self)
  defer { _fixLifetime(ns) }
  
  let rhsCount = _stdlib_binary_CFStringGetLength(ns)
    
  let tryASCIIRHS = { () -> Int8? in
    return unsafe withCocoaASCIIPointer(ns) { (rhsPtr) -> Int8? in
      return unsafe _swift_unicodeBuffersEqual_nonNormalizing(
        bytes: lhsPtr,
        count: lhsByteCount,
        encoding: lhsEncoding,
        bytes: rhsPtr,
        count: rhsCount,
        encoding: _cocoaASCIIEncoding
      ) ? 1 : 0
    }
  }
  
  let tryUTF16RHS = { () -> Int8? in
    if let rhsPtr = unsafe _stdlib_binary_CFStringGetCharactersPtr(ns) {
      return unsafe _swift_unicodeBuffersEqual_nonNormalizing(
        bytes: lhsPtr,
        count: lhsByteCount,
        encoding: lhsEncoding,
        bytes: rhsPtr,
        count: rhsCount,
        encoding: _cocoaUTF16Encoding
      ) ? 1 : 0
    }
    return nil
  }
  
  //TODO: figure out how to de-duplicate these length checks with the ones in StringComparison.swift
  switch lhsEncoding {
  case _cocoaASCIIEncoding:
    // One LHS byte per UTF16 code unit
    if rhsCount != lhsByteCount {
      return 0
    }
    if let result = tryASCIIRHS() {
      return result
    }
    if let result = tryUTF16RHS() {
      return result
    }
  case _cocoaUTF8Encoding:
    // Best case size change for UTF8 -> UTF16 is 3 bytes -> 1 code unit
    if rhsCount < lhsByteCount / 3 {
      return 0
    }
    // Worst case size change for UTF8 -> UTF16 is 1 byte1 -> 1 code unit
    if rhsCount > lhsByteCount * 2 {
      return 0
    }
    if let result = tryASCIIRHS() {
      return result
    }
    if let result = tryUTF16RHS() {
      return result
    }
  case _cocoaUTF16Encoding:
    // Two LHS bytes per UTF16 code unit
    if rhsCount != lhsByteCount * 2 {
      return 0
    }
    if let result = tryUTF16RHS() {
      return result
    }
    if let result = tryASCIIRHS() {
      return result
    }
  default:
    fatalError("Unsupported encoding")
  }
    
  var remainingLHSByteCount = lhsByteCount
  var offset = 0
  
  enum ChunkResult {
    case equal
    case nonequal
    case `continue`
  }
  
  let tryCopyingChunk = { () -> ChunkResult in
    /*
     Larger chunk sizes mean we read more out of `rhs` even if the difference
     is early, which can be a bit expensive if it has to transcode or doesn't
     do bulk access (the latter is unusual but appears in our benchmark suite).
     
     Smaller chunk sizes mean more call overhead.
     */
    let chunkSize = Swift.min(32, remainingLHSByteCount)
    return unsafe withUnsafeTemporaryAllocation(
      of: UInt8.self,
      capacity: chunkSize
    ) { tmpBuffer in
      let buffer = UnsafeMutableRawBufferPointer(tmpBuffer)
      var remainingRange = 0 ..< 0
      /*
       Ask rhs to start transcoding from `offset` until it's either
       run out of contents or filled `chunkSize` bytes of output buffer
       */
      guard let rhsChunkByteCount = unsafe _cocoaStringCopyBytes(
        ns,
        encoding: lhsEncoding,
        into: buffer,
        options: 0,
        UTF16Range: offset ..< rhsCount, //the remaining tail of rhs
        remainingRange: &remainingRange
      ) else {
        return .nonequal
      }
      /*
       rhsChunkByteCount is now the number of bytes occupied by the next
       N UTF16 code units from rhs, when transcoded into `lhsEncoding`
       */
      if rhsChunkByteCount > remainingLHSByteCount {
        return .nonequal
      }
      if remainingRange.isEmpty && rhsChunkByteCount != remainingLHSByteCount {
        //We've processed all of RHS. If we don't have enough bytes now we never will
        return .nonequal
      }
      
      remainingLHSByteCount &-= rhsChunkByteCount
      offset = remainingRange.lowerBound
      
      let result = unsafe _swift_stdlib_memcmp(
        lhsPtr + (lhsByteCount &- remainingLHSByteCount),
        buffer.baseAddress.unsafelyUnwrapped,
        rhsChunkByteCount
      )
      if result != 0 {
        return .nonequal
      }
      return remainingLHSByteCount == 0 ? .equal : .continue
    }
  }
  
  while true {
    switch tryCopyingChunk() {
    case .equal:
      return 1
    case .nonequal:
      return 0
    default:
      continue
    }
  }
}

#endif // _runtime(_ObjC)
