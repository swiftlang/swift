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
    _precondition(aRange.location >= 0 && aRange.length >= 0,
                  "Range out of bounds")
    // Note: `count` is counting UTF-8 code units, while `aRange` is measured in
    // UTF-16 offsets. This precondition is a necessary, but not sufficient test
    // for validity. (More precise checks are done in UTF16View._nativeCopy.)
    _precondition(aRange.location + aRange.length <= Int(count),
                  "Range out of bounds")

    let range = unsafe Range(
      _uncheckedBounds: (aRange.location, aRange.location+aRange.length))
    let str = asString
    unsafe str._copyUTF16CodeUnits(
      into: UnsafeMutableBufferPointer(start: buffer, count: range.count),
      range: range)
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

      // At this point we've proven that it is a non-Swift NSString
      let otherUTF16Length = _stdlib_binary_CFStringGetLength(other)

      // CFString will only give us ASCII bytes here, but that's fine.
      // We already handled non-ASCII UTF8 strings earlier since they're Swift.
      if let asciiEqual = unsafe withCocoaASCIIPointer(other, work: { (ascii) -> Bool in
        // UTF16 length == UTF8 length iff ASCII
        if otherUTF16Length == self.count {
          return unsafe (start == ascii || (memcmp(start, ascii, self.count) == 0))
        }
        return false
      }) {
        return asciiEqual ? 1 : 0
      }

      if self.UTF16Length != otherUTF16Length {
        return 0
      }

      /*
      The abstract implementation of -isEqualToString: falls back to -compare:
      immediately, so when we run out of fast options to try, do the same.
      We can likely be more clever here if need be
      */
      return _cocoaStringCompare(self, other) == 0 ? 1 : 0
    }
  }
}

extension __StringStorage {
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) @inline(__always) get {
      return asString.utf16.count // UTF16View special-cases ASCII for us.
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
    let str = asString
    return str.utf16[str._toUTF16Index(offset)]
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
      return asString.utf16.count // UTF16View special-cases ASCII for us.
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
    let str = asString
    return str.utf16[str._toUTF16Index(offset)]
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

  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While __StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }
}

#endif // _runtime(_ObjC)
