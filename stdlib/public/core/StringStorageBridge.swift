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

@_effects(readonly)
private func _isNSString(_ str:AnyObject) -> UInt8 {
  return _swift_stdlib_isNSString(str)
}

internal let _cocoaASCIIEncoding:UInt = 1 /* NSASCIIStringEncoding */
internal let _cocoaUTF8Encoding:UInt = 4 /* NSUTF8StringEncoding */

// ObjC interfaces.
extension _AbstractStringStorage {
  @inline(__always)
  @_effects(releasenone)
  internal func _getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>, _ aRange: _SwiftNSRange
  ) {
    _precondition(aRange.location >= 0 && aRange.length >= 0,
                  "Range out of bounds")
    _precondition(aRange.location + aRange.length <= Int(count),
                  "Range out of bounds")

    let range = Range(
      uncheckedBounds: (aRange.location, aRange.location+aRange.length))
    let str = asString
    str._copyUTF16CodeUnits(
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
         (_cocoaUTF8Encoding, _):
      guard maxLength >= count + 1 else { return 0 }
      return withFastUTF8 {
        outputPtr.initialize(from: $0.baseAddress._unsafelyUnwrappedUnchecked,
                             count: $0.count)
        outputPtr[count] = 0
        return 1
      }
    default:
      return  _cocoaGetCStringTrampoline(self, outputPtr, maxLength, encoding)
    }
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _getCocoaHash() -> UInt {
    if isASCII {
      return withFastUTF8 {
        return _cocoaHashASCIIBytes($0.baseAddress._unsafelyUnwrappedUnchecked,
                                    length: $0.count)
      }
    }
    return _cocoaHashString(self)
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _getFastestEncoding() -> UInt {
    if isASCII {
      return _cocoaASCIIEncoding
    }
    return _cocoaUTF8Encoding
  }

  @inline(__always)
  @_effects(readonly)
  internal func _cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    switch (encoding, isASCII) {
    case (_cocoaASCIIEncoding, true),
         (_cocoaUTF8Encoding, _):
      return start
    default:
      return _cocoaCStringUsingEncodingTrampoline(self, encoding)
    }
  }

  @_effects(readonly)
  internal func _nativeIsEqual<T:_AbstractStringStorage>(
    _ nativeOther: T
  ) -> Int8 {
    if count != nativeOther.count {
      return 0
    }
    return withFastUTF8 { (buffer) in
      let ourStart = buffer.baseAddress._unsafelyUnwrappedUnchecked
      return nativeOther.withFastUTF8 { (otherBuffer) in
        let otherStart = otherBuffer.baseAddress._unsafelyUnwrappedUnchecked
        return (ourStart == otherStart ||
          (memcmp(ourStart, otherStart, count) == 0)) ? 1 : 0
      }
    }
  }

  @inline(__always)
  @_effects(readonly)
  internal func _isEqual(_ other: AnyObject?) -> Int8 {
    guard let other = other else {
      return 0
    }
    
    guard self !== other else {
      return 1
    }
    
    // Handle the case where both strings were bridged from Swift.
    // We can't use String.== because it doesn't match NSString semantics.
    switch _KnownCocoaString(other) {
    case .storage(let storage):
      return _nativeIsEqual(storage)
    case .shared(let storage):
      return _nativeIsEqual(storage)
    case .mutable(let storage):
      return _nativeIsEqual(storage)
      #if !(arch(i386) || arch(arm))
    case .tagged(let otherStr):
      fallthrough
      #endif
    case .cocoa(let otherStr):
      // We're allowed to crash, but for compatibility reasons NSCFString allows
      // non-strings here.
      guard _isNSString(otherStr) == 1 else {
        return 0
      }
      
      // At this point we've proven that it is an NSString of some sort, but not
      // one of ours.
      defer { _fixLifetime(otherStr) }
      
      let otherUTF16Length = _stdlib_binary_CFStringGetLength(otherStr)
      
      // CFString will only give us ASCII bytes here, but that's fine.
      // We already handled non-ASCII UTF8 strings earlier since they're Swift.
      guard let otherStart = _cocoaASCIIPointer(otherStr) else {
        if UTF16Length != otherUTF16Length {
          return 0
        }
        
        /*
         The abstract implementation of -isEqualToString: falls back to compare:
         immediately, so when we run out of fast options to try, do the same.
         We can likely be more clever here if need be
         */
        return _cocoaStringCompare(self, other) == 0 ? 1 : 0
      }
      
      //We know that otherUTF16Length is also its byte count at this point
      guard count == otherUTF16Length else {
        return 0
      }
      
      return withFastUTF8 {
        let ourStart = $0.baseAddress._unsafelyUnwrappedUnchecked
        return (ourStart == otherStart ||
          memcmp(ourStart, otherStart, count) == 0) ? 1 : 0
      }
    }
  }
  
}

extension __StringStorage {
  @objc(length) @_effects(readonly) @inline(__always)
  final internal var UTF16Length: Int {
    return asString.utf16.count // UTF16View special-cases ASCII for us.
  }

  @objc @_effects(readonly)
  final internal var hash: UInt {
    return _getCocoaHash()
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
    _getCharacters(buffer, aRange)
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return start._unsafelyUnwrappedUnchecked._asCChar
    }
    return nil
  }

  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8>? {
    return start
  }

  @objc(cStringUsingEncoding:)
  @_effects(readonly)
  final internal func cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    return _cString(encoding: encoding)
  }

  @objc(getCString:maxLength:encoding:)
  @_effects(releasenone)
  final internal func getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, maxLength: Int, encoding: UInt
  ) -> Int8 {
    return _getCString(outputPtr, maxLength, encoding)
  }

  @objc @_effects(readonly)
  final internal var fastestEncoding: UInt {
    return _getFastestEncoding()
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
  
  @objc(copy)
  final internal func copy() -> AnyObject {
    return self
  }
  
  @objc(mutableCopyWithZone:)
  final internal func mutableCopy(with zone: _SwiftNSZone?) -> AnyObject {
    return _SwiftNSMutableString(self.asString)
  }
  
  @objc(mutableCopy)
  final internal func mutableCopy() -> AnyObject {
    return _SwiftNSMutableString(self.asString)
  }
  
}

extension __SharedStringStorage {
  @objc(length) @_effects(readonly)
  final internal var UTF16Length: Int {
    return asString.utf16.count // UTF16View special-cases ASCII for us.
  }

  @objc @_effects(readonly)
  final internal var hash: UInt {
    return _getCocoaHash()
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
    _getCharacters(buffer, aRange)
  }

  @objc @_effects(readonly)
  final internal var fastestEncoding: UInt {
    return _getFastestEncoding()
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return start._unsafelyUnwrappedUnchecked._asCChar
    }
    return nil
  }

  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8>? {
    return start
  }

  @objc(cStringUsingEncoding:)
  @_effects(readonly)
  final internal func cString(encoding: UInt) -> UnsafePointer<UInt8>? {
    return _cString(encoding: encoding)
  }

  @objc(getCString:maxLength:encoding:)
  @_effects(releasenone)
  final internal func getCString(
    _ outputPtr: UnsafeMutablePointer<UInt8>, maxLength: Int, encoding: UInt
  ) -> Int8 {
    return _getCString(outputPtr, maxLength, encoding)
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
  
  @objc(copy)
  final internal func copy() -> AnyObject {
    return self
  }
  
  @objc(mutableCopyWithZone:)
  final internal func mutableCopy(with zone: _SwiftNSZone?) -> AnyObject {
    return _SwiftNSMutableString(self.asString)
  }
  
  @objc(mutableCopy)
  final internal func mutableCopy() -> AnyObject {
    return _SwiftNSMutableString(self.asString)
  }
  
}

#endif // _runtime(_ObjC)
