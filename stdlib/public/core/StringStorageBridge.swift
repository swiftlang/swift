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
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) @inline(__always) get {
      return asString.utf16.count // UTF16View special-cases ASCII for us.
    }
  }

  @objc(hash)
  final internal var hash: UInt {
    @_effects(readonly) get {
      return _getCocoaHash()
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

  @objc
  final internal var fastestEncoding: UInt {
    @_effects(readonly) get {
      return _getFastestEncoding()
    }
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
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) get {
      return asString.utf16.count // UTF16View special-cases ASCII for us.
    }
  }

  @objc(hash)
  final internal var hash: UInt {
    @_effects(readonly) get {
      return _getCocoaHash()
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
    _getCharacters(buffer, aRange)
  }

  @objc(fastestEncoding)
  final internal var fastestEncoding: UInt {
    @_effects(readonly) get {
      return _getFastestEncoding()
    }
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

@_fixed_layout
@usableFromInline
@objc internal final class _SwiftNSMutableString :
  _SwiftNativeNSMutableString, _AbstractStringStorage {
  
  // Swift String always wants to have valid contents, so for cases where
  // mutation of the contents makes them invalid (e.g. deleting half of a
  // surrogate pair) we have an alternate backing store that's just an
  // Array of UTF16, which we largely leave up to NSString to interpret
  private enum Contents {
    case native(String)
    case utf16([UTF16.CodeUnit])
    
    var asString: String {
      @_effects(readonly) @inline(__always) get {
        switch self {
        case .native(str):
          return str
        case .utf16(arr):
          return String(decoding: arr, as: UTF16.self)
        }
      }
    }
    
    var start: UnsafePointer<UInt8>? {
      @_effects(readonly) @inline(__always) get {
        switch self {
        case .native(str):
          let guts = str._guts
          guard !guts.isSmall else { return nil }
          return guts._object.fastUTF8.baseAddress!
        default:
          return nil
        }
      }
    }
    
    var isASCII: Bool {
      @_effects(readonly) @inline(__always) get {
        switch self {
        case .native(str):
          return str._guts.isASCII
        default:
          return false
        }
      }
    }
    
    var UTF16Length: Int {
      @_effects(readonly) @inline(__always) get {
        switch self {
        case .native(str):
          return str.utf16.count // UTF16View special-cases ASCII for us.
        case .utf16(arr):
          return arr.count
        }
      }
    }
    
    @_effects(readonly)
    func character(at offset: Int) -> UInt16 {
      switch self {
      case .native(str):
        return str.utf16[str._toUTF16Index(offset)]
      case .utf16(arr):
        return arr[offset]
      }
    }
    
    func _validUTF16(_ data: UnsafeBufferPointer<UTF16.CodeUnit>) -> Bool {
      var lookingForTrailingSurrogate = false
      for scalar in data {
        if UTF.isTrailSurrogate(scalar) {
          if _fastPath(lookingForTrailingSurrogate) {
            lookingForTrailingSurrogate = false
          } else {
            return false
          }
        }
        if UTF.isLeadSurrogate(scalar) {
          if _fastPath(!lookingForTrailingSurrogate) {
            lookingForTrailingSurrogate = true
          } else {
            return false
          }
        }
      }
      return true
    }
    
    func _switchRepresentationsIfInvalidUTF16(
      replacementBytes: UnsafeBufferPointer<UTF16.CodeUnit>
    ) -> Bool {
      switch self {
      case .native(str):
        if _fastPath(_validUTF16(replacementBytes)) {
          return false
        }
        _contents = .utf16(str.utf16)
        return true
      case .utf16(_):
        return true
      }
    }
    
    func _convertIncomingNSRangeSwitchingRepresentationIfUnaligned(
      _ range: _SwiftNSRange
    ) -> Range<String.Index>? {
      switch self {
      case .native(str):
        let range = str._toUTF16Indices(
          range.location ..< range.location + range.length
        )
        let rs = range.lowerBound
        let re = range.upperBound
        let scalarAligned =
          (rs._isScalarAligned || rs.transcodedOffset == 0) &&
            (re._isScalarAligned || re.transcodedOffset == 0)
        
        if _slowPath(!scalarAligned) {
          _contents = .utf16(str.utf16)
        }
        return range
      case .utf16(_):
        return nil
      }
    }
    
    // Fast mutation path: replacement is known valid, we are known String-backed
    func _replace(
      in nsRange: Range<String.Index>,
      with replacement: String
    ) {
      switch _contents {
      case .native(var str):
        // The next two lines should optimize to an in-place mutation
        str.replaceSubrange(range, with: replacement)
        _contents = .native(str)
      case .utf16(var arr):
        _internalInvariantFailure(
          "Fast path can't be used on UTF16-backed NSMutableString"
        )
      }
    }
    
    // Slow mutation path: replacement may be invalid, we are known Array-backed
    func _replace_slow(
      in nsRange: _SwiftNSRange,
      with utf16Bytes: UnsafeBufferPointer<UTF16.CodeUnit>
    ) {
      switch self {
      case .native(var str):
        _internalInvariantFailure(
          "Slow path can't be used on String-backed NSMutableString"
        )
      case .utf16(var arr):
        let cocoaRange = nsRange.location ..< nsRange.location + nsRange.length
        arr.replaceSubrange(cocoaRange, with: utf16Bytes)
        _contents = .utf16(arr)
      }
    }
    
    // Fast path for append
    func _append(string: String) {
      switch self {
      case .native(var str):
        str.append(string)
        _contents = .native(str)
      case .utf16(var arr):
        arr.append(string.utf16)
        _contents = .utf16(arr)
      }
    }
    
    // Slow path: bytes may be invalid UTF-16, either representation allowed
    func _append(utf16Bytes: UnsafeBufferPointer<UTF16.CodeUnit>) {
      self._switchRepresentationsIfInvalidUTF16(utf16Bytes)
      
      switch self {
      case .native(var str):
        str.append(String(decoding: utf16Bytes, as: UTF16.self))
        _contents = .native(str)
      case .utf16(var arr):
        arr.append(utf16Bytes)
        _contents = .utf16(arr)
      }
    }
  }
  
  private var _contents: Contents
  
  internal init(_ str: String) {
    _contents = .native(str)
    super.init()
    _internalInvariant(_contents._guts.isFastUTF8)
  }
  
  internal init(brokenContents arr: [UTF16.CodeUnit]) {
    _contents = .utf16(arr)
    super.init()
  }
  
  final internal var asString: String {
    @_effects(readonly) @inline(__always) get {
      return _contents.asString
    }
  }
  
  @inline(__always)
  final func withFastUTF8<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    return try asString._guts.withFastUTF8(f)
  }
  
  @inline(__always)
  internal var start: UnsafePointer<UInt8>? {
    return _contents.start
  }
  
  @inline(__always)
  internal var count: Int { return asString.utf8.count }
  
  @inline(__always)
  final internal var isASCII: Bool {
    return _contents.isASCII
  }
  
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) get {
      return _contents.UTF16Length
    }
  }

  @objc(hash)
  final internal var hash: UInt {
    @_effects(readonly) get {
      return _getCocoaHash()
    }
  }

  @objc(characterAtIndex:)
  @_effects(readonly)
  final internal func character(at offset: Int) -> UInt16 {
    return _contents.character(at: offset)
  }

  @objc(getCharacters:range:)
  @_effects(releasenone)
  final internal func getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  ) {
    switch _contents {
    case .native(_):
      _getCharacters(buffer, aRange)
    case .utf16(arr):
      _precondition(aRange.location >= 0 && aRange.length >= 0,
                       "Range out of bounds")
      _precondition(aRange.location + aRange.length <= Int(count),
                       "Range out of bounds")
      let range = Range(
        uncheckedBounds: (aRange.location, aRange.location+aRange.length))
      arr.withUnsafeBufferPointer {
        let toCopy = UnsafeBufferPointer(rebasing: $0[range])
        buffer.initialize(from: toCopy)
      }
    }
  }

  @objc
  final internal var fastestEncoding: UInt {
    @_effects(readonly) get {
      return _getFastestEncoding()
    }
  }

  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8> {
    // This is Cocoa's trick for returning an "autoreleased char *", but using
    // our CoW to make it a bit faster
    let anchor = asString._bridgeToObjectiveCImpl()
    let unmanagedAnchor = Unmanaged.passRetained(anchor)
    _ = unmanagedAnchor.autorelease()
    return _cocoaStringGetUTF8Pointer(anchor)
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
  final internal func isEqual(to other:AnyObject?) -> Int8 {
    return _isEqual(other)
  }

  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While __StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return asString._bridgeToObjectiveCImpl()
  }
  
  @objc(copy)
  final internal func copy() -> AnyObject {
    return asString._bridgeToObjectiveCImpl()
  }
  
  @objc(mutableCopyWithZone:)
  final internal func mutableCopy(with zone: _SwiftNSZone?) -> AnyObject {
    switch _contents {
    case .native(str):
      return _SwiftNSMutableString(str)
    case .utf16(arr):
      return _SwiftNSMutableString(brokenContents: arr)
    }
  }
  
  @objc(mutableCopy)
  final internal func mutableCopy() -> AnyObject {
    switch _contents {
    case .native(str):
      return _SwiftNSMutableString(str)
    case .utf16(arr):
      return _SwiftNSMutableString(brokenContents: arr)
    }
  }
  
  /*
   NSString, unlike Swift String, does not require its contents to be valid
   Unicode bytes. For example, you can simply replace the first character in a
   surrogate pair with something incorrect.
   
   This presents awkwardness for us when people try to build up a mutable string
   character by character, leaving it briefly invalid.
   
   To handle that, we have two "modes" for _SwiftNSMutableString:
    • String-backed, where we know we have valid contents
    • Byte-Array-backed, where we're just a buffer
   
   We switch from String to Byte-Array backing when we encounter either of the
   following situations:
    • The range of a mutation splits a unicode scalar
    • The replacement bytes of a mutation are not themselves valid UTF-16
   */
  
  @objc(replaceCharactersInRange:withString:)
  final func replaceCharacters(
    in nsRange: _SwiftNSRange,
    with aString: _CocoaString?
  ) {
    let cocoaString = aString ?? ""._bridgeToObjectiveCImpl()
    
    guard let range = _contents._convertIncomingNSRangeSwitchingRepresentationIfUnaligned(
      nsRange
    ) else {
      _withCocoaStringUTF16Contents(cocoaString) {
        _contents._replace_slow(in: nsRange, with: $0)
      }
      return
    }
    
    // At this point we know that the following are true:
    // • The range is scalar aligned
    // • We are String-backed
    // • The replacement is non-nil
    
    // If `aString` is actually a bridged Swift String or something else
    // we can be sure is valid and bridge quickly, get a String to work with
    let didFastPath = _withTemporaryBridgedCocoaString(cocoaString) {
      _contents._replace(in: range, with: $0)
    }
    
    if _slowPath(!didFastPath) {
      _withCocoaStringUTF16Contents(cocoaString) {
        _contents._switchRepresentationsIfInvalidUTF16($0)
        if case .native(_) = contents {
          _contents._replace(
            in: range,
            with: String(decoding: $0, as: UTF16.self)
          )
        } else {
          // Fall back to the slow path because we found invalid UTF-16
          _contents._replace_slow(in: nsRange, with: $0)
        }
      }
    }
  }
  
  // We don't implement -appendString: in terms of
  // -replaceCharacters:inRange:with:, because getting the UTF-16 end index
  // requires constructing breadcrumbs, which we would like to avoid
  @objc(appendString:)
  final func appendString(_ aString: _CocoaString?) {
    guard let aString = aString else {
        return
    }

    let didFastPath = _withTemporaryBridgedCocoaString(aString) {
      _contents._append(string: $0)
    }
    
    if _slowPath(!didFastPath) {
      _withCocoaStringUTF16Contents(cocoaString) {
        _contents._append(utf16Bytes: $0)
       }
    }
  }
  
  @objc(deleteCharactersInRange:)
  final func deleteCharacters(in range: _SwiftNSRange) {
    replaceCharacters(in: range, with: nil)
  }
  
  @objc(insertString:atIndex:)
  final func insert(str: _CocoaString?, at offset: Int) {
    replaceCharacters(in: _SwiftNSRange(location: offset, length: 0), with: str)
  }
}

#endif // _runtime(_ObjC)
