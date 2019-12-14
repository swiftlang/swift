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
  
  @inline(__always)
  @_effects(readonly)
  internal func _nativeIsEqual(
    _ ourStart: UnsafePointer<UInt8>,
    _ otherStart: UnsafePointer<UInt8>,
    _ count: Int
  ) -> Int8 {
    return (ourStart == otherStart ||
    (memcmp(ourStart, otherStart, count) == 0)) ? 1 : 0
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _nativeIsEqual<T:_AbstractStringStorage>(
    _ ourStart: UnsafePointer<UInt8>,
    _ nativeOther: T,
    _ count: Int
  ) -> Int8 {
    if let otherStart = nativeOther.start {
      return _nativeIsEqual(ourStart, otherStart, count)
    }
    return nativeOther.withFastUTF8 {
      let otherStart = $0.baseAddress._unsafelyUnwrappedUnchecked
      return _nativeIsEqual(ourStart, otherStart, count)
    }
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _nativeIsEqual<T:_AbstractStringStorage>(
    _ nativeOther: T
  ) -> Int8 {
    if count != nativeOther.count {
      return 0
    }
    if let ourStart = start {
      return _nativeIsEqual(ourStart, nativeOther, count)
    }
    return withFastUTF8 {
      let ourStart = $0.baseAddress._unsafelyUnwrappedUnchecked
      return _nativeIsEqual(ourStart, nativeOther, count)
    }
  }
  
  @_effects(readonly)
  internal func _fallbackIsEqual(
    _ otherStr: AnyObject,
    _ isNSString:Bool
  ) -> Int8 {
    // We're allowed to crash, but for compatibility reasons NSCFString allows
    // non-strings here.
    guard isNSString || _isNSString(otherStr) == 1 else {
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
      return _cocoaStringCompare(self, otherStr) == 0 ? 1 : 0
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
      //should pass true here but need to exclude non-string tagged pointers
      return _fallbackIsEqual(otherStr, false)
#endif
    case .cocoa(let otherStr):
      return _fallbackIsEqual(otherStr, false)
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

// Swift String always wants to have valid contents, so for cases where
// mutation of the contents makes them invalid (e.g. deleting half of a
// surrogate pair) we have an alternate backing store that's just an
// Array of UTF16, which we largely leave up to NSString to interpret
// We smuggle this into _SwiftNSMutableString by storing it as a foreign String
@objc internal final class _InvalidUTF16StringStorage : __SwiftNativeNSString {
  var contents: ContiguousArray<UTF16.CodeUnit>
  
  init(brokenContents arr: ContiguousArray<UTF16.CodeUnit>) {
    contents = arr
    super.init()
  }
  
  @objc(characterAtIndex:)
  @_effects(readonly)
  final internal func character(at offset: Int) -> UInt16 {
    return contents[offset]
  }
  
  @objc(length)
  internal var UTF16Length : Int {
    @_effects(readonly) get {
      return contents.count
    }
  }
  
  @objc(isEqualToString:)
  @_effects(readonly)
  final internal func isEqual(toString other:AnyObject?) -> Int8 {
    guard let other = other else {
      return 0
    }
    
    guard self !== other else {
      return 1
    }
    
    guard _isNSString(other) == 1 else {
      return 0
    }
    
    if UTF16Length != _stdlib_binary_CFStringGetLength(other) {
      return 0
    }
    
    /*
     The abstract implementation of -isEqualToString: falls back to compare:
     immediately, so when we run out of fast options to try, do the same.
     We can likely be more clever here if need be
     */
    return _cocoaStringCompare(self, other) == 0 ? 1 : 0
  }
  
  @objc(isEqual:)
  @_effects(readonly)
  final internal func isEqual(to other:AnyObject?) -> Int8 {
    return isEqual(toString: other)
  }

  @objc(getCharacters:range:)
  @_effects(releasenone)
  final internal func getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  ) {
    let range = aRange.location ..< aRange.location+aRange.length

    let bufPtr = UnsafeMutableBufferPointer(
      start: buffer,
      count: range.count
    )
    contents[range]._copyContents(initializing: bufPtr)
  }
}

@usableFromInline
@objc internal final class _SwiftNSMutableString :
  _SwiftNativeNSMutableString, _AbstractStringStorage {
  
  private var contents: String
    
  internal init(_ str: String) {
    contents = str
    super.init()
    _internalInvariant(asString._guts.isFastUTF8)
  }
  
  internal init(brokenContents arr: ContiguousArray<UTF16.CodeUnit>) {
    contents = ""
    super.init()
    _switchToUTF16Backing(arr)
  }
  
  final internal var brokenStorage: _InvalidUTF16StringStorage? {
    @_effects(readonly) @inline(__always) get {
      if _slowPath(contents._guts.isForeign) {
        return contents._guts._object.cocoaObject as? _InvalidUTF16StringStorage
      }
      return nil
    }
  }
  
  final internal var asString: String {
    @_effects(readonly) @inline(__always) get {
      return contents
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
    @_effects(readonly) get {
      let guts = contents._guts
      guard !guts.isSmall else { return nil }
      return guts._object.fastUTF8.baseAddress
    }
  }
  
  @inline(__always)
  internal var count: Int {
    @_effects(readonly) get {
      return asString.utf8.count
    }
  }
  
  @inline(__always)
  final internal var isASCII: Bool {
    @_effects(readonly) get {
      return contents._guts.isASCII
    }
  }
  
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) get {
      return brokenStorage?.UTF16Length ?? contents.utf16.count
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
    return brokenStorage?.character(at: offset) ??
      contents.utf16[contents._toUTF16Index(offset)]
  }

  @objc(getCharacters:range:)
  @_effects(releasenone)
  final internal func getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  ) {
    if let storage = brokenStorage {
      storage.getCharacters(buffer, range: aRange)
    } else {
      return _getCharacters(buffer, aRange)
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
  final internal func isEqual(toString other:AnyObject?) -> Int8 {
    return brokenStorage?.isEqual(to: other) ?? _isEqual(other)
  }
  
  @objc(isEqual:)
  @_effects(readonly)
  final internal func isEqual(to other:AnyObject?) -> Int8 {
    return brokenStorage?.isEqual(to: other) ?? _isEqual(other)
  }

  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    return brokenStorage ?? contents._bridgeToObjectiveCImpl()
  }
  
  @objc(copy)
  final internal func copy() -> AnyObject {
    return brokenStorage ?? contents._bridgeToObjectiveCImpl()
  }
  
  @objc(mutableCopyWithZone:)
  final internal func mutableCopy(with zone: _SwiftNSZone?) -> AnyObject {
    if let broken = brokenStorage {
      return _SwiftNSMutableString(brokenContents: broken.contents)
    }
    return _SwiftNSMutableString(contents)
  }
  
  @objc(mutableCopy)
  final internal func mutableCopy() -> AnyObject {
    if let broken = brokenStorage {
      return _SwiftNSMutableString(brokenContents: broken.contents)
    }
    return _SwiftNSMutableString(contents)
}
  
  /*
   NSString, unlike Swift String, does not require its contents to be valid
   Unicode bytes. For example, you can simply replace the first character in a
   surrogate pair with something incorrect.
   
   This presents awkwardness for us when people try to build up a mutable string
   character by character, leaving it briefly invalid.
   
   To handle that, we have two "modes" for _SwiftNSMutableString:
    • String-backed, where we know we have valid contents
    • UTF16-Codepoint-Array-backed, where we're just a buffer
   
   We switch from String to UTF16-Codepoint-Array backing when we encounter
   either of the following situations:
    • The range of a mutation splits a unicode scalar
    • The replacement bytes of a mutation are not themselves valid UTF-16
 */
  func _switchToUTF16Backing<T>(_ data: T)
    where T:Sequence, T.Element == UTF16.CodeUnit {
    let arr = ContiguousArray(data)
    let broken = _InvalidUTF16StringStorage(brokenContents: arr) as AnyObject
    let length = _stdlib_binary_CFStringGetLength(broken)
    contents = String(
      _StringGuts(
        cocoa: broken,
        providesFastUTF8: false,
        isASCII: false,
        length: length
      )
    )
  }

  func _convertIncomingNSRangeSwitchingRepresentationIfUnaligned(
    _ range: _SwiftNSRange
  ) -> Range<String.Index>? {
    guard brokenStorage == nil else { return nil }
    
    let range = contents._toUTF16Indices(
      range.location ..< range.location + range.length
    )
    let rs = range.lowerBound
    let re = range.upperBound
    let scalarAligned =
      (rs._isScalarAligned || rs.transcodedOffset == 0) &&
        (re._isScalarAligned || re.transcodedOffset == 0)
    
    if _slowPath(!scalarAligned) {
      _switchToUTF16Backing(contents.utf16)
      return nil
    }
    return range
  }

  static func _validUTF16(_ data: UnsafeBufferPointer<UTF16.CodeUnit>) -> Bool {
    var lookingForTrailingSurrogate = false
    for scalar in data {
      if UTF16.isTrailSurrogate(scalar) {
        if _fastPath(lookingForTrailingSurrogate) {
          lookingForTrailingSurrogate = false
        } else {
          return false
        }
      }
      if UTF16.isLeadSurrogate(scalar) {
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
    _ replacementBytes: UnsafeBufferPointer<UTF16.CodeUnit>
  ) {
    guard brokenStorage == nil else { return }
    if !_SwiftNSMutableString._validUTF16(replacementBytes) {
      _switchToUTF16Backing(contents.utf16)
    }
  }
  
  // Fast mutation path: replacement is known valid, we are known String-backed
  func _replace(
    in range: Range<String.Index>,
    with replacement: String
  ) {
    guard brokenStorage == nil else {
      _internalInvariantFailure(
         "Fast path can't be used on UTF16-backed NSMutableString"
       )
    }
    
    contents.replaceSubrange(range, with: replacement)
    //make sure we're in a native representation
    contents.reserveCapacity(contents.utf8.count)
  }
  
  // Slow mutation path: replacement may be invalid, we are known Array-backed
  func _replace_slow(
    in nsRange: _SwiftNSRange,
    with utf16Bytes: UnsafeBufferPointer<UTF16.CodeUnit>
  ) {
    guard let storage = brokenStorage else {
      _internalInvariantFailure(
            "Slow path can't be used on String-backed NSMutableString"
      )
    }
    let cocoaRange = nsRange.location ..< nsRange.location + nsRange.length
    var backing = storage.contents
    backing.replaceSubrange(cocoaRange, with: utf16Bytes)
    _switchToUTF16Backing(backing)
  }
  
  // Fast path for append
  func _append(string: String) {
    if let storage = brokenStorage {
      var backing = storage.contents
      backing.append(contentsOf: string.utf16)
      _switchToUTF16Backing(backing)
    } else {
      contents.append(string)
      //make sure we're in a native representation
      contents.reserveCapacity(contents.utf8.count)
    }
  }
  
  // Slow path: bytes may be invalid UTF-16, either representation allowed
  func _append(utf16Bytes: UnsafeBufferPointer<UTF16.CodeUnit>) {
    _switchRepresentationsIfInvalidUTF16(utf16Bytes)
    if let storage = brokenStorage {
      var backing = storage.contents
      backing.append(contentsOf: utf16Bytes)
      _switchToUTF16Backing(backing)
    } else {
      contents.append(String(decoding: utf16Bytes, as: UTF16.self))
      //make sure we're in a native representation
      contents.reserveCapacity(contents.utf8.count)
    }
  }

  @objc(replaceCharactersInRange:withString:)
  final func replaceCharacters(
    in nsRange: _SwiftNSRange,
    with aString: _CocoaString?
  ) {
    let cocoaString = aString ?? ""._bridgeToObjectiveCImpl()
    
    guard let range = _convertIncomingNSRangeSwitchingRepresentationIfUnaligned(
      nsRange
    ) else {
      _withCocoaStringUTF16Contents(cocoaString) {
        _replace_slow(in: nsRange, with: $0)
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
      _replace(in: range, with: $0)
    }
    
    if _slowPath(!didFastPath) {
      _withCocoaStringUTF16Contents(cocoaString) {
        _switchRepresentationsIfInvalidUTF16($0)
        if brokenStorage == nil {
          _replace(
            in: range,
            with: String(decoding: $0, as: UTF16.self)
          )
        } else {
          // Fall back to the slow path because we found invalid UTF-16
          _replace_slow(in: nsRange, with: $0)
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
      _append(string: $0)
    }
    
    if _slowPath(!didFastPath) {
      _withCocoaStringUTF16Contents(aString) {
        _append(utf16Bytes: $0)
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
