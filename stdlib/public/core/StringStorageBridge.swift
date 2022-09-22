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

internal let _cocoaASCIIEncoding:UInt = 1 /* NSASCIIStringEncoding */
internal let _cocoaUTF8Encoding:UInt = 4 /* NSUTF8StringEncoding */
internal let _cocoaNotFound = Int.max

extension Collection {
  @inline(__always)
  @_effects(readonly)
  func _boyerMooreSearch<C>(
    for needle: C, skipTableLookup: (Element) -> Int) -> Range<Index>?
  where C: BidirectionalCollection, C.Element == Element, Element: Hashable {
    let needleCount = needle.count
    guard let initialSearchEnd = index(
      startIndex,
      offsetBy: needleCount,
      limitedBy: endIndex
    ) else {
      return nil
    }
    var searchRange = startIndex ..< initialSearchEnd
    let needleSlice = needle.reversed()
    while true {
      let ourSlice = self[searchRange].reversed()
      
      let maybeMismatch = zip(ourSlice.indices, needleSlice.indices).first {
        ourSlice[$0] != needleSlice[$1]
      }?.0
      guard let mismatch = maybeMismatch else {
        return searchRange
      }
      let skip = skipTableLookup(ourSlice[mismatch])
      guard let newEnd = index(
        searchRange.upperBound,
        offsetBy: skip,
        limitedBy: endIndex
      ) else {
        //went off the end, no match
        return nil
      }
      let newStart = index(
        searchRange.lowerBound,
        offsetBy: skip
      )
      searchRange = newStart ..< newEnd
    }
  }
  
  @_effects(readonly)
  func boyerMooreSearch<C>(for needle: C) -> Range<Index>?
  where C: BidirectionalCollection, C.Element == Element, Element: Hashable {
    if needle.count > count {
      return nil
    }
    if needle.count == count {
      return elementsEqual(needle) ? startIndex ..< endIndex : nil
    }
    var skipTable:[Element : Int] = [:]
    skipTable.reserveCapacity(needle.count)
    var offset = 0
    for element in needle.reversed() {
      skipTable[element] = needle.count - offset
      offset += 1
    }
    return _boyerMooreSearch(for: needle) { skipTable[$0] ?? needle.count }
  }
  
  // 256 bytes
  typealias TableStorage =
  (UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64,
   UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64,
   UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64,
   UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64)
  
  @_effects(readonly)
  func boyerMooreSearch<C>(for needle: C) -> Range<Index>?
  where C: BidirectionalCollection, C.Element == UInt8, Element == UInt8 {
    if needle.count > count {
      return nil
    }
    if needle.count == count {
      return elementsEqual(needle) ? startIndex ..< endIndex : nil
    }
    if needle.count < 256 {
      var skipTableStorage: TableStorage =
      (0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0)
      return withUnsafeMutableBytes(of: &skipTableStorage) { rawBuffer in
        let skipTable = rawBuffer.bindMemory(to: UInt8.self)
        skipTable.initialize(repeating: UInt8(needle.count))
        for (i, c) in needle.reversed().enumerated() {
          skipTable[Int(c)] = UInt8(needle.count - i)
        }
        return _boyerMooreSearch(for: needle) { Int(skipTable[Int($0)]) }
      }
    } else {
      var skipTable = [Int](repeating: needle.count, count: 256)
      for (i, c) in needle.reversed().enumerated() {
        skipTable[Int(c)] = needle.count - i
      }
      return _boyerMooreSearch(for: needle) { skipTable[Int($0)] }
    }
  }
}

struct NSStringUTF16View : BidirectionalCollection {
  typealias Index = Int
  typealias Element = UTF16.CodeUnit
  
  let str: AnyObject
  var ptr: UnsafePointer<UTF16.CodeUnit>? = nil
  let endIndex: Int
  
  init(_ opaque: AnyObject) {
    str = opaque
    endIndex = _stdlib_binary_CFStringGetLength(opaque)
    if let direct = _stdlib_binary_CFStringGetCharactersPtr(opaque) {
      ptr = UnsafePointer(direct)
    }
  }
  
  @inline(__always)
  subscript(position: Int) -> UTF16.CodeUnit {
    guard let direct = ptr else {
      return _cocoaStringSubscript(str, position)
    }
    return UnsafeBufferPointer(start: direct, count: endIndex)[position]
  }
  
  var startIndex: Int {
    0
  }
  
  func index(after i: Index) -> Int {
    return i + 1
  }
  func index(before i: Index) -> Int {
    return i - 1
  }
}

extension String {
  @available(SwiftStdlib 5.6, *)
  @_spi(Foundation)
  public init?(_nativeStorage: AnyObject) {
    let knownOther = _KnownCocoaString(_nativeStorage)
    switch knownOther {
    case .storage:
      self = _unsafeUncheckedDowncast(
        _nativeStorage,
        to: __StringStorage.self
      ).asString
    case .shared:
      self = _unsafeUncheckedDowncast(
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
    _precondition(aRange.location + aRange.length <= Int(count),
                  "Range out of bounds")

    let range = Range(
      _uncheckedBounds: (aRange.location, aRange.location+aRange.length))
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
      outputPtr.initialize(from: start, count: count)
      outputPtr[count] = 0
      return 1
    default:
      return  _cocoaGetCStringTrampoline(self, outputPtr, maxLength, encoding)
    }
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
    return (start == nativeOther.start ||
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
      return _nativeIsEqual(
        _unsafeUncheckedDowncast(other, to: __StringStorage.self))
    case .shared:
      return _nativeIsEqual(
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
      if let asciiEqual = withCocoaASCIIPointer(other, work: { (ascii) -> Bool in
        // UTF16 length == UTF8 length iff ASCII
        if otherUTF16Length == self.count {
          return (start == ascii || (memcmp(start, ascii, self.count) == 0))
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
  
  @inline(__always)
  func _toNSRange(_ indices: Range<String.Index>?) -> _SwiftNSRange {
    return asString._toNSRange(indices)
  }
  
  @inline(__always) var utf8: String.UTF8View { asString.utf8 }
  @inline(__always) var utf16: String.UTF16View { asString.utf16 }

  @_effects(readonly)
  internal func _nativeRange<T:_AbstractStringStorage>(
    of nativeOther: T
  ) -> _SwiftNSRange {
    return _toNSRange(utf8.boyerMooreSearch(for: nativeOther.utf8))
  }
  
  @_effects(readonly)
  internal func _foreignRange(of needle: AnyObject) -> _SwiftNSRange {
    precondition(_isNSString(needle))
    // At this point we've proven that it is a non-Swift NSString
    
    // CFString will only give us ASCII bytes here, but that's fine.
    // We already handled non-ASCII UTF8 strings earlier since they're Swift.
    if let range = withCocoaASCIIPointer(
      needle,
      work: { ptr -> Range<String.Index>? in
        let asciiNeedle = UnsafeBufferPointer(
          start: ptr,
          count: _stdlib_binary_CFStringGetLength(needle)
        )
        return utf8.boyerMooreSearch(for: asciiNeedle)
      }) {
      return _toNSRange(range)
    }
    
    if let range = utf16.boyerMooreSearch(for: NSStringUTF16View(needle)) {
      return _toNSRange(range)
    }
    
    return _toNSRange(nil)
  }
  
  @inline(__always)
  @_effects(readonly)
  internal func _range(of other: AnyObject?) -> _SwiftNSRange {
    guard let other = other, count > 0 else {
      return _toNSRange(nil)
    }
    
    if self === other {
      return _toNSRange(utf16.startIndex ..< utf16.endIndex)
    }
    
    let knownOther = _KnownCocoaString(other)
    switch knownOther {
    case .storage:
      return _nativeRange(
        of: _unsafeUncheckedDowncast(other, to: __StringStorage.self)
      )
    case .shared:
      return _nativeRange(
        of: _unsafeUncheckedDowncast(other, to: __SharedStringStorage.self)
      )
    default:
      return _foreignRange(of: other)
    }
  }
}

extension __StringStorage {
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) @inline(__always) get {
      return utf16.count // UTF16View special-cases ASCII for us.
    }
  }

  @objc
  final internal var hash: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaHashASCIIBytes(start, length: count)
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
    _getCharacters(buffer, aRange)
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return start._asCChar
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
      if isASCII {
        return _cocoaASCIIEncoding
      }
      return _cocoaUTF8Encoding
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
    
  @objc(rangeOfString:)
  @_effects(readonly)
  final internal func range(of other: AnyObject?) -> _SwiftNSRange {
    return _range(of: other)
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
      return utf16.count // UTF16View special-cases ASCII for us.
    }
  }

  @objc
  final internal var hash: UInt {
    @_effects(readonly) get {
      if isASCII {
        return _cocoaHashASCIIBytes(start, length: count)
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
    _getCharacters(buffer, aRange)
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

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    if isASCII {
      return start._asCChar
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
  
  @objc(rangeOfString:)
  @_effects(readonly)
  final internal func range(of other: AnyObject?) -> _SwiftNSRange {
    return _range(of: other)
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
