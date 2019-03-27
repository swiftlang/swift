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

/// Effectively an untyped NSString that doesn't require foundation.
@usableFromInline
internal typealias _CocoaString = AnyObject

#if _runtime(_ObjC)
// Swift's String bridges NSString via this protocol and these
// variables, allowing the core stdlib to remain decoupled from
// Foundation.

@usableFromInline // @testable
@_effects(releasenone)
internal func _stdlib_binary_CFStringCreateCopy(
  _ source: _CocoaString
) -> _CocoaString {
  let result = _swift_stdlib_CFStringCreateCopy(nil, source) as AnyObject
  return result
}

@usableFromInline // @testable
@_effects(readonly)
internal func _stdlib_binary_CFStringGetLength(
  _ source: _CocoaString
) -> Int {
  return _swift_stdlib_CFStringGetLength(source)
}

@usableFromInline // @testable
@_effects(readonly)
internal func _stdlib_binary_CFStringGetCharactersPtr(
  _ source: _CocoaString
) -> UnsafeMutablePointer<UTF16.CodeUnit>? {
  return UnsafeMutablePointer(
    mutating: _swift_stdlib_CFStringGetCharactersPtr(source))
}

/// Copies a slice of a _CocoaString into contiguous storage of sufficient
/// capacity.
@_effects(releasenone)
internal func _cocoaStringCopyCharacters(
  from source: _CocoaString,
  range: Range<Int>,
  into destination: UnsafeMutablePointer<UTF16.CodeUnit>
) {
  _swift_stdlib_CFStringGetCharacters(
    source,
    _swift_shims_CFRange(location: range.lowerBound, length: range.count),
    destination)
}

@_effects(readonly)
internal func _cocoaStringSubscript(
  _ target: _CocoaString, _ position: Int
) -> UTF16.CodeUnit {
  let cfSelf: _swift_shims_CFStringRef = target
  return _swift_stdlib_CFStringGetCharacterAtIndex(cfSelf, position)
}

@_effects(readonly)
internal func _cocoaStringCompare(
  _ string: _CocoaString, _ other: _CocoaString
) -> Int {
  let cfSelf: _swift_shims_CFStringRef = string
  let cfOther: _swift_shims_CFStringRef = other
  return _swift_stdlib_CFStringCompare(cfSelf, cfOther)
}

@_effects(readonly)
internal func _cocoaHashString(
  _ string: _CocoaString
) -> UInt {
  return _swift_stdlib_CFStringHashNSString(string)
}

@_effects(readonly)
internal func _cocoaHashASCIIBytes(
  _ bytes: UnsafePointer<UInt8>, length: Int
) -> UInt {
  return _swift_stdlib_CFStringHashCString(bytes, length)
}

// These "trampolines" are effectively objc_msgSend_super.
// They bypass our implementations to use NSString's.

@_effects(readonly)
internal func _cocoaCStringUsingEncodingTrampoline(
  _ string: _CocoaString, _ encoding: UInt
) -> UnsafePointer<UInt8>? {
  return _swift_stdlib_NSStringCStringUsingEncodingTrampoline(string, encoding)
}

@_effects(releasenone)
internal func _cocoaGetCStringTrampoline(
  _ string: _CocoaString,
  _ buffer: UnsafeMutablePointer<UInt8>,
  _ maxLength: Int,
  _ encoding: UInt
) -> Int8 {
  return Int8(_swift_stdlib_NSStringGetCStringTrampoline(
    string, buffer, maxLength, encoding))
}

//
// Conversion from NSString to Swift's native representation.
//

private var kCFStringEncodingASCII : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x0600 }
}

private var kCFStringEncodingUTF8 : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x8000100 }
}

#if !(arch(i386) || arch(arm))
// Resiliently write a tagged _CocoaString's contents into a buffer.
@_effects(releasenone) // @opaque
internal func _bridgeTagged(
  _ cocoa: _CocoaString,
  intoUTF8 bufPtr: UnsafeMutableBufferPointer<UInt8>
) -> Int? {
  _internalInvariant(_isObjCTaggedPointer(cocoa))
  let ptr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
  let length = _stdlib_binary_CFStringGetLength(cocoa)
  _internalInvariant(length <= _SmallString.capacity)
  var count = 0
  let numCharWritten = _swift_stdlib_CFStringGetBytes(
    cocoa, _swift_shims_CFRange(location: 0, length: length),
    kCFStringEncodingUTF8, 0, 0, ptr, bufPtr.count, &count)
  return length == numCharWritten ? count : nil
}
#endif

@_effects(releasenone) // @opaque
internal func _cocoaUTF8Pointer(_ str: _CocoaString) -> UnsafePointer<UInt8>? {
  // TODO(String bridging): Is there a better interface here? This requires nul
  // termination and may assume ASCII.
  guard let ptr = _swift_stdlib_CFStringGetCStringPtr(
    str, kCFStringEncodingUTF8
  ) else { return nil }

  return ptr._asUInt8
}

private enum CocoaStringPointer {
  case ascii(UnsafePointer<UInt8>)
  case utf8(UnsafePointer<UInt8>)
  case utf16(UnsafePointer<UInt16>)
  case none
}

@_effects(readonly)
private func _getCocoaStringPointer(
  _ cfImmutableValue: _CocoaString
) -> CocoaStringPointer {
  if let utf8Ptr = _cocoaUTF8Pointer(cfImmutableValue) {
    // NOTE: CFStringGetCStringPointer means ASCII
    return .ascii(utf8Ptr)
  }
  if let utf16Ptr = _swift_stdlib_CFStringGetCharactersPtr(cfImmutableValue) {
    return .utf16(utf16Ptr)
  }
  return .none
}

@_effects(releasenone)
public //SPI(Foundation)
func _bridgeCocoaStringLazily(
  _ cocoaString: AnyObject,
  _ cls: AnyClass,
  _ length: Int
) -> String {
  
  if cls == __StringStorage.self {
    return String(_unsafeUncheckedDowncast(
      cocoaString, to: __StringStorage.self).asString._guts)
  }
  if cls == __SharedStringStorage.self {
    return String(_unsafeUncheckedDowncast(
      cocoaString, to: __SharedStringStorage.self).asString._guts)
  }
  
  let (fastUTF8, isASCII): (Bool, Bool)
  switch _getCocoaStringPointer(cocoaString) {
  case .ascii(_): (fastUTF8, isASCII) = (true, true)
  case .utf8(_): (fastUTF8, isASCII) = (true, false)
  default:  (fastUTF8, isASCII) = (false, false)
  }
  
  return String(_StringGuts(
    cocoa: cocoaString,
    providesFastUTF8: fastUTF8,
    isASCII: isASCII,
    length: length))
}

@usableFromInline
@_effects(releasenone) // @opaque
internal func _bridgeCocoaString(
  _ cocoaString: _CocoaString
) -> _StringGuts {
  return (cocoaString as Any as! String)._guts
}

extension String {
  public // SPI(Foundation)
  init(_cocoaString: AnyObject) {
    self._guts = _bridgeCocoaString(_cocoaString)
  }
}

private let BAD:UInt8 = 0xFF
private let charToSixBitLookup:[UInt8] = [
  /* 0 - 31 */    BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
  //                                                                                              '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  /* 32 - 63  */  15,  BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, 46,  7,   60,  29,  30,  40,  31,  28,  43,  44,  48,  42,  49,  BAD, BAD, BAD, BAD, BAD, BAD,
  //                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  /* 64 - 95 */   BAD, 50,  47,  27,  33,  53,  51,  58,  61,  13,  59,  52,  39,  19,  34,  41,  45,  BAD, 24,  20,  23,  38,  55,  54,  63,  62,  BAD, BAD, BAD, BAD, BAD, 56,
  /* 96 - 127 */  BAD, 8,   32,  14,  10,  0,   17,  26,  21,  1,   22,  18,  2,   6,   11,  3,   9,   BAD, 5,   12,  4,   16,  35,  36,  25,  37,  57,  BAD, BAD, BAD, BAD, BAD,
  /*   */         BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
  /*   */         BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
  /*   */         BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
  /*   */         BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD
];

extension String {
  @_effects(releasenone)
  public // SPI(Foundation)
  func _bridgeToObjectiveCImpl() -> AnyObject {
    if _guts.isSmall {
      return _guts.asSmall.withUTF8 { bufPtr in
        if _guts.isASCII {
          return charToSixBitLookup.withUnsafeBufferPointer { lookupTable in
            
            //NOTE: This makes assumptions about how tagged NSStrings work
            //If those assumptions become invalid, this will be less efficient
            //but not incorrect. Make sure not to rely on the valid lengths or
            //allowed characters in a tagged NSString for correctness.
            
            let canTag:Bool
            switch bufPtr.count {
            case 0...7: //TAGGED_STRING_UNPACKED_MAXLEN
              canTag = true
            case 8...9: //TAGGED_STRING_SIXBIT_MAXLEN
              canTag = bufPtr.allSatisfy { lookupTable[_unchecked: Int($0)] <= 63 }
            case 9...11: //TAGGED_STRING_FIVEBIT_MAXLEN
              canTag = bufPtr.allSatisfy { lookupTable[_unchecked: Int($0)] <= 31 }
            default:
 	      canTag = false
	    }
            
            if canTag {
              return _swift_stdlib_CFStringCreateWithBytes(
                nil, bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
                bufPtr.count,
                kCFStringEncodingASCII, 0)
                as AnyObject
            }
            
            return __StringStorage.create(initializingFrom: bufPtr,
                                          isASCII: true)
          }
        } else {
          return __StringStorage.create(initializingFrom: bufPtr,
                                        isASCII: false)
        }
      }
    }
    if _guts._object.isImmortal {
      // TODO: We'd rather emit a valid ObjC object statically than create a
      // shared string class instance.
      let gutsCountAndFlags = _guts._object._countAndFlags
      return __SharedStringStorage(
        immortal: _guts._object.fastUTF8.baseAddress!,
        countAndFlags: _StringObject.CountAndFlags(
          sharedCount: _guts.count, isASCII: gutsCountAndFlags.isASCII))
    }

    _internalInvariant(_guts._object.hasObjCBridgeableObject,
      "Unknown non-bridgeable object case")
    return _guts._object.objCBridgeableObject
  }
}

// At runtime, this class is derived from `__SwiftNativeNSStringBase`,
// which is derived from `NSString`.
//
// The @_swift_native_objc_runtime_base attribute
// This allows us to subclass an Objective-C class and use the fast Swift
// memory allocator.
@objc @_swift_native_objc_runtime_base(__SwiftNativeNSStringBase)
class __SwiftNativeNSString {
  @objc internal init() {}
  deinit {}
}

// Called by the SwiftObject implementation to get the description of a value
// as an NSString.
@_silgen_name("swift_stdlib_getDescription")
public func _getDescription<T>(_ x: T) -> AnyObject {
  return String(reflecting: x)._bridgeToObjectiveCImpl()
}

#else // !_runtime(_ObjC)

internal class __SwiftNativeNSString {
  internal init() {}
  deinit {}
}

#endif

// Special-case Index <-> Offset converters for bridging and use in accelerating
// the UTF16View in general.
extension StringProtocol {
  @_specialize(where Self == String)
  @_specialize(where Self == Substring)
  public // SPI(Foundation)
  func _toUTF16Offset(_ idx: Index) -> Int {
    return self.utf16.distance(from: self.utf16.startIndex, to: idx)
  }

  @_specialize(where Self == String)
  @_specialize(where Self == Substring)
  public // SPI(Foundation)
  func _toUTF16Index(_ offset: Int) -> Index {
    return self.utf16.index(self.utf16.startIndex, offsetBy: offset)
  }

  @_specialize(where Self == String)
  @_specialize(where Self == Substring)
  public // SPI(Foundation)
  func _toUTF16Offsets(_ indices: Range<Index>) -> Range<Int> {
    let lowerbound = _toUTF16Offset(indices.lowerBound)
    let length = self.utf16.distance(
      from: indices.lowerBound, to: indices.upperBound)
    return Range(
      uncheckedBounds: (lower: lowerbound, upper: lowerbound + length))
  }

  @_specialize(where Self == String)
  @_specialize(where Self == Substring)
  public // SPI(Foundation)
  func _toUTF16Indices(_ range: Range<Int>) -> Range<Index> {
    let lowerbound = _toUTF16Index(range.lowerBound)
    let upperbound = _toUTF16Index(range.lowerBound + range.count)
    return Range(uncheckedBounds: (lower: lowerbound, upper: upperbound))
  }
}

extension String {
  public // @testable / @benchmarkable
  func _copyUTF16CodeUnits(
    into buffer: UnsafeMutableBufferPointer<UInt16>,
    range: Range<Int>
  ) {
    _internalInvariant(buffer.count >= range.count)
    let indexRange = self._toUTF16Indices(range)
    self._nativeCopyUTF16CodeUnits(into: buffer, range: indexRange)
  }
}
