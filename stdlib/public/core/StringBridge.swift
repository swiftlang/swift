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

import SwiftShims

#if _runtime(_ObjC)
// Swift's String bridges NSString via this protocol and these
// variables, allowing the core stdlib to remain decoupled from
// Foundation.

/// Effectively an untyped NSString that doesn't require foundation.
public typealias _CocoaString = AnyObject

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

/// Copies a slice of a _CocoaString into contiguous storage of
/// sufficient capacity.
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
  _ bytes: UnsafePointer<UInt8>,
  length: Int
  ) -> UInt {
  return _swift_stdlib_CFStringHashCString(bytes, length)
}

//
// Conversion from NSString to Swift's native representation
//

private var kCFStringEncodingASCII : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x0600 }
}
private var kCFStringEncodingUTF8 : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x8000100 }
}

#if !(arch(i386) || arch(arm))
// Resiliently write a tagged cocoa string's contents into a buffer
@_effects(releasenone) // @opaque
internal func _bridgeTagged(
  _ cocoa: _CocoaString,
  intoUTF8 bufPtr: UnsafeMutableBufferPointer<UInt8>
) -> Int? {
  _sanityCheck(_isObjCTaggedPointer(cocoa))
  let ptr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
  let length = _stdlib_binary_CFStringGetLength(cocoa)
  _sanityCheck(length <= _SmallString.capacity)
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

@usableFromInline
@_effects(releasenone) // @opaque
internal func _bridgeCocoaString(_ cocoaString: _CocoaString) -> _StringGuts {
  if let abstract = cocoaString as? _AbstractStringStorage {
    return abstract.asString._guts
  }
#if !(arch(i386) || arch(arm))
  if _isObjCTaggedPointer(cocoaString) {
    return _StringGuts(_SmallString(taggedCocoa: cocoaString))
  }
#endif

  // "copy" it into a value to be sure nobody will modify behind
  // our backs.  In practice, when value is already immutable, this
  // just does a retain.
  //
  // TODO: Only in certain circumstances should we emit this call:
  //   1) If it's immutable, just retain it.
  //   2) If it's mutable with no associated information, then a copy must
  //      happen; might as well eagerly bridge it in.
  //   3) If it's mutable with associated information, must make the call
  //
  let immutableCopy
    = _stdlib_binary_CFStringCreateCopy(cocoaString) as AnyObject

#if !(arch(i386) || arch(arm))
  if _isObjCTaggedPointer(immutableCopy) {
    return _StringGuts(_SmallString(taggedCocoa: immutableCopy))
  }
#endif

  let (fastUTF8, isASCII): (Bool, Bool)
  switch _getCocoaStringPointer(immutableCopy) {
    case .ascii(_): (fastUTF8, isASCII) = (true, true)
    case .utf8(_): (fastUTF8, isASCII) = (true, false)
    default:  (fastUTF8, isASCII) = (false, false)
  }
  let length = _stdlib_binary_CFStringGetLength(immutableCopy)

  return _StringGuts(
    cocoa: immutableCopy,
    providesFastUTF8: fastUTF8,
    isASCII: isASCII,
    length: length)
}

extension String {
  public // SPI(Foundation)
  init(_cocoaString: AnyObject) {
    self._guts = _bridgeCocoaString(_cocoaString)
  }
}

extension String {
  @_effects(releasenone)
  public // SPI(Foundation)
  func _bridgeToObjectiveCImpl() -> AnyObject {
    if _guts.isSmall {
      return _guts.asSmall.withUTF8 { bufPtr in
        // TODO(String bridging): worth isASCII check for different encoding?
        return _swift_stdlib_CFStringCreateWithBytes(
            nil, bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
            bufPtr.count,
            kCFStringEncodingUTF8, 0)
        as AnyObject
      }
    }
    if _guts._object.isImmortal {
      return _SharedStringStorage(
        immortal: _guts._object.fastUTF8.baseAddress!,
        countAndFlags: _guts._object._countAndFlags)
    }

    _sanityCheck(_guts._object.hasObjCBridgeableObject,
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
@_fixed_layout // FIXME(sil-serialize-all)
@objc @_swift_native_objc_runtime_base(__SwiftNativeNSStringBase)
public class __SwiftNativeNSString {
  @usableFromInline // FIXME(sil-serialize-all)
  @objc
  internal init() {}
  deinit {}
}

/// A shadow for the "core operations" of NSString.
///
/// Covers a set of operations everyone needs to implement in order to
/// be a useful `NSString` subclass.
@objc
public protocol _NSStringCore : _NSCopying /* _NSFastEnumeration */ {

  // The following methods should be overridden when implementing an
  // NSString subclass.

  @objc(length)
  var length: Int { get }

  @objc(characterAtIndex:)
  func character(at index: Int) -> UInt16
}

// Called by the SwiftObject implementation to get the description of a value
// as an NSString.
@_silgen_name("swift_stdlib_getDescription")
public func _getDescription<T>(_ x: T) -> AnyObject {
  return String(reflecting: x)._bridgeToObjectiveCImpl()
}

#else // !_runtime(_ObjC)

@_fixed_layout // FIXME(sil-serialize-all)
public class __SwiftNativeNSString {
  @usableFromInline // FIXME(sil-serialize-all)
  internal init() {}
  deinit {}
}

public protocol _NSStringCore: class {}

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
    _sanityCheck(buffer.count >= range.count)
    let indexRange = self._toUTF16Indices(range)
    self._nativeCopyUTF16CodeUnits(into: buffer, range: indexRange)
  }
}
