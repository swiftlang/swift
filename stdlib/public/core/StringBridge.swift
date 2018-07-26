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


@_effects(releasenone)
internal func _cocoaStringSlice(
  _ target: _CocoaString, _ bounds: Range<Int>
) -> _CocoaString {
  let cfSelf: _swift_shims_CFStringRef = target

  _sanityCheck(
    _swift_stdlib_CFStringGetCharactersPtr(cfSelf) == nil,
    "Known contiguously stored strings should already be converted to Swift")

  let cfResult = _swift_stdlib_CFStringCreateWithSubstring(
    nil, cfSelf, _swift_shims_CFRange(
      location: bounds.lowerBound, length: bounds.count)) as AnyObject

  return cfResult
}


@_effects(readonly)
internal func _cocoaStringSubscript(
  _ target: _CocoaString, _ position: Int
) -> UTF16.CodeUnit {
  let cfSelf: _swift_shims_CFStringRef = target
  return _swift_stdlib_CFStringGetCharacterAtIndex(cfSelf, position)
}

//
// Conversion from NSString to Swift's native representation
//

internal var kCFStringEncodingASCII : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x0600 }
}
internal var kCFStringEncodingUTF8 : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x8000100 }
}

// Resiliently write a tagged cocoa string's contents into a buffer
@_effects(readonly) // @opaque
internal func _bridgeTagged(
  _ cocoa: _CocoaString,
  intoUTF8 bufPtr: UnsafeMutableBufferPointer<UInt8>
) -> Int? {
  _sanityCheck(_isObjCTaggedPointer(cocoa))
  let ptr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
  let length = _stdlib_binary_CFStringGetLength(cocoa)
  _sanityCheck(length <= _SmallUTF8String.capacity)
  var count = 0
  let numCharWritten = _swift_stdlib_CFStringGetBytes(
    cocoa, _swift_shims_CFRange(location: 0, length: length),
    kCFStringEncodingUTF8, 0, 0, ptr, bufPtr.count, &count)
  return length == numCharWritten ? count : nil
}

@_effects(releasenone)
internal func _bridgeToCocoa(_ small: _SmallUTF8String) -> _CocoaString {
  unimplemented_utf8()
}

internal func _cocoaUTF8Pointer(_ str: _CocoaString) -> UnsafePointer<UInt8>? {
  // TODO(UTF8): Is there a better interface here? This requires nul
  // termination and may assume ASCII.
  guard let ptr = _swift_stdlib_CFStringGetCStringPtr(
    str, kCFStringEncodingUTF8
  ) else { return nil }

  return ptr._asUInt8
}

@_effects(readonly)
internal func _getCocoaStringPointer(
  _ cfImmutableValue: _CocoaString
) -> (UnsafeRawPointer?, isUTF16: Bool)  {
  let nulTerminatedASCII = _cocoaUTF8Pointer(cfImmutableValue)

  // start will hold the base pointer of contiguous storage, if it
  // is found.
  var start: UnsafeRawPointer?
  let isUTF16 = (nulTerminatedASCII == nil)
  if isUTF16 {
    let utf16Buf = _swift_stdlib_CFStringGetCharactersPtr(cfImmutableValue)
    start = UnsafeRawPointer(utf16Buf)
  } else {
    start = UnsafeRawPointer(nulTerminatedASCII)
  }
  return (start, isUTF16: isUTF16)
}

@usableFromInline
@_effects(releasenone) // @opaque
internal func _bridgeCocoaString(_ cocoaString: _CocoaString) -> _StringGuts {
  if let abstract = cocoaString as? _AbstractStringStorage {
    return abstract.asString._guts
  } else if _isObjCTaggedPointer(cocoaString) {
    return _StringGuts(_SmallUTF8String(taggedCocoa: cocoaString))
  }

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

  if _isObjCTaggedPointer(immutableCopy) {
    return _StringGuts(_SmallUTF8String(taggedCocoa: immutableCopy))
  }

  let (start, isUTF16) = _getCocoaStringPointer(immutableCopy)
  let length = _stdlib_binary_CFStringGetLength(immutableCopy)

  // Detect fast-UTF8 Cocoa
  let fastUTF8 = !isUTF16 && start != nil
  return _StringGuts(
    cocoa: immutableCopy, providesFastUTF8: fastUTF8, length: length)
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
    // TODO(UTF8): create and use a visit pattern on _StringGuts to handle each
    // form, rather than querying object directly. Presumably there will be
    // other such visitors.
    if _guts._object.isSmall {
      return _guts._object.asSmallString.withUTF8 { bufPtr in
        // TODO(UTF8 perf): worth isKnownASCII check for different encoding?
        return _swift_stdlib_CFStringCreateWithBytes(
            nil, bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
            bufPtr.count,
            kCFStringEncodingUTF8, 0)
        as AnyObject
      }
    }
    if _guts._object.isImmortal {
      return _SharedStringStorage(immortal: _guts._object.fastUTF8)
    }

    _sanityCheck(_guts._object.hasObjCBridgeableObject,
      "Unknown non-bridgeable object case")
    return _guts._object.objCBridgeableObject
  }
}

// At runtime, this class is derived from `_SwiftNativeNSStringBase`,
// which is derived from `NSString`.
//
// The @_swift_native_objc_runtime_base attribute
// This allows us to subclass an Objective-C class and use the fast Swift
// memory allocator.
@_fixed_layout // FIXME(sil-serialize-all)
@objc @_swift_native_objc_runtime_base(_SwiftNativeNSStringBase)
public class _SwiftNativeNSString {
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

 // We also override the following methods for efficiency.

  @objc(getCharacters:range:)
  func getCharacters(
   _ buffer: UnsafeMutablePointer<UInt16>,
   range aRange: _SwiftNSRange)

  @objc(_fastCharacterContents)
  func _fastCharacterContents() -> UnsafePointer<UInt16>?

  @objc(_fastCStringContents)
  func _fastCStringContents() -> UnsafePointer<CChar>?
}

// Called by the SwiftObject implementation to get the description of a value
// as an NSString.
@_silgen_name("swift_stdlib_getDescription")
public func _getDescription<T>(_ x: T) -> AnyObject {
  return String(reflecting: x)._bridgeToObjectiveCImpl()
}

#else // !_runtime(_ObjC)

@_fixed_layout // FIXME(sil-serialize-all)
public class _SwiftNativeNSString {
  @usableFromInline // FIXME(sil-serialize-all)
  internal init() {}
  deinit {}
}

public protocol _NSStringCore: class {}

#endif

extension String {
  // Resiliently provide a (barely) amortized random access UTF-16 interface
  //
  // @opaque
  internal func _utf16OffsetToIndex(_ offset: Int) -> Index {
    // TODO(UTF8): Track known ASCII

    // TODO(UTF8): Leave breadcrumbs, and more efficient impl

    return self.utf16.index(self.utf16.startIndex, offsetBy: offset)
  }

  // Resiliently provide a (barely) amortized random access UTF-16 interface
  //
  // @opaque
  internal func _utf16OffsetToIndex(_ range: Range<Int>) -> Range<Index> {
    // TODO(UTF8): Can be more efficient for a range
    return self._utf16OffsetToIndex(range.lowerBound)
       ..< self._utf16OffsetToIndex(range.upperBound)
  }

  // Resiliently provide a (barely) amortized random access UTF-16 interface
  //
  // @opaque
  internal func _utf16Length() -> Int {
    // TODO(UTF8): Track known ASCII

    // TODO(UTF8): Leave breadcrumbs, and more efficient impl. Perhaps even
    // store it.

    return self.utf16.count
  }

  // Resiliently provide a (barely) amortized `characterAtIndex`
  //
  // @opaque
  internal func _utf16CodeUnitAtOffset(_ offset: Int) -> UInt16 {
    return self.utf16[self._utf16OffsetToIndex(offset)]
  }

}

