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

@objc private protocol _StringSelectorHolder : _NSCopying {

  @objc var length: Int { get }

  @objc var hash: UInt { get }

  @objc(characterAtIndex:)
  func character(at offset: Int) -> UInt16

  @objc(getCharacters:range:)
  func getCharacters(
   _ buffer: UnsafeMutablePointer<UInt16>, range aRange: _SwiftNSRange
  )

  @objc(_fastCStringContents:)
  func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>?

  @objc(_fastCharacterContents)
  func _fastCharacterContents() -> UnsafePointer<UInt16>?

  @objc(getBytes:maxLength:usedLength:encoding:options:range:remainingRange:)
  func getBytes(_ buffer: UnsafeMutableRawPointer?,
   maxLength maxBufferCount: Int,
  usedLength usedBufferCount: UnsafeMutablePointer<Int>?,
    encoding: UInt,
     options: UInt,
       range: _SwiftNSRange,
       remaining leftover: UnsafeMutablePointer<_SwiftNSRange>?) -> Int8

  @objc(compare:options:range:locale:)
  func compare(_ string: _CocoaString,
               options: UInt,
               range: _SwiftNSRange,
               locale: AnyObject?) -> Int

  @objc(newTaggedNSStringWithASCIIBytes_:length_:)
  func createTaggedString(bytes: UnsafePointer<UInt8>,
                          count: Int) -> AnyObject?
}

/*
 Passing a _CocoaString through _objc() lets you call ObjC methods that the
 compiler doesn't know about, via the protocol above. In order to get good
 performance, you need a double indirection like this:

  func a -> _objc -> func a'

 because any refcounting @_effects on 'a' will be lost when _objc breaks ARC's
 knowledge that the _CocoaString and _StringSelectorHolder are the same object.
 */
@inline(__always)
private func _objc(_ str: _CocoaString) -> _StringSelectorHolder {
  return unsafeBitCast(str, to: _StringSelectorHolder.self)
}

@usableFromInline // @testable
internal func _stdlib_binary_CFStringCreateCopy(
  _ source: _CocoaString
) -> _CocoaString {
  return _objc(source).copy(with: nil)
}

@usableFromInline // @testable
 internal func _stdlib_binary_CFStringGetLength(
  _ source: _CocoaString
) -> Int {
  return _objc(source).length
}

@usableFromInline // @testable
 internal func _stdlib_binary_CFStringGetCharactersPtr(
  _ source: _CocoaString
) -> UnsafeMutablePointer<UTF16.CodeUnit>? {
  return UnsafeMutablePointer(mutating: _objc(source)._fastCharacterContents())
}

/// Copies a slice of a _CocoaString into contiguous storage of sufficient
/// capacity.
internal func _cocoaStringCopyCharacters(
  from source: _CocoaString,
  range: Range<Int>,
  into destination: UnsafeMutablePointer<UTF16.CodeUnit>
) {
  _objc(source).getCharacters(destination, range: _SwiftNSRange(
    location: range.startIndex,
    length: range.count)
  )
}

 internal func _cocoaStringSubscript(
  _ target: _CocoaString, _ position: Int
) -> UTF16.CodeUnit {
  return _objc(target).character(at: position)
}

internal func _cocoaStringCopyUTF8(
  _ target: _CocoaString,
  into bufPtr: UnsafeMutableBufferPointer<UInt8>
) -> Int? {
  let o = _objc(target)
  let ptr = bufPtr.baseAddress!
  let len = o.length
  var remainingRange = _SwiftNSRange(location: 0, length: 0)
  var usedLen = 0
  let success = 0 != o.getBytes(
    ptr,
    maxLength: bufPtr.count,
    usedLength: &usedLen,
    encoding: _cocoaUTF8Encoding,
    options: 0,
    range: _SwiftNSRange(location: 0, length: len),
    remaining: &remainingRange
  )
  if success && remainingRange.length == 0 {
    return usedLen
  }
  return nil
}

 internal func _cocoaStringUTF8Count(
  _ target: _CocoaString,
  range: Range<Int>
) -> Int? {
  if range.isEmpty { return 0 }
  let o = _objc(target)
  var remainingRange = _SwiftNSRange(location: 0, length: 0)
  var usedLen = 0
  let success = 0 != o.getBytes(
    UnsafeMutablePointer<UInt8>(Builtin.inttoptr_Word(0._builtinWordValue)),
    maxLength: 0,
    usedLength: &usedLen,
    encoding: _cocoaUTF8Encoding,
    options: 0,
    range: _SwiftNSRange(location: range.startIndex, length: range.count),
    remaining: &remainingRange
  )
  if success && remainingRange.length == 0 {
    return usedLen
  }
  return nil
}

 internal func _cocoaStringCompare(
  _ string: _CocoaString, _ other: _CocoaString
) -> Int {
  let o = _objc(string)
  let range = _SwiftNSRange(location: 0, length: o.length)
  let options = UInt(2) /* NSLiteralSearch*/
  return o.compare(other, options: options, range: range, locale: nil)
}

 internal func _cocoaHashString(
  _ string: _CocoaString
) -> UInt {
  return _swift_stdlib_CFStringHashNSString(string)
}

 internal func _cocoaHashASCIIBytes(
  _ bytes: UnsafePointer<UInt8>, length: Int
) -> UInt {
  return _swift_stdlib_CFStringHashCString(bytes, length)
}

// These "trampolines" are effectively objc_msgSend_super.
// They bypass our implementations to use NSString's.

 internal func _cocoaCStringUsingEncodingTrampoline(
  _ string: _CocoaString, _ encoding: UInt
) -> UnsafePointer<UInt8>? {
  return _swift_stdlib_NSStringCStringUsingEncodingTrampoline(string, encoding)
}

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

private var kCFStringEncodingASCII: _swift_shims_CFStringEncoding {
  get { return 0x0600 }
}

private var kCFStringEncodingUTF8: _swift_shims_CFStringEncoding {
 get { return 0x8000100 }
}

internal enum _KnownCocoaString {
  case storage
  case shared
  case cocoa
#if !(arch(i386) || arch(arm))
  case tagged
#endif

  init(_ str: _CocoaString) {

#if !(arch(i386) || arch(arm))
    if _isObjCTaggedPointer(str) {
      self = .tagged
      return
    }
#endif
    
    if str is __StringStorage {
      self = .storage
    } else if str is __SharedStringStorage {
      self = .shared
    } else {
      self = .cocoa
    }
  }
}

#if !(arch(i386) || arch(arm))

// Resiliently write a tagged _CocoaString's contents into a buffer.
// TODO: move this to the Foundation overlay and reimplement it with
// _NSTaggedPointerStringGetBytes
@_effects(releasenone) // @opaque
internal func _bridgeTagged(
  _ cocoa: _CocoaString,
  intoUTF8 bufPtr: UnsafeMutableBufferPointer<UInt8>
) -> Int? {
  _internalInvariant(_isObjCTaggedPointer(cocoa))
  return _cocoaStringCopyUTF8(cocoa, into: bufPtr)
}
#endif

 private func _NSStringASCIIPointer(_ str: _StringSelectorHolder) -> UnsafePointer<UInt8>? {
 // TODO(String bridging): Is there a better interface here? Ideally we'd be
  // able to ask for UTF8 rather than just ASCII
  return str._fastCStringContents(0)?._asUInt8
}

internal func _cocoaASCIIPointer(_ str: _CocoaString) -> UnsafePointer<UInt8>? {
  return _NSStringASCIIPointer(_objc(str))
}

private enum CocoaStringPointer {
  case ascii(UnsafePointer<UInt8>)
  case utf8(UnsafePointer<UInt8>)
  case utf16(UnsafePointer<UInt16>)
  case none
}

 private func _getCocoaStringPointer(
  _ cfImmutableValue: _CocoaString
) -> CocoaStringPointer {
  if let asciiPtr = _cocoaASCIIPointer(cfImmutableValue) {
    // NOTE: CFStringGetCStringPointer means ASCII
    return .ascii(asciiPtr)
  }
  if let utf16Ptr = _stdlib_binary_CFStringGetCharactersPtr(cfImmutableValue) {
    return .utf16(utf16Ptr)
  }
  return .none
}

@usableFromInline
internal func _bridgeCocoaString(_ cocoaString: _CocoaString) -> _StringGuts {
  switch _KnownCocoaString(cocoaString) {
  case .storage:
    fallthrough
  case .shared:
    return (cocoaString as! __StringStorage).asString._guts
#if !(arch(i386) || arch(arm))
  case .tagged:
    return _StringGuts(_SmallString(taggedCocoa: cocoaString))
#endif
  case .cocoa:
    // "Copy" it into a value to be sure nobody will modify behind
    // our backs. In practice, when value is already immutable, this
    // just does a retain.
    //
    // TODO: Only in certain circumstances should we emit this call:
    //   1) If it's immutable, just retain it.
    //   2) If it's mutable with no associated information, then a copy must
    //      happen; might as well eagerly bridge it in.
    //   3) If it's mutable with associated information, must make the call
    let immutableCopy
      = _stdlib_binary_CFStringCreateCopy(cocoaString)

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
}

extension String {
  public // SPI(Foundation)
  init(_cocoaString: AnyObject) {
    self._guts = _bridgeCocoaString(_cocoaString)
  }
}

private func _createNSString(
  _ receiver: _StringSelectorHolder,
  _ ptr: UnsafePointer<UInt8>,
  _ count: Int,
  _ encoding: UInt32
) -> AnyObject? {
  return receiver.createTaggedString(bytes: ptr, count: count)
}

private func _createCFString(
  _ ptr: UnsafePointer<UInt8>,
  _ count: Int,
  _ encoding: UInt32
) -> AnyObject? {
  return _createNSString(
    unsafeBitCast(__StringStorage.self as AnyClass, to: _StringSelectorHolder.self),
    ptr,
    count,
    encoding
  )
}

extension String {
    public // SPI(Foundation)
  func _bridgeToObjectiveCImpl() -> AnyObject {

    _connectOrphanedFoundationSubclassesIfNeeded()

    // Smol ASCII a) may bridge to tagged pointers, b) can't contain a BOM
    if _guts.isSmallASCII {
      let maybeTagged = _guts.asSmall.withUTF8 { bufPtr in
        return _createCFString(
          bufPtr.baseAddress!,
          bufPtr.count,
          kCFStringEncodingUTF8
        )
      }
      if let tagged = maybeTagged { return tagged }
    }

    if _guts.isSmall {
        // We can't form a tagged pointer String, so grow to a non-small String,
        // and bridge that instead. Also avoids CF deleting any BOM that may be
        // present
        var copy = self
        copy._guts.grow(_SmallString.capacity + 1)
        _internalInvariant(!copy._guts.isSmall)
        return copy._bridgeToObjectiveCImpl()
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

@available(macOS, introduced: 9999, deprecated)
@available(iOS, introduced: 9999, deprecated)
@available(watchOS, introduced: 9999, deprecated)
@available(tvOS, introduced: 9999, deprecated)
@available(*, deprecated)
@_cdecl("_SwiftCreateBridgedString")
@usableFromInline
internal func _SwiftCreateBridgedString(
  bytes: UnsafePointer<UInt8>,
  length: Int,
  encoding: _swift_shims_CFStringEncoding
) -> Unmanaged<AnyObject> {
  let bufPtr = UnsafeBufferPointer(start: bytes, count: length)
  let str:String
  switch encoding {
  case kCFStringEncodingUTF8:
    str = String(decoding: bufPtr, as: Unicode.UTF8.self)
  case kCFStringEncodingASCII:
    str = String(decoding: bufPtr, as: Unicode.ASCII.self)
  default:
    fatalError("Unsupported encoding in shim")
  }
  return Unmanaged<AnyObject>.passRetained(str._bridgeToObjectiveCImpl())
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

@_silgen_name("swift_stdlib_NSStringFromUTF8")
@usableFromInline //this makes the symbol available to the runtime :(
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
internal func _NSStringFromUTF8(_ s: UnsafePointer<UInt8>, _ len: Int)
  -> AnyObject {
  return String(
    decoding: UnsafeBufferPointer(start: s, count: len),
    as: UTF8.self
  )._bridgeToObjectiveCImpl()
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
    self.utf16._nativeCopy(into: buffer, alignedRange: indexRange)
  }
}
