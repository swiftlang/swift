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
  
  @objc(UTF8String)
  func _utf8String() -> UnsafePointer<UInt8>
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

@_effects(releasenone)
private func _copyNSString(_ str: _StringSelectorHolder) -> _CocoaString {
  return str.copy(with: nil)
}

@usableFromInline // @testable
@_effects(releasenone)
internal func _stdlib_binary_CFStringCreateCopy(
  _ source: _CocoaString
) -> _CocoaString {
  return _copyNSString(_objc(source))
}

@_effects(readonly)
private func _NSStringLen(_ str: _StringSelectorHolder) -> Int {
  return str.length
}

@usableFromInline // @testable
@_effects(readonly)
internal func _stdlib_binary_CFStringGetLength(
  _ source: _CocoaString
) -> Int {
  return _NSStringLen(_objc(source))
}

@_effects(readonly)
private func _NSStringCharactersPtr(_ str: _StringSelectorHolder) -> UnsafeMutablePointer<UTF16.CodeUnit>? {
  return UnsafeMutablePointer(mutating: str._fastCharacterContents())
}

@usableFromInline // @testable
@_effects(readonly)
internal func _stdlib_binary_CFStringGetCharactersPtr(
  _ source: _CocoaString
) -> UnsafeMutablePointer<UTF16.CodeUnit>? {
  return _NSStringCharactersPtr(_objc(source))
}

@_effects(releasenone)
private func _NSStringGetCharacters(
  from source: _StringSelectorHolder,
  range: Range<Int>,
  into destination: UnsafeMutablePointer<UTF16.CodeUnit>
) {
  source.getCharacters(destination, range: _SwiftNSRange(
    location: range.startIndex,
    length: range.count)
  )
}

/// Copies a slice of a _CocoaString into contiguous storage of sufficient
/// capacity.
@_effects(releasenone)
internal func _cocoaStringCopyCharacters(
  from source: _CocoaString,
  range: Range<Int>,
  into destination: UnsafeMutablePointer<UTF16.CodeUnit>
) {
  _NSStringGetCharacters(from: _objc(source), range: range, into: destination)
}

@_effects(readonly)
private func _NSStringGetCharacter(
  _ target: _StringSelectorHolder, _ position: Int
) -> UTF16.CodeUnit {
  return target.character(at: position)
}

@_effects(readonly)
internal func _cocoaStringSubscript(
  _ target: _CocoaString, _ position: Int
) -> UTF16.CodeUnit {
  return _NSStringGetCharacter(_objc(target), position)
}

@_effects(releasenone)
private func _NSStringCopyUTF8(
  _ o: _StringSelectorHolder,
  into bufPtr: UnsafeMutableBufferPointer<UInt8>
) -> Int? {
  let ptr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
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

@_effects(releasenone)
internal func _cocoaStringCopyUTF8(
  _ target: _CocoaString,
  into bufPtr: UnsafeMutableBufferPointer<UInt8>
) -> Int? {
  return _NSStringCopyUTF8(_objc(target), into: bufPtr)
}

@_effects(releasenone)
private func _NSStringGetUTF8Pointer(
  _ o: _StringSelectorHolder
) -> UnsafePointer<UInt8> {
  return o._utf8String()
}

@_effects(releasenone)
internal func _cocoaStringGetUTF8Pointer(
  _ target: _CocoaString
) -> UnsafePointer<UInt8> {
  return _NSStringGetUTF8Pointer(_objc(target))
}

@_effects(readonly)
private func _NSStringUTF8Count(
  _ o: _StringSelectorHolder,
  range: Range<Int>
) -> Int? {
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

@_effects(readonly)
internal func _cocoaStringUTF8Count(
  _ target: _CocoaString,
  range: Range<Int>
) -> Int? {
  if range.isEmpty { return 0 }
  return _NSStringUTF8Count(_objc(target), range: range)
}

@_effects(readonly)
private func _NSStringCompare(
  _ o: _StringSelectorHolder, _ other: _CocoaString
) -> Int {
  let range = _SwiftNSRange(location: 0, length: o.length)
  let options = UInt(2) /* NSLiteralSearch*/
  return o.compare(other, options: options, range: range, locale: nil)
}

@_effects(readonly)
internal func _cocoaStringCompare(
  _ string: _CocoaString, _ other: _CocoaString
) -> Int {
  return _NSStringCompare(_objc(string), other)
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

private var kCFStringEncodingASCII: _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x0600 }
}

private var kCFStringEncodingUTF8: _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x8000100 }
}

internal enum _KnownCocoaString {
  case storage(__StringStorage)
  case shared(__SharedStringStorage)
  case mutable(_SwiftNSMutableString)
  case cocoa(_CocoaString)
#if !(arch(i386) || arch(arm))
  case tagged(_CocoaString)
#endif

  @inline(__always)
  init(_ str: _CocoaString) {

#if !(arch(i386) || arch(arm))
    if _isObjCTaggedPointer(str) {
      self = .tagged(str)
      return
    }
#endif
    
    switch ObjectIdentifier(_swift_classOfObjCHeapObject(str)) {
    case ObjectIdentifier(__StringStorage.self):
      self = .storage(_unsafeUncheckedDowncast(cocoaString,
                                               to: __StringStorage.self))
    case ObjectIdentifier(__SharedStringStorage.self):
      self = .shared(_unsafeUncheckedDowncast(cocoaString,
                                              to: __SharedStringStorage.self))
    case ObjectIdentifier(_SwiftNSMutableString.self):
      self = .mutable(_unsafeUncheckedDowncast(cocoaString,
                                              to: _SwiftNSMutableString.self))
    default:
      self = .cocoa(str)
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

@_effects(readonly)
private func _NSStringASCIIPointer(_ str: _StringSelectorHolder) -> UnsafePointer<UInt8>? {
 // TODO(String bridging): Is there a better interface here? Ideally we'd be
  // able to ask for UTF8 rather than just ASCII
  return str._fastCStringContents(0)?._asUInt8
}

@_effects(readonly) // @opaque
internal func _cocoaASCIIPointer(_ str: _CocoaString) -> UnsafePointer<UInt8>? {
  return _NSStringASCIIPointer(_objc(str))
}

// This does not create a bridged String that's safe to use past the current
// stack frame, e.g. it does not defensively copy it and so on.
@_effects(releasenone)
internal func _withTemporaryBridgedCocoaString<R>(
  _ cocoaString: _CocoaString?,
  _ work: (String) throws -> R
) rethrows -> R {
  guard let cocoaString = cocoaString else {
    return try work("")
  }
  switch _KnownCocoaString(cocoaString) {
  case .storage(let storage):
    return try work(storage.asString)
  case .shared(let storage):
    return try work(storage.asString)
    #if !(arch(i386) || arch(arm))
  case .tagged(let str):
    let tmpStr = String(_StringGuts(_SmallString(taggedCocoa: str)))
    return try work(.string(tmpStr, true))
    #endif
  case .mutable(let storage):
    return try work(storage.asString)
  case .cocoa(let str):
    let length = _stdlib_binary_CFStringGetLength(str)
    guard let ascii = _cocoaASCIIPointer(str) else {
      let tmp = _StringGuts(
        cocoa: str,
        providesFastUTF8: false,
        isASCII: false,
        length: length
      )
      return try work(tmp)
    }
    return try work(String(_StringGuts(ascii, isASCII: true)))
  }
}

@usableFromInline
@_effects(releasenone) // @opaque
internal func _bridgeCocoaString(_ cocoaString: _CocoaString) -> _StringGuts {
  switch _KnownCocoaString(cocoaString) {
  case .storage(let storage):
    return storage.asString._guts
  case .shared(let storage):
    return storage.asString._guts
  case .mutable(let storage):
    return storage.asString._guts
#if !(arch(i386) || arch(arm))
  case .tagged(let str):
    return _StringGuts(_SmallString(taggedCocoa: str))
#endif
  case .cocoa(let str):
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
      = _stdlib_binary_CFStringCreateCopy(str) as AnyObject

#if !(arch(i386) || arch(arm))
    if _isObjCTaggedPointer(immutableCopy) {
      return _StringGuts(_SmallString(taggedCocoa: immutableCopy))
    }
#endif

    let ascii = _cocoaASCIIPointer(immutableCopy)
    let length = _stdlib_binary_CFStringGetLength(immutableCopy)
    return _StringGuts(
      cocoa: immutableCopy,
      providesFastUTF8: ascii != nil,
      isASCII: ascii != nil,
      length: length)
  }
}

extension String {
  public // SPI(Foundation)
  init(_cocoaString: AnyObject) {
    self._guts = _bridgeCocoaString(_cocoaString)
  }
}

@_effects(releasenone)
private func _createNSString(
  _ receiver: _StringSelectorHolder,
  _ ptr: UnsafePointer<UInt8>,
  _ count: Int,
  _ encoding: UInt32
) -> AnyObject? {
  return receiver.createTaggedString(bytes: ptr, count: count)
}

@_effects(releasenone)
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
  @_effects(releasenone)
  public // SPI(Foundation)
  func _bridgeToObjectiveCImpl() -> AnyObject {
    
    _connectOrphanedFoundationSubclassesIfNeeded()
    
    // Smol ASCII a) may bridge to tagged pointers, b) can't contain a BOM
    if _guts.isSmallASCII {
      let maybeTagged = _guts.asSmall.withUTF8 { bufPtr in
        return _createCFString(
          bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
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

@available(macOS, introduced: 9999, deprecated)
@available(iOS, introduced: 9999, deprecated)
@available(watchOS, introduced: 9999, deprecated)
@available(tvOS, introduced: 9999, deprecated)
@available(*, deprecated)
@_cdecl("_SwiftCreateBridgedMutableString")
@usableFromInline
internal func _SwiftCreateBridgedMutableString(
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
  return Unmanaged<AnyObject>.passRetained(_SwiftNSMutableString(str))
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

@_fixed_layout
@usableFromInline
@objc internal final class _SwiftNSMutableString :
  _SwiftNativeNSMutableString, _AbstractStringStorage {
  private var _contents: String
  
  internal init(_ str: String) {
    _contents = str
    assert(_contents._guts.isFastUTF8)
    super.init()
  }
  
  final internal var asString: String {
    @_effects(readonly) @inline(__always) get {
      return _contents
    }
  }
  
  @inline(__always)
  final func withFastUTF8<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    return try _contents._guts.withFastUTF8(f)
  }
  
  @inline(__always)
  internal var start: UnsafePointer<UInt8>? {
     let guts = _contents._guts
     if !guts.isSmall && guts.isFastUTF8 {
       return guts._object.fastUTF8.baseAddress!
     }
     return nil
  }
  
  @inline(__always)
  internal var count: Int { return _contents.utf8.count }
  
  @inline(__always)
  final internal var isASCII: Bool {
    return _contents._guts.isASCII
  }
  
  @objc(length)
  final internal var UTF16Length: Int {
    @_effects(readonly) get {
      return _contents.utf16.count // UTF16View special-cases ASCII for us.
    }
  }

  @objc
  final internal var hash: UInt {
    @_effects(readonly) get {
      return _getCocoaHash()
    }
  }

  @objc(characterAtIndex:)
  @_effects(readonly)
  final internal func character(at offset: Int) -> UInt16 {
    return _contents.utf16[_contents._toUTF16Index(offset)]
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
      return _getFastestEncoding()
    }
  }

  @objc(_fastCStringContents:)
  @_effects(readonly)
  final internal func _fastCStringContents(
    _ requiresNulTermination: Int8
  ) -> UnsafePointer<CChar>? {
    let guts = _contents._guts
    guard !guts.isSmall && guts.isASCII else {
      return nil
    }
    assert(guts.isFastUTF8)
    return guts._object.fastUTF8.baseAddress._unsafelyUnwrappedUnchecked._asCChar
  }

  @objc(UTF8String)
  @_effects(readonly)
  final internal func _utf8String() -> UnsafePointer<UInt8> {
    let guts = _contents._guts
    guard !guts.isSmall else {
      // This is Cocoa's trick for returning an "autoreleased char *", but using
      // our CoW to make it a bit faster
      let anchor = _contents._bridgeToObjectiveCImpl()
      let unmanagedAnchor = Unmanaged.passRetained(anchor)
      _ = unmanagedAnchor.autorelease()
      return _cocoaStringGetUTF8Pointer(anchor)
    }
    assert(guts.isFastUTF8)
    return guts._object.fastUTF8.baseAddress._unsafelyUnwrappedUnchecked
    
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
  final internal func isEqual(to other:AnyObject?) -> Int8 {
    return _isEqual(other)
  }

  @objc(copyWithZone:)
  final internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While __StringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return _contents._bridgeToObjectiveCImpl()
  }
  
  @objc(copy)
  final internal func copy() -> AnyObject {
    return _contents._bridgeToObjectiveCImpl()
  }
  
  @objc(mutableCopyWithZone:)
  final internal func mutableCopy(with zone: _SwiftNSZone?) -> AnyObject {
    return _SwiftNSMutableString(_contents)
  }
  
  @objc(mutableCopy)
  final internal func mutableCopy() -> AnyObject {
    return _SwiftNSMutableString(_contents)
  }
  
  final internal func _convertIncomingNSRange(_ range: _SwiftNSRange)
    -> (Range<String.Index>, scalarAligned: Bool) {
    let range = _contents._toUTF16Indices(
      range.location ..< range.location + range.length
    )
    let rs = range.startIndex
    let re = range.endIndex
    let scalarAligned =
      (rs._isScalarAligned || rs.transcodedOffset == 0) &&
      (re._isScalarAligned || re.transcodedOffset == 0)
    return (range, scalarAligned)
  }
  
  final func _replace(in nsRange: _SwiftNSRange, with str: String) {
    let (range, scalarAligned) = _convertIncomingNSRange(nsRange)
    guard _fastPath(scalarAligned) else {
      // Slow path: if they're doing something like replacing one half of a
      // surrogate pair, then we play things safe and transcode to UTF16
      var utf16 = Array(_contents.utf16)
      let cocoaRange = nsRange.location ..< nsRange.location + nsRange.length
      utf16.replaceSubrange(cocoaRange, with: str)
      _contents = String(decoding: utf16, as: UTF16.self)
      return
    }
    _contents.replaceSubrange(range, with: str)
  }
  
  @objc(replaceCharactersInRange:withString:)
  final func replaceCharacters(
    in range: _SwiftNSRange,
    with aString: _CocoaString?
  ) {
    _withTemporaryBridgedCocoaString(string) {
      _replace(in: range, with: $0)
    }
  }
  
  @objc(appendString:)
  final func appendString(_ aString: _CocoaString?) {
    _withTemporaryBridgedCocoaString(aString) {
      _contents.append($0)
      return
    }
  }
  
  @objc(deleteCharactersInRange:)
  final func deleteCharacters(in range: _SwiftNSRange) {
    _replace(in: range, with: "")
  }
  
  @objc(insertString:atIndex:)
  final func insert(str: _CocoaString?, at offset: Int) {
    replaceCharacters(in: _SwiftNSRange(location: offset, length: 0), with: str)
  }
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
