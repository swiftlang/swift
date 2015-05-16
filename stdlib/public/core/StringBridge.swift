//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

#if _runtime(_ObjC)
// Swift's String bridges NSString via this protocol and these
// variables, allowing the core stdlib to remain decoupled from
// Foundation.

/// Effectively a proxy for NSString that doesn't mention it by
/// name.  NSString's conformance to this protocol is declared in
/// Foundation.
@objc public protocol _CocoaStringType {}

public // @testable
func _stdlib_binary_CFStringCreateCopy(
  source: _CocoaStringType
) -> _CocoaStringType {
  let result = CFStringCreateCopy(nil, source)
  Builtin.release(result)
  return unsafeBitCast(result, _CocoaStringType.self)
}

public // @testable
func _stdlib_binary_CFStringGetLength(
  source: _CocoaStringType
) -> Int {
  return CFStringGetLength(source)
}

public // @testable
func _stdlib_binary_CFStringGetCharactersPtr(
  source: _CocoaStringType
) -> UnsafeMutablePointer<UTF16.CodeUnit> {
  return UnsafeMutablePointer(CFStringGetCharactersPtr(source))
}

/// Bridges `source` to `Swift.String`, assuming that `source` has non-ASCII
/// characters (does not apply ASCII optimizations).
@inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
func _cocoaStringToSwiftString_NonASCII(
  source: _CocoaStringType
) -> String {
  let cfImmutableValue = _stdlib_binary_CFStringCreateCopy(source)
  let length = _stdlib_binary_CFStringGetLength(cfImmutableValue)
  let start = _stdlib_binary_CFStringGetCharactersPtr(cfImmutableValue)

  return String(_StringCore(
    baseAddress: COpaquePointer(start),
    count: length,
    elementShift: 1,
    hasCocoaBuffer: true,
    owner: unsafeBitCast(cfImmutableValue, Optional<AnyObject>.self)))
}

/// Loading Foundation initializes these function variables
/// with useful values

/// Produces a `_StringBuffer` from a given subrange of a source
/// `_CocoaStringType`, having the given minimum capacity.
@inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
internal func _cocoaStringToContiguous(
  source: _CocoaStringType, _ range: Range<Int>, minimumCapacity: Int
) -> _StringBuffer {
  _sanityCheck(CFStringGetCharactersPtr(source) == nil,
    "Known contiguously-stored strings should already be converted to Swift")

  let startIndex = range.startIndex
  let count = range.endIndex - startIndex

  let buffer = _StringBuffer(capacity: max(count, minimumCapacity), 
                             initialSize: count, elementWidth: 2)

  CFStringGetCharacters(
    source, _swift_shims_CFRange(location: startIndex, length: count), 
    UnsafeMutablePointer<_swift_shims_UniChar>(buffer.start))
  
  return buffer
}

/// Reads the entire contents of a _CocoaStringType into contiguous
/// storage of sufficient capacity.
@inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
internal func _cocoaStringReadAll(
  source: _CocoaStringType, _ destination: UnsafeMutablePointer<UTF16.CodeUnit>
) {
  CFStringGetCharacters(
    source, _swift_shims_CFRange(
      location: 0, length: CFStringGetLength(source)), destination)
}

@inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
internal func _cocoaStringSlice(
  target: _StringCore, _ subRange: Range<Int>
) -> _StringCore {
  _sanityCheck(target.hasCocoaBuffer)
  
  let cfSelf: _swift_shims_CFStringRef = unsafeUnwrap(target.cocoaBuffer)
  
  _sanityCheck(
    CFStringGetCharactersPtr(cfSelf) == nil,
    "Known contiguously-stored strings should already be converted to Swift")

  let cfResult: AnyObject = CFStringCreateWithSubstring(
    nil, cfSelf, _swift_shims_CFRange(
      location: subRange.startIndex, length: subRange.count()))

  return String(_cocoaString: cfResult)._core
}

@inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
internal func _cocoaStringSubscript(
  target: _StringCore, _ position: Int
) -> UTF16.CodeUnit {
  let cfSelf: _swift_shims_CFStringRef = unsafeUnwrap(target.cocoaBuffer)

  _sanityCheck(CFStringGetCharactersPtr(cfSelf)._isNull,
    "Known contiguously-stored strings should already be converted to Swift")

  return CFStringGetCharacterAtIndex(cfSelf, position)
}

//
// Conversion from NSString to Swift's native representation
//

internal var kCFStringEncodingASCII : _swift_shims_CFStringEncoding {
  return 0x0600
}

extension String {
  @inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
  public // SPI(Foundation)
  init(_cocoaString: AnyObject) {
    if let wrapped = _cocoaString as? _NSContiguousString {
      self._core = wrapped._core
      return
    }

    // Treat it as a CF object because presumably that's what these
    // things tend to be, and CF has a fast path that avoids
    // objc_msgSend
    let cfValue = unsafeBitCast(_cocoaString, _CocoaStringType.self)

    // "copy" it into a value to be sure nobody will modify behind
    // our backs.  In practice, when value is already immutable, this
    // just does a retain.
    let cfImmutableValue: _swift_shims_CFStringRef
      = _stdlib_binary_CFStringCreateCopy(cfValue)

    let length = CFStringGetLength(cfImmutableValue)

    // Look first for null-terminated ASCII
    // Note: the code in clownfish appears to guarantee
    // nul-termination, but I'm waiting for an answer from Chris Kane
    // about whether we can count on it for all time or not.
    let nulTerminatedASCII = CFStringGetCStringPtr(
      cfImmutableValue, kCFStringEncodingASCII)

    // start will hold the base pointer of contiguous storage, if it
    // is found.
    var start = UnsafeMutablePointer<RawByte>(nulTerminatedASCII)
    let isUTF16 = nulTerminatedASCII._isNull
    if (isUTF16) {
      start = UnsafeMutablePointer(CFStringGetCharactersPtr(cfImmutableValue))
    }

    self._core = _StringCore(
      baseAddress: COpaquePointer(start),
      count: length,
      elementShift: isUTF16 ? 1 : 0,
      hasCocoaBuffer: true,
      owner: unsafeBitCast(cfImmutableValue, Optional<AnyObject>.self))
  }
}

// At runtime, this class is derived from `_SwiftNativeNSStringBase`,
// which is derived from `NSString`.
//
// The @_swift_native_objc_runtime_base attribute
// This allows us to subclass an Objective-C class and use the fast Swift
// memory allocator.
@objc @_swift_native_objc_runtime_base(_SwiftNativeNSStringBase)
public class _SwiftNativeNSString {}

@objc
public protocol _NSStringCoreType :
    _NSCopyingType, _NSFastEnumerationType {

  // The following methods should be overridden when implementing an
  // NSString subclass.

  func length() -> Int

  func characterAtIndex(index: Int) -> UInt16

  // We also override the following methods for efficiency.
}

/// An `NSString` built around a slice of contiguous Swift `String` storage.
public final class _NSContiguousString : _SwiftNativeNSString {
  public init(_ _core: _StringCore) {
    _sanityCheck(
      _core.hasContiguousStorage,
      "_NSContiguousString requires contiguous storage")
    self._core = _core
    super.init()
  }

  init(coder aDecoder: AnyObject) {
    _sanityCheckFailure("init(coder:) not implemented for _NSContiguousString")
  }

  func length() -> Int {
    return _core.count
  }

  func characterAtIndex(index: Int) -> UInt16 {
    return _core[index]
  }

  func getCharacters(
    buffer: UnsafeMutablePointer<UInt16>,
    range aRange: _SwiftNSRange) {

    _precondition(aRange.location + aRange.length <= Int(_core.count))

    if _core.elementWidth == 2 {
      UTF16._copy(
        _core.startUTF16 + aRange.location,
        destination: UnsafeMutablePointer<UInt16>(buffer),
        count: aRange.length)
    }
    else {
      UTF16._copy(
        _core.startASCII + aRange.location,
        destination: UnsafeMutablePointer<UInt16>(buffer),
        count: aRange.length)
    }
  }

  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16> {
    return _core.elementWidth == 2
      ? UnsafeMutablePointer(_core.startUTF16) : nil
  }

  //
  // Implement sub-slicing without adding layers of wrapping
  //
  func substringFromIndex(start: Int) -> _NSContiguousString {
    return _NSContiguousString(_core[Int(start)..<Int(_core.count)])
  }

  func substringToIndex(end: Int) -> _NSContiguousString {
    return _NSContiguousString(_core[0..<Int(end)])
  }

  func substringWithRange(aRange: _SwiftNSRange) -> _NSContiguousString {
    return _NSContiguousString(
      _core[Int(aRange.location)..<Int(aRange.location + aRange.length)])
  }

  func copy() -> AnyObject {
    // Since this string is immutable we can just return ourselves.
    return self
  }

  public let _core: _StringCore
}

extension String {
  /// Same as `_bridgeToObjectiveC()`, but located inside the core standard
  /// library.
  public func _stdlib_binary_bridgeToObjectiveCImpl() -> AnyObject {
    if let ns = _core.cocoaBuffer where CFStringGetLength(ns) == _core.count {
      return ns
    }
    _sanityCheck(_core.hasContiguousStorage)
    return _NSContiguousString(_core)
  }

  @inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
  public func _bridgeToObjectiveCImpl() -> AnyObject {
    return _stdlib_binary_bridgeToObjectiveCImpl()
  }
}
#endif
