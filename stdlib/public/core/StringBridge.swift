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

extension String {
  @inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
  public // SPI(Foundation)
  init(_cocoaString: AnyObject) {
    // If the NSString is actually a Swift String in disguise, 
    // we can just copy out the internal repr.
    if let wrapped = _cocoaString as? _NSContiguousString {
      self._core = wrapped._core
      return
    }

    let cocoaString = unsafeBitCast(_cocoaString, to: _NSString.self)

    let length = cocoaString.length()

    // The 0 here is `nullRequired: false` (bad hack)
    let nulTerminatedASCII = cocoaString._fastCStringContents(0)

    let isUTF16 = (nulTerminatedASCII == nil)
    let buffer = _StringBuffer(capacity: length, 
                               initialSize: length, 
                               elementWidth: isUTF16 ? 2 : 1)

    // We try our darndest to just get a pointer and memcpy, falling back
    // to having the string do it for us with getCharacters.
    if isUTF16 {
      let bufPtr = buffer.start.assumingMemoryBound(to: UInt16.self)
      if let utf16Ptr = cocoaString._fastCharacterContents() {
        bufPtr.assign(from: utf16Ptr, count: length)
      } else {
        cocoaString.getCharacters(bufPtr)
      }
      
    } else {
      buffer.start.copyBytes(from: nulTerminatedASCII!, count: length)
    }

    self._core = _StringCore(buffer)
  }
}

// At runtime, this class is derived from `_SwiftNativeNSStringBase`,
// which is derived from `NSString`.
//
// The @_swift_native_objc_runtime_base attribute
// This allows us to subclass an Objective-C class and use the fast Swift
// memory allocator.
//
// Subclasses should provide:
// * func length() -> Int
// * func characterAtIndex(_ index: Int) -> UInt16
@objc @_swift_native_objc_runtime_base(_SwiftNativeNSStringBase)
public class _SwiftNativeNSString {}

/// An `NSString` built around a slice of contiguous Swift `String` storage.
public final class _NSContiguousString : _SwiftNativeNSString, _NSStringCore {
  public init(_ _core: _StringCore) {
    self._core = _core
    super.init()
  }

  init(coder aDecoder: AnyObject) {
    _sanityCheckFailure("init(coder:) not implemented for _NSContiguousString")
  }

  public func length() -> Int {
    return _core.count
  }

  public func characterAtIndex(_ index: Int) -> UInt16 {
    return _core[index]
  }

  @inline(__always) // Performance: To save on reference count operations.
  public func getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>,
    range aRange: _SwiftNSRange) {

    _precondition(aRange.location + aRange.length <= Int(_core.count))

    if _core.elementWidth == 2 {
      UTF16._copy(
        source: _core.startUTF16 + aRange.location,
        destination: UnsafeMutablePointer<UInt16>(buffer),
        count: aRange.length)
    }
    else {
      UTF16._copy(
        source: _core.startASCII + aRange.location,
        destination: UnsafeMutablePointer<UInt16>(buffer),
        count: aRange.length)
    }
  }

  @objc
  public func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    return _core.elementWidth == 2 ? _core.startUTF16 : nil
  }

  //
  // Implement sub-slicing without adding layers of wrapping
  //
  func substringFromIndex(_ start: Int) -> _NSContiguousString {
    return _NSContiguousString(_core[Int(start)..<Int(_core.count)])
  }

  func substringToIndex(_ end: Int) -> _NSContiguousString {
    return _NSContiguousString(_core[0..<Int(end)])
  }

  func substringWithRange(_ aRange: _SwiftNSRange) -> _NSContiguousString {
    return _NSContiguousString(
      _core[Int(aRange.location)..<Int(aRange.location + aRange.length)])
  }

  func copy() -> AnyObject {
    // Since this string is immutable we can just return ourselves.
    return self
  }

  /// The caller of this function guarantees that the closure 'body' does not
  /// escape the object referenced by the opaque pointer passed to it or
  /// anything transitively reachable form this object. Doing so
  /// will result in undefined behavior.
  @_semantics("self_no_escaping_closure")
  func _unsafeWithNotEscapedSelfPointer<Result>(
    _ body: (OpaquePointer) throws -> Result
  ) rethrows -> Result {
    let selfAsPointer = unsafeBitCast(self, to: OpaquePointer.self)
    defer {
      _fixLifetime(self)
    }
    return try body(selfAsPointer)
  }

  /// The caller of this function guarantees that the closure 'body' does not
  /// escape either object referenced by the opaque pointer pair passed to it or
  /// transitively reachable objects. Doing so will result in undefined
  /// behavior.
  @_semantics("pair_no_escaping_closure")
  func _unsafeWithNotEscapedSelfPointerPair<Result>(
    _ rhs: _NSContiguousString,
    _ body: (OpaquePointer, OpaquePointer) throws -> Result
  ) rethrows -> Result {
    let selfAsPointer = unsafeBitCast(self, to: OpaquePointer.self)
    let rhsAsPointer = unsafeBitCast(rhs, to: OpaquePointer.self)
    defer {
      _fixLifetime(self)
      _fixLifetime(rhs)
    }
    return try body(selfAsPointer, rhsAsPointer)
  }

  public let _core: _StringCore
}

extension String {
  /// Same as `_bridgeToObjectiveC()`, but located inside the core standard
  /// library.
  public func _stdlib_binary_bridgeToObjectiveCImpl() -> AnyObject {
    return _NSContiguousString(_core)
  }

  @inline(never) @_semantics("stdlib_binary_only") // Hide the CF dependency
  public func _bridgeToObjectiveCImpl() -> AnyObject {
    return _stdlib_binary_bridgeToObjectiveCImpl()
  }
}
#endif
