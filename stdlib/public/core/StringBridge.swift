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

@inlinable // FIXME(sil-serialize-all)
public // @testable
func _stdlib_binary_CFStringCreateCopy(
  _ source: _CocoaString
) -> _CocoaString {
  let result = _swift_stdlib_CFStringCreateCopy(nil, source) as AnyObject
  return result
}

@inlinable // FIXME(sil-serialize-all)
@effects(readonly)
public // @testable
func _stdlib_binary_CFStringGetLength(
  _ source: _CocoaString
) -> Int {
  return _swift_stdlib_CFStringGetLength(source)
}

@inlinable // FIXME(sil-serialize-all)
public // @testable
func _stdlib_binary_CFStringGetCharactersPtr(
  _ source: _CocoaString
) -> UnsafeMutablePointer<UTF16.CodeUnit>? {
  return UnsafeMutablePointer(
    mutating: _swift_stdlib_CFStringGetCharactersPtr(source))
}

/// Loading Foundation initializes these function variables
/// with useful values

/// Copies the entire contents of a _CocoaString into contiguous
/// storage of sufficient capacity.
@usableFromInline // FIXME(sil-serialize-all)
@inline(never) // Hide the CF dependency
internal func _cocoaStringReadAll(
  _ source: _CocoaString, _ destination: UnsafeMutablePointer<UTF16.CodeUnit>
) {
  _swift_stdlib_CFStringGetCharacters(
    source, _swift_shims_CFRange(
      location: 0, length: _swift_stdlib_CFStringGetLength(source)), destination)
}

/// Copies a slice of a _CocoaString into contiguous storage of
/// sufficient capacity.
@usableFromInline // FIXME(sil-serialize-all)
@inline(never) // Hide the CF dependency
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

@usableFromInline // FIXME(sil-serialize-all)
@inline(never) // Hide the CF dependency
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

@usableFromInline // FIXME(sil-serialize-all)
@inline(never) // Hide the CF dependency
internal func _cocoaStringSubscript(
  _ target: _CocoaString, _ position: Int
) -> UTF16.CodeUnit {
  let cfSelf: _swift_shims_CFStringRef = target

  _sanityCheck(_swift_stdlib_CFStringGetCharactersPtr(cfSelf) == nil,
    "Known contiguously stored strings should already be converted to Swift")

  return _swift_stdlib_CFStringGetCharacterAtIndex(cfSelf, position)
}

//
// Conversion from NSString to Swift's native representation
//

@inlinable // FIXME(sil-serialize-all)
internal var kCFStringEncodingASCII : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x0600 }
}
@inlinable // FIXME(sil-serialize-all)
internal var kCFStringEncodingUTF8 : _swift_shims_CFStringEncoding {
  @inline(__always) get { return 0x8000100 }
}

@usableFromInline // @opaque
internal func _bridgeASCIICocoaString(
  _ cocoa: _CocoaString,
  intoUTF8 bufPtr: UnsafeMutableRawBufferPointer
) -> Int? {
  let ptr = bufPtr.baseAddress._unsafelyUnwrappedUnchecked.assumingMemoryBound(
    to: UInt8.self)
  let length = _stdlib_binary_CFStringGetLength(cocoa)
  var count = 0
  let numCharWritten = _swift_stdlib_CFStringGetBytes(
    cocoa, _swift_shims_CFRange(location: 0, length: length),
    kCFStringEncodingUTF8, 0, 0, ptr, bufPtr.count, &count)
  return length == numCharWritten ? count : nil
}

public // @testable
func _bridgeToCocoa(_ small: _SmallUTF8String) -> _CocoaString {
  return small.withUTF8CodeUnits { bufPtr in
      return _swift_stdlib_CFStringCreateWithBytes(
          nil, bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
          bufPtr.count,
          small.isASCII ? kCFStringEncodingASCII : kCFStringEncodingUTF8, 0)
      as AnyObject
  }
}

internal func _getCocoaStringPointer(
  _ cfImmutableValue: _CocoaString
) -> (UnsafeRawPointer?, isUTF16: Bool)  {
  // Look first for null-terminated ASCII
  // Note: the code in clownfish appears to guarantee
  // nul-termination, but I'm waiting for an answer from Chris Kane
  // about whether we can count on it for all time or not.
  let nulTerminatedASCII = _swift_stdlib_CFStringGetCStringPtr(
    cfImmutableValue, kCFStringEncodingASCII)

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
@inline(never) // Hide the CF dependency
internal
func _makeCocoaStringGuts(_ cocoaString: _CocoaString) -> _StringGuts {
  if let ascii = cocoaString as? _ASCIIStringStorage {
    return _StringGuts(_large: ascii)
  } else if let utf16 = cocoaString as? _UTF16StringStorage {
    return _StringGuts(_large: utf16)
  } else if let wrapped = cocoaString as? _NSContiguousString {
    return wrapped._guts
  } else if _isObjCTaggedPointer(cocoaString) {
    guard let small = _SmallUTF8String(_cocoaString: cocoaString) else {
      fatalError("Internal invariant violated: large tagged NSStrings")
    }
    return _StringGuts(small)
  }
  // "copy" it into a value to be sure nobody will modify behind
  // our backs.  In practice, when value is already immutable, this
  // just does a retain.
  let immutableCopy
    = _stdlib_binary_CFStringCreateCopy(cocoaString) as AnyObject

  if _isObjCTaggedPointer(immutableCopy) {
    guard let small = _SmallUTF8String(_cocoaString: cocoaString) else {
      fatalError("Internal invariant violated: large tagged NSStrings")
    }
    return _StringGuts(small)
  }

  let (start, isUTF16) = _getCocoaStringPointer(immutableCopy)

  let length = _StringGuts.getCocoaLength(
    _unsafeBitPattern: Builtin.reinterpretCast(immutableCopy))

  // TODO(SSO): And also for UTF-16 strings and non-contiguous strings
  if let ptr = start, !isUTF16 && length <= _SmallUTF8String.capacity {
    if let small = _SmallUTF8String(
      UnsafeBufferPointer(
        start: ptr.assumingMemoryBound(to: UInt8.self), count: length)
    ) {
      return _StringGuts(small)
    } else {
#if arch(i386) || arch(arm)
#else
      _sanityCheckFailure("Couldn't fit 15-char ASCII small string?")
#endif
    }
  }

  return _StringGuts(
    _largeNonTaggedCocoaObject: immutableCopy,
    count: length,
    isSingleByte: !isUTF16,
    start: start)
}

extension String {
  public // SPI(Foundation)
  init(_cocoaString: AnyObject) {
    self._guts = _makeCocoaStringGuts(_cocoaString)
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
}

/// An `NSString` built around a slice of contiguous Swift `String` storage.
@_fixed_layout // FIXME(sil-serialize-all)
public final class _NSContiguousString : _SwiftNativeNSString, _NSStringCore {
  public let _guts: _StringGuts

  @inlinable // FIXME(sil-serialize-all)
  public init(_ _guts: _StringGuts) {
    _sanityCheck(!_guts._isOpaque,
      "_NSContiguousString requires contiguous storage")
    self._guts = _guts
    super.init()
  }

  @inlinable // FIXME(sil-serialize-all)
  public init(_unmanaged guts: _StringGuts) {
    _sanityCheck(!guts._isOpaque,
      "_NSContiguousString requires contiguous storage")
    if guts.isASCII {
      self._guts = _StringGuts(_large: guts._unmanagedASCIIView)
    } else {
      self._guts = _StringGuts(_large: guts._unmanagedUTF16View)
    }
    super.init()
  }

  @inlinable // FIXME(sil-serialize-all)
  public init(_unmanaged guts: _StringGuts, range: Range<Int>) {
    _sanityCheck(!guts._isOpaque,
      "_NSContiguousString requires contiguous storage")
    if guts.isASCII {
      self._guts = _StringGuts(_large: guts._unmanagedASCIIView[range])
    } else {
      self._guts = _StringGuts(_large: guts._unmanagedUTF16View[range])
    }
    super.init()
  }

  @usableFromInline // FIXME(sil-serialize-all)
  @objc
  init(coder aDecoder: AnyObject) {
    _sanityCheckFailure("init(coder:) not implemented for _NSContiguousString")
  }

  @inlinable // FIXME(sil-serialize-all)
  deinit {}

  @inlinable
  @objc(length)
  public var length: Int {
    return _guts.count
  }

  @inlinable
  @objc(characterAtIndex:)
  public func character(at index: Int) -> UInt16 {
    defer { _fixLifetime(self) }
    return _guts[index]
  }

  @inlinable
  @objc(getCharacters:range:)
  public func getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>,
    range aRange: _SwiftNSRange) {
    _precondition(aRange.location >= 0 && aRange.length >= 0)
    let range: Range<Int> = aRange.location ..< aRange.location + aRange.length
    _precondition(range.upperBound <= Int(_guts.count))

    if _guts.isASCII {
      _guts._unmanagedASCIIView[range]._copy(
        into: UnsafeMutableBufferPointer(start: buffer, count: range.count))
    } else {
      _guts._unmanagedUTF16View[range]._copy(
        into: UnsafeMutableBufferPointer(start: buffer, count: range.count))
    }
    _fixLifetime(self)
  }

  @inlinable
  @objc(_fastCharacterContents)
  public func _fastCharacterContents() -> UnsafePointer<UInt16>? {
    guard !_guts.isASCII else { return nil }
    return _guts._unmanagedUTF16View.start
  }

  @objc(copyWithZone:)
  public func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // Since this string is immutable we can just return ourselves.
    return self
  }

  /// The caller of this function guarantees that the closure 'body' does not
  /// escape the object referenced by the opaque pointer passed to it or
  /// anything transitively reachable form this object. Doing so
  /// will result in undefined behavior.
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
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
}

extension String {
  /// Same as `_bridgeToObjectiveC()`, but located inside the core standard
  /// library.
  @inlinable // FIXME(sil-serialize-all)
  public func _stdlib_binary_bridgeToObjectiveCImpl() -> AnyObject {
    if _guts._isSmall {
      return _bridgeToCocoa(_guts._smallUTF8String)
    }
    if let cocoa = _guts._underlyingCocoaString {
      return cocoa
    }
    return _NSContiguousString(_guts)
  }

  @inline(never) // Hide the CF dependency
  public func _bridgeToObjectiveCImpl() -> AnyObject {
    return _stdlib_binary_bridgeToObjectiveCImpl()
  }
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
