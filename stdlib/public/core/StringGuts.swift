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

//
// StringGuts is a parameterization over String's representations. It provides
// functionality and guidance for efficiently working with Strings.
//
@_fixed_layout @usableFromInline
internal struct _StringGuts {
  @usableFromInline
  internal var _object: _StringObject

  @inlinable @inline(__always)
  internal init(_ object: _StringObject) {
    self._object = object
    _invariantCheck()
  }

  // Empty string
  @inlinable @inline(__always)
  init() {
    self.init(_StringObject(empty: ()))
  }
}

// Raw
extension _StringGuts {
  @usableFromInline
  internal typealias RawBitPattern = _StringObject.RawBitPattern

  @inlinable
  internal var rawBits: RawBitPattern {
    @inline(__always) get { return _object.rawBits }
  }

  @inlinable @inline(__always)
  init(raw bits: RawBitPattern) {
    self.init(_StringObject(raw: bits))
  }

}

// Creation
extension _StringGuts {
  @inlinable @inline(__always)
  internal init(_ smol: _SmallString) {
    self.init(_StringObject(smol))
  }

  @inlinable @inline(__always)
  internal init(_ bufPtr: UnsafeBufferPointer<UInt8>, isKnownASCII: Bool) {
    self.init(_StringObject(immortal: bufPtr, isASCII: isKnownASCII))
  }

  @inlinable @inline(__always)
  internal init(_ storage: _StringStorage) {
    // TODO(UTF8): We should probably store perf flags on the storage's capacity
    self.init(_StringObject(storage, isASCII: false))
  }

  internal init(_ storage: _SharedStringStorage) {
    // TODO(UTF8 perf): Is it better, or worse, to throw the object away if
    // immortal? We could avoid having to ARC the object itself when owner is
    // nil, at the cost of repeatedly creating a new one when this string is
    // bridged back-and-forth to ObjC. For now, we're choosing to keep the
    // object allocation, perform some ARC, and require a dependent load for
    // literals that have bridged back in from ObjC. Long term, we will likely
    // emit some kind of "immortal" object for literals with efficient bridging
    // anyways, so this may be a short-term decision.

    // TODO(UTF8): We should probably store perf flags in the object
    self.init(_StringObject(storage, isASCII: false))
  }

  internal init(cocoa: AnyObject, providesFastUTF8: Bool, length: Int) {
    self.init(_StringObject(
      cocoa: cocoa, providesFastUTF8: providesFastUTF8, length: length))
  }
}

// Queries
extension _StringGuts {
  // The number of code units
  @inlinable
  internal var count: Int { @inline(__always) get { return _object.count } }

  @inlinable
  internal var isEmpty: Bool { @inline(__always) get { return count == 0 } }

  @inlinable
  internal var isKnownASCII: Bool  {
    @inline(__always) get { return _object.isASCII }
  }

  @inlinable
  internal var hasNativeStorage: Bool { return _object.hasNativeStorage }

  internal var hasSharedStorage: Bool { return _object.hasSharedStorage }

  internal var hasBreadcrumbs: Bool {
    return hasNativeStorage || hasSharedStorage
  }
}

//
extension _StringGuts {
  // Whether we can provide fast access to contiguous UTF-8 code units
  @inlinable
  internal var isFastUTF8: Bool {
    @inline(__always) get {
      // TODO(UTF8 merge): Can we add the Builtin.expected here?
      return _object.providesFastUTF8
    }
  }
  // A String which does not provide fast access to contiguous UTF-8 code units
  @inlinable
  internal var isForeign: Bool {
    @inline(__always) get { return _object.isForeign }
  }

  @inlinable @inline(__always)
  internal func withFastUTF8<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    _sanityCheck(isFastUTF8)

    if _object.isSmall { return try _object.asSmallString.withUTF8(f) }

    defer { _fixLifetime(self) }
    return try f(_object.fastUTF8)
  }

  @inlinable @inline(__always)
  internal func withUTF8IfAvailable<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R? {
    if _slowPath(isForeign) { return nil }
    return try withFastUTF8(f)
  }
}

// Internal invariants
extension _StringGuts {
  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    _object._invariantCheck()
    #if arch(i386) || arch(arm)
    _sanityCheck(MemoryLayout<String>.size == 12, """
    the runtime is depending on this, update Reflection.mm and \
    this if you change it
    """)
    #else
    _sanityCheck(MemoryLayout<String>.size == 16, """
    the runtime is depending on this, update Reflection.mm and \
    this if you change it
    """)
    #endif
  }
  #endif // INTERNAL_CHECKS_ENABLED

  internal func _dump() { _object._dump() }
}

// C String interop
extension _StringGuts {
  @inlinable @inline(__always) // fast-path: already C-string compatible
  internal func withCString<Result>(
    _ body: (UnsafePointer<Int8>) throws -> Result
  ) rethrows -> Result {
    if _slowPath(!_object.isFastZeroTerminated) {
      return try _slowWithCString(body)
    }

    return try self.withFastUTF8 {
      let ptr = $0._asCChar.baseAddress._unsafelyUnwrappedUnchecked
      return try body(ptr)
    }
  }

  @inline(never) // slow-path
  @usableFromInline
  internal func _slowWithCString<Result>(
    _ body: (UnsafePointer<Int8>) throws -> Result
  ) rethrows -> Result {
    _sanityCheck(!_object.isFastZeroTerminated)
    return try String(self).utf8CString.withUnsafeBufferPointer {
      let ptr = $0.baseAddress._unsafelyUnwrappedUnchecked
      return try body(ptr)
    }
  }
}

extension _StringGuts {
  // Copy UTF-8 contents. Returns number written or nil if not enough space.
  // Contents of the buffer are unspecified if nil is returned.
  @inlinable
  internal func copyUTF8(into mbp: UnsafeMutableBufferPointer<UInt8>) -> Int? {
    let ptr = mbp.baseAddress._unsafelyUnwrappedUnchecked
    if _fastPath(self.isFastUTF8) {
      return self.withFastUTF8 { utf8 in
        guard utf8.count <= mbp.count else { return nil }

        let utf8Start = utf8.baseAddress._unsafelyUnwrappedUnchecked
        ptr.initialize(from: utf8Start, count: utf8.count)
        return utf8.count
      }
    }

    return _foreignCopyUTF8(into: mbp)
  }

  @_effects(releasenone)
  @usableFromInline @inline(never) // slow-path
  internal func _foreignCopyUTF8(
    into mbp: UnsafeMutableBufferPointer<UInt8>
  ) -> Int? {
    var ptr = mbp.baseAddress._unsafelyUnwrappedUnchecked
    var numWritten = 0
    for cu in String(self).utf8 {
      guard numWritten < mbp.count else { return nil }
      ptr.initialize(to: cu)
      ptr += 1
      numWritten += 1
    }

    return numWritten
  }

  internal var utf8Count: Int {
    @inline(__always) get {
      // TODO: Might be worth burning a bit for count-is-UTF-8, regardless of
      // fast status.
      if _fastPath(self.isFastUTF8) {
        return self.count
      }
      return _foreignUTF8Count()
    }
  }

  @_effects(releasenone)
  @inline(never) // slow-path
  internal func _foreignUTF8Count() -> Int {
    return String(self).utf8.count
  }
}

// Index
extension _StringGuts {
  @usableFromInline
  internal typealias Index = String.Index

  @inlinable
  internal var startIndex: String.Index {
    @inline(__always) get { return Index(encodedOffset: 0) }
  }
  @inlinable
  internal var endIndex: String.Index {
    @inline(__always) get { return Index(encodedOffset: self.count) }
  }
}
