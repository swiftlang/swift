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

@_effects(releasenone)
internal func _measureCharacterStride(
  of utf8: UnsafeBufferPointer<UInt8>, startingAt i: Int
) -> Int {
  let iterator = _ThreadLocalStorage.getUBreakIterator(utf8)
  let offset = __swift_stdlib_ubrk_following(
    iterator, Int32(truncatingIfNeeded: i))
  // ubrk_following returns -1 (UBRK_DONE) when it hits the end of the buffer.
  if _fastPath(offset != -1) {
    // The offset into our buffer is the distance.
    _sanityCheck(offset > i, "zero-sized grapheme?")
    return Int(truncatingIfNeeded: offset) &- i
  }
  return utf8.count &- i
}

@_effects(releasenone)
internal func _measureCharacterStride(
  of utf16: UnsafeBufferPointer<UInt16>, startingAt i: Int
) -> Int {
  let iterator = _ThreadLocalStorage.getUBreakIterator(utf16)
  let offset = __swift_stdlib_ubrk_following(
    iterator, Int32(truncatingIfNeeded: i))
  // ubrk_following returns -1 (UBRK_DONE) when it hits the end of the buffer.
  if _fastPath(offset != -1) {
    // The offset into our buffer is the distance.
    _sanityCheck(offset > i, "zero-sized grapheme?")
    return Int(truncatingIfNeeded: offset) &- i
  }
  return utf16.count &- i
}

@_effects(releasenone)
internal func _measureCharacterStride(
  of utf8: UnsafeBufferPointer<UInt8>, endingAt i: Int
) -> Int {
  let iterator = _ThreadLocalStorage.getUBreakIterator(utf8)
  let offset = __swift_stdlib_ubrk_preceding(
    iterator, Int32(truncatingIfNeeded: i))
  // ubrk_following returns -1 (UBRK_DONE) when it hits the end of the buffer.
  if _fastPath(offset != -1) {
    // The offset into our buffer is the distance.
    _sanityCheck(offset < i, "zero-sized grapheme?")
    return i &- Int(truncatingIfNeeded: offset)
  }
  return i &- utf8.count
}

@_effects(releasenone)
internal func _measureCharacterStride(
  of utf16: UnsafeBufferPointer<UInt16>, endingAt i: Int
) -> Int {
  let iterator = _ThreadLocalStorage.getUBreakIterator(utf16)
  let offset = __swift_stdlib_ubrk_preceding(
    iterator, Int32(truncatingIfNeeded: i))
  // ubrk_following returns -1 (UBRK_DONE) when it hits the end of the buffer.
  if _fastPath(offset != -1) {
    // The offset into our buffer is the distance.
    _sanityCheck(offset < i, "zero-sized grapheme?")
    return i &- Int(truncatingIfNeeded: offset)
  }
  return i &- utf16.count
}

extension _StringGuts {
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func isOnGraphemeClusterBoundary(_ i: String.Index) -> Bool {
    let offset = i.encodedOffset
    if offset == 0 || offset == self.count { return true }

    guard isOnUnicodeScalarBoundary(i) else { return false }

    let str = String(self)
    return i == str.index(before: str.index(after: i))
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _opaqueCharacterStride(startingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignOpaqueCharacterStride(startingAt: i)
    }

    // TODO(UTF8 perf): grapheme breaking fast-paths...

    return self.withFastUTF8 {
      return _measureCharacterStride(of: $0, startingAt: i)
    }
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignOpaqueCharacterStride(startingAt i: Int) -> Int {
    _sanityCheck(isForeign)

    // TODO(UTF8 perf): grapheme breaking fast-paths...

    // TODO(UTF8 perf): local stack first, before nuclear solution
    // TODO(UTF8 perf): even nuclear solution should copy to larger arrays in a
    //                  loop

    let count = _object.largeCount
    let cocoa = _object.cocoaObject
    var codeUnits = Array<UInt16>(repeating: 0, count: count)

    codeUnits.withUnsafeMutableBufferPointer {
      _cocoaStringCopyCharacters(
        from: cocoa,
        range: 0..<count,
        into: $0.baseAddress._unsafelyUnwrappedUnchecked)
    }
    return codeUnits.withUnsafeBufferPointer {
      _measureCharacterStride(of: $0, startingAt: i)
    }
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _opaqueCharacterStride(endingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignOpaqueCharacterStride(endingAt: i)
    }

    // TODO(UTF8 perf): grapheme breaking fast-paths...

    return self.withFastUTF8 {
      return _measureCharacterStride(of: $0, endingAt: i)
    }
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignOpaqueCharacterStride(endingAt i: Int) -> Int {
    _sanityCheck(isForeign)

    // TODO(UTF8 perf): grapheme breaking fast-paths...

    // TODO(UTF8 perf): local stack first, before nuclear solution
    // TODO(UTF8 perf): even nuclear solution should copy to larger arrays in a
    //                  loop

    let count = _object.largeCount
    let cocoa = _object.cocoaObject
    var codeUnits = Array<UInt16>(repeating: 0, count: count)

    codeUnits.withUnsafeMutableBufferPointer {
      _cocoaStringCopyCharacters(
        from: cocoa,
        range: 0..<count,
        into: $0.baseAddress._unsafelyUnwrappedUnchecked)
    }
    return codeUnits.withUnsafeBufferPointer {
      _measureCharacterStride(of: $0, endingAt: i)
    }
  }
}

