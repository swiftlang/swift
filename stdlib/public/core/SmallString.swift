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

@_fixed_layout @usableFromInline
internal struct _SmallString {
  @usableFromInline
  internal typealias RawBitPattern = (UInt64, UInt64)

  // Small strings are values; store them raw
  @usableFromInline
  internal var _storage: RawBitPattern

  @inlinable
  internal var rawBits: RawBitPattern {
    @inline(__always) get { return _storage }
  }

  @inlinable
  internal var leadingRawBits: UInt64 {
    @inline(__always) get { return _storage.0 }
    @inline(__always) set { _storage.0 = newValue }
  }

  @inlinable
  internal var trailingRawBits: UInt64 {
    @inline(__always) get { return _storage.1 }
    @inline(__always) set { _storage.1 = newValue }
  }

  @inlinable @inline(__always)
  internal init(rawUnchecked bits: RawBitPattern) {
    self._storage = bits
  }

  @inlinable @inline(__always)
  internal init(raw bits: RawBitPattern) {
    self.init(rawUnchecked: bits)
    _invariantCheck()
  }

  @inlinable @inline(__always)
  internal init(_ object: _StringObject) {
    _sanityCheck(object.isSmall)
    self.init(raw: object.rawBits)
  }

  @inlinable @inline(__always)
  internal init() {
    self.init(raw: _StringObject(empty:()).rawBits)
  }
}

// TODO
extension _SmallString {
  @inlinable
  internal static var capacity: Int {
    @inline(__always) get {
#if arch(i386) || arch(arm)
      return 10
#else
      return 15
#endif
    }
  }

  @inlinable
  internal var discriminator: _StringObject.Discriminator {
    @inline(__always) get {
      let value = _storage.1 &>> _StringObject.Nibbles.discriminatorShift
      return _StringObject.Discriminator(UInt8(truncatingIfNeeded: value))
    }
    @inline(__always) set {
      _storage.1 &= _StringObject.Nibbles.largeAddressMask
      _storage.1 |= (
        UInt64(truncatingIfNeeded: newValue._value)
          &<< _StringObject.Nibbles.discriminatorShift)
    }
  }

  @inlinable
  internal var capacity: Int {
    @inline(__always) get {
      return _SmallString.capacity
    }
  }

  @inlinable
  internal var count: Int {
    @inline(__always) get {
      return discriminator.smallCount
    }
  }

  @inlinable
  internal var unusedCapacity: Int {
    @inline(__always) get { return capacity &- count }
  }

  @inlinable
  internal var isASCII: Bool {
    @inline(__always) get {
      return discriminator.smallIsASCII
    }
  }

  // Give raw, nul-terminated code units. This is only for limited internal
  // usage: it always clears the discriminator and count (in case it's full)
  @inlinable
  internal var zeroTerminatedRawCodeUnits: RawBitPattern {
    @inline(__always) get {
      return (
        self._storage.0,
        self._storage.1 & _StringObject.Nibbles.largeAddressMask)
    }
  }

  @inlinable
  internal func computeIsASCII() -> Bool {
    // TODO(String micro-performance): Evaluate other expressions, e.g. | first
    let asciiMask: UInt64 = 0x8080_8080_8080_8080
    let raw = zeroTerminatedRawCodeUnits
    return (raw.0 & asciiMask == 0) && (raw.1 & asciiMask == 0)
  }
}

// Internal invariants
extension _SmallString {
  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    _sanityCheck(count <= _SmallString.capacity)
    _sanityCheck(isASCII == computeIsASCII())
  }
  #endif // INTERNAL_CHECKS_ENABLED

  internal func _dump() {
    #if INTERNAL_CHECKS_ENABLED
    print("""
      smallUTF8: count: \(self.count), codeUnits: \(
        self.map { String($0, radix: 16) }.joined()
      )
      """)
    #endif // INTERNAL_CHECKS_ENABLED
  }
}

// Provide a RAC interface
extension _SmallString: RandomAccessCollection, MutableCollection {
  @usableFromInline
  internal typealias Index = Int

  @usableFromInline
  internal typealias Element = UInt8

  @usableFromInline
  internal typealias SubSequence = _SmallString

  @inlinable
  internal var startIndex: Int { @inline(__always) get { return 0 } }

  @inlinable
  internal var endIndex: Int { @inline(__always) get { return count } }

  @inlinable
  internal subscript(_ idx: Int) -> UInt8 {
    @inline(__always) get {
      _sanityCheck(idx >= 0 && idx <= 15)
      if idx < 8 {
        return leadingRawBits._uncheckedGetByte(at: idx)
      } else {
        return trailingRawBits._uncheckedGetByte(at: idx &- 8)
      }
    }
    @inline(__always) set {
      _sanityCheck(idx >= 0 && idx <= 15)
      if idx < 8 {
        leadingRawBits._uncheckedSetByte(at: idx, to: newValue)
      } else {
        trailingRawBits._uncheckedSetByte(at: idx &- 8, to: newValue)
      }
    }
  }

  @usableFromInline // testable
  internal subscript(_ bounds: Range<Index>) -> SubSequence {
    @inline(__always) get {
      // TODO(String performance): In-vector-register operation
      return self.withUTF8 { utf8 in
        let rebased = UnsafeBufferPointer(rebasing: utf8[bounds])
        return _SmallString(rebased)._unsafelyUnwrappedUnchecked
      }
    }
  }
}

extension _SmallString {
  @inlinable @inline(__always)
  internal func withUTF8<Result>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> Result
  ) rethrows -> Result {
    var raw = self.zeroTerminatedRawCodeUnits
    return try Swift.withUnsafeBytes(of: &raw) { rawBufPtr in
      let ptr = rawBufPtr.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: UInt8.self)
      return try f(UnsafeBufferPointer(start: ptr, count: self.count))
    }
  }

  // Overwrite stored code units, including uninitialized. `f` should return the
  // new count.
  @inlinable @inline(__always)
  internal mutating func withMutableCapacity(
    _ f: (UnsafeMutableBufferPointer<UInt8>) throws -> Int
  ) rethrows {
    let len = try withUnsafeMutableBytes(of: &self._storage) {
      (rawBufPtr: UnsafeMutableRawBufferPointer) -> Int in
      let ptr = rawBufPtr.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: UInt8.self)
      return try f(UnsafeMutableBufferPointer(
        start: ptr, count: _SmallString.capacity))
    }

    _sanityCheck(len <= _SmallString.capacity)
    discriminator = .small(withCount: len, isASCII: self.computeIsASCII())
  }
}

// Creation
extension _SmallString {
  // Direct from UTF-8
  @inlinable @inline(__always)
  internal init?(_ input: UnsafeBufferPointer<UInt8>) {
    let count = input.count
    guard count <= _SmallString.capacity else { return nil }

    // TODO(SIMD): The below can be replaced with just be a masked unaligned
    // vector load
    let ptr = input.baseAddress._unsafelyUnwrappedUnchecked
    let leading = _bytesToUInt64(ptr, Swift.min(input.count, 8))
    let trailing = count > 8 ? _bytesToUInt64(ptr + 8, count &- 8) : 0

    let isASCII = (leading | trailing) & 0x8080_8080_8080_8080 == 0
    let discriminator = _StringObject.Discriminator.small(
      withCount: count,
      isASCII: isASCII)
    self.init(raw: (leading, trailing | discriminator.rawBits))
  }

  @usableFromInline // @testable
  internal init?(_ base: _SmallString, appending other: _SmallString) {
    let totalCount = base.count + other.count
    guard totalCount <= _SmallString.capacity else { return nil }

    // TODO(SIMD): The below can be replaced with just be a couple vector ops

    var result = base
    var writeIdx = base.count
    for readIdx in 0..<other.count {
      result[writeIdx] = other[readIdx]
      writeIdx &+= 1
    }
    _sanityCheck(writeIdx == totalCount)

    let isASCII = base.isASCII && other.isASCII
    let discriminator = _StringObject.Discriminator.small(
      withCount: totalCount,
      isASCII: isASCII)

    let (leading, trailing) = result.zeroTerminatedRawCodeUnits
    self.init(raw: (leading, trailing | discriminator.rawBits))
  }
}

#if _runtime(_ObjC) && !(arch(i386) || arch(arm))
// Cocoa interop
extension _SmallString {
  // Resiliently create from a tagged cocoa string
  //
  @_effects(readonly) // @opaque
  @usableFromInline // testable
  internal init(taggedCocoa cocoa: AnyObject) {
    self.init()
    self.withMutableCapacity {
      let len = _bridgeTagged(cocoa, intoUTF8: $0)
      _sanityCheck(len != nil && len! < _SmallString.capacity,
        "Internal invariant violated: large tagged NSStrings")
      return len._unsafelyUnwrappedUnchecked
    }
    self._invariantCheck()
  }
}
#endif

extension UInt64 {
  // Fetches the `i`th byte, from least-significant to most-significant
  //
  // TODO: endianess awareness day
  @inlinable @inline(__always)
  internal func _uncheckedGetByte(at i: Int) -> UInt8 {
    _sanityCheck(i >= 0 && i < MemoryLayout<UInt64>.stride)
    let shift = UInt64(truncatingIfNeeded: i) &* 8
    return UInt8(truncatingIfNeeded: (self &>> shift))
  }

  // Sets the `i`th byte, from least-significant to most-significant
  //
  // TODO: endianess awareness day
  @inlinable @inline(__always)
  internal mutating func _uncheckedSetByte(at i: Int, to value: UInt8) {
    _sanityCheck(i >= 0 && i < MemoryLayout<UInt64>.stride)
    let shift = UInt64(truncatingIfNeeded: i) &* 8
    let valueMask: UInt64 = 0xFF &<< shift
    self = (self & ~valueMask) | (UInt64(truncatingIfNeeded: value) &<< shift)
  }
}

@inlinable @inline(__always)
internal func _bytesToUInt64(
  _ input: UnsafePointer<UInt8>,
  _ c: Int
) -> UInt64 {
  // FIXME: This should be unified with _loadPartialUnalignedUInt64LE.
  // Unfortunately that causes regressions in literal concatenation tests. (Some
  // owned to guaranteed specializations don't get inlined.)
  var r: UInt64 = 0
  var shift: Int = 0
  for idx in 0..<c {
    r = r | (UInt64(input[idx]) &<< shift)
    shift = shift &+ 8
  }
  return r
}
