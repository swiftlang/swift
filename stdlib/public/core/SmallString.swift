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

//
// NOTE: This is a prototype, it does not have e.g. 32-bit support yet.
//

@_fixed_layout @usableFromInline
internal struct _SmallString {
  @usableFromInline
  internal typealias RawBitPattern = _StringObject.RawBitPattern

  // Small strings are values; store them raw
  @usableFromInline
  internal var _storage: RawBitPattern

  @inlinable
  internal var rawBits: RawBitPattern {
    @inline(__always) get { return _storage }
  }

  @inlinable
  internal var leadingRawBits: UInt {
    @inline(__always) get { return _storage.0 }
    @inline(__always) set { _storage.0 = newValue }
  }

  @inlinable
  internal var trailingRawBits: UInt {
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
  internal static var capacity: Int { @inline(__always) get { return 15 } }

  @inlinable
  internal var capacity: Int { @inline(__always) get { return 15 } }

  @inlinable
  internal var count: Int {
    @inline(__always) get {
      return _StringObject(rawUncheckedValue: self.rawBits).smallCount
    }
  }

  @inlinable
  internal var unusedCapacity: Int {
    @inline(__always) get { return capacity &- count }
  }

  @inlinable
  internal var isASCII: Bool {
    @inline(__always) get {
      return _StringObject(rawUncheckedValue: self.rawBits).smallIsASCII
    }
  }

  // Give raw, nul-terminated code units. This is only for limited internal
  // usage: it always clears the discriminator and count (in case it's full)
  @inlinable
  internal var zeroTerminatedRawCodeUnits: RawBitPattern {
    @inline(__always) get {
      return (
        self._storage.0,
        _StringObject(
          rawUncheckedValue: self.rawBits
        ).undiscriminatedObjectRawBits
      )
    }
  }

  @inlinable
  internal func computeIsASCII() -> Bool {
    // TODO(UTF8 codegen): Either mask off discrim before, or don't set bit
    // after

#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
    let asciiMask: UInt = 0x8080_8080_8080_8080
    let raw = zeroTerminatedRawCodeUnits
    return raw.0 & asciiMask == 0 && raw.1 & asciiMask == 0
#endif
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
        self.map { String($0, radix: 16) }.dropLast().joined()
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
      // TODO(UTF8 perf): In-register; just a couple shifts...
      return self.withUTF8 { utf8 in
        _SmallString(utf8[bounds]._rebased)._unsafelyUnwrappedUnchecked
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
    var obj = _StringObject(rawUncheckedValue: self.rawBits)
    obj.setSmallCount(len, isASCII: self.computeIsASCII())
    self = _SmallString(obj)
  }

  // Write to excess capacity. `f` should return the new count.
  @inlinable @inline(__always)
  internal mutating func withMutableExcessCapacity(
    _ f: (UnsafeMutableBufferPointer<UInt8>) throws -> Int
  ) rethrows {
    let currentCount = self.count

    try self.withMutableCapacity { fullBufPtr in
      let rebased = UnsafeMutableBufferPointer(rebasing:
        fullBufPtr[currentCount...])
      let delta = try f(rebased)
      return currentCount + delta
    }
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

    let low = _bytesToUInt(ptr, Swift.min(input.count, 8))
    let high = count > 8 ? _bytesToUInt(ptr + 8, count &- 8) : 0

    let isASCII = (low | high) & 0x8080_8080_8080_8080 == 0
    let smallDiscriminator = _StringObject.Nibbles.small(
      withCount: count, isASCII: isASCII)

    self.init(_StringObject(
      discriminator: smallDiscriminator,
      valueLeading: low,
      valueTrailing: high))
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
    let smallDiscriminator = _StringObject.Nibbles.small(
      withCount: totalCount, isASCII: isASCII)

    let (leading, trailing) = result.zeroTerminatedRawCodeUnits
    self.init(_StringObject(
      discriminator: smallDiscriminator,
      valueLeading: leading,
      valueTrailing: trailing))
  }
}

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

extension UInt {
  // Fetches the `i`th byte, from least-significant to most-significant
  //
  // TODO: endianess awareness day
  @inlinable @inline(__always)
  internal func _uncheckedGetByte(at i: Int) -> UInt8 {
    _sanityCheck(i >= 0 && i < MemoryLayout<UInt>.stride)
    let shift = UInt(bitPattern: i) &* 8
    return UInt8(truncatingIfNeeded: (self &>> shift))
  }

  // Sets the `i`th byte, from least-significant to most-significant
  //
  // TODO: endianess awareness day
  @inlinable @inline(__always)
  internal mutating func _uncheckedSetByte(at i: Int, to value: UInt8) {
    _sanityCheck(i >= 0 && i < MemoryLayout<UInt>.stride)
    let shift = UInt(bitPattern: i) &* 8
    let valueMask = 0xFF &<< shift
    self = (self & ~valueMask) | (UInt(truncatingIfNeeded: value) &<< shift)
  }
}

@inlinable @inline(__always)
internal func _bytesToUInt(_ input: UnsafePointer<UInt8>, _ c: Int) -> UInt {
  var r: UInt = 0
  var shift: Int = 0
  for idx in 0..<c {
    r = r | (UInt(input[idx]) &<< shift)
    shift = shift &+ 8
  }
  return r
}


