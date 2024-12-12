//===--- StableHasher.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@inline(__always)
private func _loadPartialUnalignedUInt64LE(
  _ p: UnsafeRawPointer,
  byteCount: Int
) -> UInt64 {
  var result: UInt64 = 0
  switch byteCount {
  case 7:
    result |= UInt64(p.load(fromByteOffset: 6, as: UInt8.self)) &<< 48
    fallthrough
  case 6:
    result |= UInt64(p.load(fromByteOffset: 5, as: UInt8.self)) &<< 40
    fallthrough
  case 5:
    result |= UInt64(p.load(fromByteOffset: 4, as: UInt8.self)) &<< 32
    fallthrough
  case 4:
    result |= UInt64(p.load(fromByteOffset: 3, as: UInt8.self)) &<< 24
    fallthrough
  case 3:
    result |= UInt64(p.load(fromByteOffset: 2, as: UInt8.self)) &<< 16
    fallthrough
  case 2:
    result |= UInt64(p.load(fromByteOffset: 1, as: UInt8.self)) &<< 8
    fallthrough
  case 1:
    result |= UInt64(p.load(fromByteOffset: 0, as: UInt8.self))
    fallthrough
  case 0:
    return result
  default:
    preconditionFailure()
  }
}

/// SipHash-1-3 128bit hasher.
extension StableHasher {
  /// This is a buffer for segmenting arbitrary data into 8-byte chunks.  Buffer
  /// storage is represented by a single 64-bit value in the format used by the
  /// finalization step of SipHash. (The least significant 56 bits hold the
  /// trailing bytes, while the most significant 8 bits hold the count of bytes
  /// appended so far, modulo 256. The count of bytes currently stored in the
  /// buffer is in the lower three bits of the byte count.)
  struct _TailBuffer {
    // msb                                                             lsb
    // +---------+-------+-------+-------+-------+-------+-------+-------+
    // |byteCount|                 tail (<= 56 bits)                     |
    // +---------+-------+-------+-------+-------+-------+-------+-------+
    var value: UInt64

    @inline(__always)
    init() {
      self.value = 0
    }

    var tail: UInt64 {
      @inline(__always)
      get { return value & ~(0xFF &<< 56) }
    }

    var byteCount: UInt64 {
      @inline(__always)
      get { return value &>> 56 }
    }

    @inline(__always)
    mutating func append(_ bytes: UInt64) -> UInt64 {
      let c = byteCount & 7
      if c == 0 {
        value = value &+ (8 &<< 56)
        return bytes
      }
      let shift = c &<< 3
      let chunk = tail | (bytes &<< shift)
      value = (((value &>> 56) &+ 8) &<< 56) | (bytes &>> (64 - shift))
      return chunk
    }

    @inline(__always)
    internal
    mutating func append(_ bytes: UInt64, count: UInt64) -> UInt64? {
      precondition(count >= 0 && count < 8)
      precondition(bytes & ~((1 &<< (count &<< 3)) &- 1) == 0)
      let c = byteCount & 7
      let shift = c &<< 3
      if c + count < 8 {
        value = (value | (bytes &<< shift)) &+ (count &<< 56)
        return nil
      }
      let chunk = tail | (bytes &<< shift)
      value = ((value &>> 56) &+ count) &<< 56
      if c + count > 8 {
        value |= bytes &>> (64 - shift)
      }
      return chunk
    }
  }
}

struct StableHasher {
  private var _buffer: _TailBuffer
  private var _state: _State

  @inline(__always)
  init(state: _State) {
    self._buffer = _TailBuffer()
    self._state = state
  }

  @inline(__always)
  init() {
    self.init(state: _State(seed: 0))
  }

  @inline(__always)
  init(seed: Int) {
    self.init(state: _State(seed: seed))
  }

  @inline(__always)
  mutating func combine(_ value: UInt) {
#if arch(i386) || arch(arm) || arch(arm64_32) || arch(wasm32) // FIXME: Adopt _pointerBitWidth(_:).
    combine(UInt32(truncatingIfNeeded: value))
#else
    combine(UInt64(truncatingIfNeeded: value))
#endif
  }

  @inline(__always)
  mutating func combine(_ value: UInt64) {
    _state.compress(_buffer.append(value))
  }

  @inline(__always)
  mutating func combine(_ value: UInt32) {
    let value = UInt64(truncatingIfNeeded: value)
    if let chunk = _buffer.append(value, count: 4) {
      _state.compress(chunk)
    }
  }

  @inline(__always)
  mutating func combine(_ value: UInt16) {
    let value = UInt64(truncatingIfNeeded: value)
    if let chunk = _buffer.append(value, count: 2) {
      _state.compress(chunk)
    }
  }

  @inline(__always)
  mutating func combine(_ value: UInt8) {
    let value = UInt64(truncatingIfNeeded: value)
    if let chunk = _buffer.append(value, count: 1) {
      _state.compress(chunk)
    }
  }

  @inline(__always)
  mutating func combine(bytes: UInt64, count: Int) {
    precondition(count >= 0 && count < 8)
    let count = UInt64(truncatingIfNeeded: count)
    if let chunk = _buffer.append(bytes, count: count) {
      _state.compress(chunk)
    }
  }

  @inline(__always)
  mutating func combine(bytes: UnsafeRawBufferPointer) {
    var remaining = bytes.count
    guard remaining > 0 else { return }
    var data = bytes.baseAddress!

    // Load first unaligned partial word of data
    do {
      let start = UInt(bitPattern: data)
      let end = (start &+ 7) & ~7 // roundUp(start, toAlignment: MemoryLayout<UInt64>.alignment)
      let c = min(remaining, Int(end &- start))
      if c > 0 {
        let chunk = _loadPartialUnalignedUInt64LE(data, byteCount: c)
        combine(bytes: chunk, count: c)
        data += c
        remaining &-= c
      }
    }
    precondition(
      remaining == 0 ||
      Int(bitPattern: data) & (MemoryLayout<UInt64>.alignment - 1) == 0)

    // Load as many aligned words as there are in the input buffer
    while remaining >= MemoryLayout<UInt64>.size {
      combine(UInt64(littleEndian: data.load(as: UInt64.self)))
      data += MemoryLayout<UInt64>.size
      remaining &-= MemoryLayout<UInt64>.size
    }

    // Load last partial word of data
    precondition(remaining >= 0 && remaining < 8)
    if remaining > 0 {
      let chunk = _loadPartialUnalignedUInt64LE(data, byteCount: remaining)
      combine(bytes: chunk, count: remaining)
    }
  }

  @inline(__always)
  mutating func finalize() -> (UInt64, UInt64) {
    return _state.finalize(tailAndByteCount: _buffer.value)
  }
}

extension StableHasher {
  struct _State {
    // "somepseudorandomlygeneratedbytes"
    private var v0: UInt64 = 0x736f6d6570736575
    private var v1: UInt64 = 0x646f72616e646f6d
    private var v2: UInt64 = 0x6c7967656e657261
    private var v3: UInt64 = 0x7465646279746573

    @inline(__always)
    init(rawSeed: (UInt64, UInt64)) {
      v3 ^= rawSeed.1
      v2 ^= rawSeed.0
      v1 ^= rawSeed.1
      v0 ^= rawSeed.0

      v1 ^= 0xee;
    }
  }
}

extension StableHasher._State {
  @inline(__always)
  init(seed: Int) {
    self.init(rawSeed: (UInt64(truncatingIfNeeded: seed), 0))
  }
}

extension StableHasher._State {
  @inline(__always)
  private static func _rotateLeft(_ x: UInt64, by amount: UInt64) -> UInt64 {
    return (x &<< amount) | (x &>> (64 - amount))
  }

  @inline(__always)
  private mutating func _round() {
    v0 = v0 &+ v1
    v1 = Self._rotateLeft(v1, by: 13)
    v1 ^= v0
    v0 = Self._rotateLeft(v0, by: 32)
    v2 = v2 &+ v3
    v3 = Self._rotateLeft(v3, by: 16)
    v3 ^= v2
    v0 = v0 &+ v3
    v3 = Self._rotateLeft(v3, by: 21)
    v3 ^= v0
    v2 = v2 &+ v1
    v1 = Self._rotateLeft(v1, by: 17)
    v1 ^= v2
    v2 = Self._rotateLeft(v2, by: 32)
  }

  @inline(__always)
  private func _extract() -> UInt64 {
    return v0 ^ v1 ^ v2 ^ v3
  }
}

extension StableHasher._State {
  @inline(__always)
  mutating func compress(_ m: UInt64) {
    v3 ^= m
    _round()
    v0 ^= m
  }

  @inline(__always)
  mutating func finalize(tailAndByteCount: UInt64) -> (UInt64, UInt64) {
    compress(tailAndByteCount)
    v2 ^= 0xee
    for _ in 0..<3 {
      _round()
    }
    let h1 = _extract()

    v1 ^= 0xdd
    for _ in 0..<3 {
      _round()
    }
    let h2 = _extract()

    return (h1, h2)
  }
}
