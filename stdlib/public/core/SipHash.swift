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
/// This file implements SipHash-2-4 and SipHash-1-3
/// (https://131002.net/siphash/).
///
/// This file is based on the reference C implementation, which was released
/// to public domain by:
///
/// * Jean-Philippe Aumasson <jeanphilippe.aumasson@gmail.com>
/// * Daniel J. Bernstein <djb@cr.yp.to>
//===----------------------------------------------------------------------===//

// FIXME: Remove @usableFromInline and @_fixed_layout once Hasher is resilient.
// rdar://problem/38549901
@usableFromInline @_fixed_layout
internal struct _SipHashState {
  // "somepseudorandomlygeneratedbytes"
  fileprivate var v0: UInt64 = 0x736f6d6570736575
  fileprivate var v1: UInt64 = 0x646f72616e646f6d
  fileprivate var v2: UInt64 = 0x6c7967656e657261
  fileprivate var v3: UInt64 = 0x7465646279746573

  @inline(__always)
  fileprivate init(rawSeed: (UInt64, UInt64)) {
    v3 ^= rawSeed.1
    v2 ^= rawSeed.0
    v1 ^= rawSeed.1
    v0 ^= rawSeed.0
  }

  @inline(__always)
  fileprivate
  static func _rotateLeft(_ x: UInt64, by amount: UInt64) -> UInt64 {
    return (x &<< amount) | (x &>> (64 - amount))
  }

  @inline(__always)
  fileprivate mutating func _round() {
    v0 = v0 &+ v1
    v1 = _SipHashState._rotateLeft(v1, by: 13)
    v1 ^= v0
    v0 = _SipHashState._rotateLeft(v0, by: 32)
    v2 = v2 &+ v3
    v3 = _SipHashState._rotateLeft(v3, by: 16)
    v3 ^= v2
    v0 = v0 &+ v3
    v3 = _SipHashState._rotateLeft(v3, by: 21)
    v3 ^= v0
    v2 = v2 &+ v1
    v1 = _SipHashState._rotateLeft(v1, by: 17)
    v1 ^= v2
    v2 = _SipHashState._rotateLeft(v2, by: 32)
  }

  @inline(__always)
  fileprivate func _extract() -> UInt64 {
    return v0 ^ v1 ^ v2 ^ v3
  }
}

// FIXME: Remove @usableFromInline and @_fixed_layout once Hasher is resilient.
// rdar://problem/38549901
@usableFromInline @_fixed_layout
internal struct _SipHash13Core: _HasherCore {
  private var _state: _SipHashState

  @inline(__always)
  internal init(rawSeed: (UInt64, UInt64)) {
    _state = _SipHashState(rawSeed: rawSeed)
  }

  @inline(__always)
  internal mutating func compress(_ m: UInt64) {
    _state.v3 ^= m
    _state._round()
    _state.v0 ^= m
  }

  @inline(__always)
  internal mutating func finalize(tailAndByteCount: UInt64) -> UInt64 {
    compress(tailAndByteCount)
    _state.v2 ^= 0xff
    for _ in 0..<3 {
      _state._round()
    }
    return _state._extract()
  }
}

internal struct _SipHash24Core: _HasherCore {
  private var _state: _SipHashState

  @inline(__always)
  internal init(rawSeed: (UInt64, UInt64)) {
    _state = _SipHashState(rawSeed: rawSeed)
  }

  @inline(__always)
  internal mutating func compress(_ m: UInt64) {
    _state.v3 ^= m
    _state._round()
    _state._round()
    _state.v0 ^= m
  }

  @inline(__always)
  internal mutating func finalize(tailAndByteCount: UInt64) -> UInt64 {
    compress(tailAndByteCount)

    _state.v2 ^= 0xff
    for _ in 0..<4 {
      _state._round()
    }
    return _state._extract()
  }
}

// FIXME: This type only exists to facilitate testing, and should not exist in
// production builds.
@usableFromInline // @testable
internal struct _SipHash13 {
  internal typealias Core = _SipHash13Core

  internal var _core: _BufferingHasher<Core>

  @usableFromInline // @testable
  internal init(_rawSeed: (UInt64, UInt64)) {
    _core = _BufferingHasher(core: Core(rawSeed: _rawSeed))
  }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt64) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt32) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt16) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt8) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(bytes v: UInt64, count: Int) {
    _core.combine(bytes: v, count: count)
  }
  @usableFromInline // @testable
  internal mutating func combine(bytes: UnsafeRawBufferPointer) {
    _core.combine(bytes: bytes)
  }
  @usableFromInline // @testable
  internal __consuming func finalize() -> UInt64 {
    var core = _core
    return core.finalize()
  }
}

// FIXME: This type only exists to facilitate testing, and should not exist in
// production builds.
@usableFromInline // @testable
internal struct _SipHash24 {
  internal typealias Core = _SipHash24Core

  internal var _core: _BufferingHasher<Core>

  @usableFromInline // @testable
  internal init(_rawSeed: (UInt64, UInt64)) {
    _core = _BufferingHasher(core: Core(rawSeed: _rawSeed))
  }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt64) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt32) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt16) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(_ v: UInt8) { _core.combine(v) }
  @usableFromInline // @testable
  internal mutating func _combine(bytes v: UInt64, count: Int) {
    _core.combine(bytes: v, count: count)
  }
  @usableFromInline // @testable
  internal mutating func combine(bytes: UnsafeRawBufferPointer) {
    _core.combine(bytes: bytes)
  }
  @usableFromInline // @testable
  internal __consuming func finalize() -> UInt64 {
    var core = _core
    return core.finalize()
  }
}
