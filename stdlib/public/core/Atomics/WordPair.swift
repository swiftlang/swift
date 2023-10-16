//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
@frozen
public struct WordPair {
#if _pointerBitWidth(_64)
  @usableFromInline
  typealias BuiltinInt = Builtin.Int128
#elseif _pointerBitWidth(_32)
  @usableFromInline
  typealias BuiltinInt = Builtin.Int64
#else
#error("Unsupported platform")
#endif

  public var lowWord: UInt
  public var highWord: UInt

  /// Initialize a new `WordPair` value given its high- and low-order words.
  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(highWord: UInt, lowWord: UInt) {
    self.highWord = highWord
    self.lowWord = lowWord
  }
}

#if (_pointerBitWidth(_32) && _hasAtomicBitWidth(_64)) || (_pointerBitWidth(_64) && _hasAtomicBitWidth(_128))

@available(SwiftStdlib 5.10, *)
extension WordPair: AtomicValue {
#if _pointerBitWidth(_64)
  public typealias AtomicRepresentation = _AtomicStorage128
#elseif _pointerBitWidth(_32)
  public typealias AtomicRepresentation = _AtomicStorage64
#else
#error("Unsupported platform")
#endif

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming WordPair
  ) -> AtomicRepresentation {
#if _pointerBitWidth(_64)
    var i128 = Builtin.zext_Int64_Int128(value.lowWord._value)
    var high128 = Builtin.zext_Int64_Int128(value.highWord._value)
    let highShift = Builtin.zext_Int64_Int128(UInt(64)._value)
    high128 = Builtin.shl_Int128(high128, highShift)
    i128 = Builtin.or_Int128(i128, high128)

    return _AtomicStorage128(i128)
#elseif _pointerBitWidth(_32)
    var i64 = Builtin.zext_Int32_Int64(value.lowWord._value)
    var high64 = Builtin.zext_Int32_Int64(value.highWord._value)
    let highShift = Builtin.zext_Int32_Int64(UInt(32)._value)
    high64 = Builtin.shl_Int64(high64, highShift)
    i64 = Builtin.or_Int64(i64, high64)

    return _AtomicStorage64(i64)
#else
#error("Unsupported platform")
#endif
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> WordPair {
#if _pointerBitWidth(_64)
    let highShift = Builtin.zext_Int64_Int128(UInt(64)._value)
    let high128 = Builtin.lshr_Int128(representation.storage, highShift)
    let high = Builtin.trunc_Int128_Int64(high128)
    let low = Builtin.trunc_Int128_Int64(representation.storage)
#elseif _pointerBitWidth(_32)
    let highShift = Builtin.zext_Int32_Int64(UInt(32)._value)
    let high64 = Builtin.lshr_Int64(representation.storage, highShift)
    let high = Builtin.trunc_Int64_Int32(high64)
    let low = Builtin.trunc_Int64_Int32(representation.storage)
#else
#error("Unsupported platform")
#endif

    return WordPair(highWord: UInt(high), lowWord: UInt(low))
  }
}

#endif

//===----------------------------------------------------------------------===//
// Duration AtomicValue conformance
//===----------------------------------------------------------------------===//

#if _pointerBitWidth(_64) && _hasAtomicBitWidth(_128)

@available(SwiftStdlib 5.10, *)
extension Duration: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = WordPair.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming Duration
  ) -> AtomicRepresentation {
    WordPair.encodeAtomicRepresentation(
      WordPair(
        highWord: UInt(truncatingIfNeeded: value._high),
        lowWord: UInt(truncatingIfNeeded: value._low)
      )
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> Duration {
    let wp = WordPair.decodeAtomicRepresentation(representation)

    return Duration(
      _high: Int64(truncatingIfNeeded: wp.highWord),
      low: UInt64(truncatingIfNeeded: wp.lowWord)
    )
  }
}

#endif
