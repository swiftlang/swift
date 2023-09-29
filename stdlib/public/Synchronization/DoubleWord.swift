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

import Builtin

@available(SwiftStdlib 5.10, *)
@frozen
public struct DoubleWord {
#if _pointerBitWidth(_64)
  @usableFromInline
  typealias BuiltinInt = Builtin.Int128
#elseif _pointerBitWidth(_32)
  @usableFromInline
  typealias BuiltinInt = Builtin.Int64
#else
#error("Unsupported platform")
#endif

  @usableFromInline
  var value: BuiltinInt

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  var _highWordShift: BuiltinInt {
#if _pointerBitWidth(_64)
    Builtin.zext_Int64_Int128(UInt(64)._value)
#elseif _pointerBitWidth(_32)
    Builtin.zext_Int32_Int64(UInt(32)._value)
#else
#error("Unsupported platform")
#endif
  }

  /// The most significant word in `self`, considering it as a single, wide
  // integer value.
  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var highWord: UInt {
    get {
#if _pointerBitWidth(_64)
      let shifted = Builtin.lshr_Int128(value, _highWordShift)
      return UInt(Builtin.trunc_Int128_Int64(shifted))
#elseif _pointerBitWidth(_32)
      let shifted = Builtin.lshr_Int64(value, _highWordShift)
      return UInt(Builtin.trunc_Int64_Int32(shifted))
#else
#error("Unsupported platform")
#endif
    }

    set {
#if _pointerBitWidth(_64)
      let newValueExtended = Builtin.zext_Int64_Int128(newValue._value)
      let newValueShifted = Builtin.shl_Int128(newValueExtended, _highWordShift)
      let intermediateShift = Builtin.shl_Int128(value, _highWordShift)
      let cleared = Builtin.lshr_Int128(intermediateShift, _highWordShift)
      value = Builtin.and_Int128(cleared, newValueShifted)
#elseif _pointerBitWidth(_32)
      let newValueExtended = Builtin.zext_Int32_Int64(newValue._value)
      let newValueShifted = Builtin.shl_Int64(newValueExtended, _highWordShift)
      let intermediateShift = Builtin.shl_Int64(value, _highWordShift)
      let cleared = Builtin.lshr_Int64(intermediateShift, _highWordShift)
      value = Builtin.and_Int64(cleared, newValueShifted)
#else
#error("Unsupported platform")
#endif
    }
  }

  /// The least significant word in `self`, considering it as a single, wide
  /// integer value.
  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var lowWord: UInt {
    get {
#if _pointerBitWidth(_64)
      UInt(Builtin.trunc_Int128_Int64(value))
#elseif _pointerBitWidth(_32)
      UInt(Builtin.trunc_Int64_Int32(value))
#else
#error("Unsupported platform")
#endif
    }

    set {
#if _pointerBitWidth(_64)
      let newValueExtended = Builtin.zext_Int64_Int128(newValue._value)
      let intermediateShift = Builtin.lshr_Int128(value, _highWordShift)
      let cleared = Builtin.shl_Int128(intermediateShift, _highWordShift)
      value = Builtin.and_Int128(cleared, newValueExtended)
#elseif _pointerBitWidth(_32)
      let newValueExtended = Builtin.zext_Int32_Int64(newValue._value)
      let intermediateShift = Builtin.lshr_Int64(value, _highWordShift)
      let cleared = Builtin.shl_Int64(intermediateShift, _highWordShift)
      value = Builtin.and_Int64(cleared, newValueExtended)
#else
#error("Unsupported platform")
#endif
    }
  }

  /// Initialize a new `DoubleWord` value given its high- and low-order words.
  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(highWord: UInt, lowWord: UInt) {
    // Appease the compiler by default initializing our stored property to 0.
#if _pointerBitWidth(_64)
    self.value = Builtin.zext_Int64_Int128(UInt(0)._value)
#elseif _pointerBitWidth(_32)
    self.value = Builtin.zext_Int32_Int64(UInt(0)._value)
#else
#error("Unsupported platform")
#endif

    self.highWord = highWord
    self.lowWord = lowWord
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  init(_ builtin: BuiltinInt) {
    self.value = builtin
  }
}

// FIXME: Conditionalize this conformance for platforms that do no support
// double word atomics.

@available(SwiftStdlib 5.10, *)
extension DoubleWord: AtomicValue {
#if _pointerBitWidth(_64)
  public typealias AtomicRepresentation = AtomicInt128Storage
#elseif _pointerBitWidth(_32)
  public typealias AtomicRepresentation = AtomicInt64Storage
#else
#error("Unsupported platform")
#endif

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: consuming DoubleWord
  ) -> AtomicInt128Storage {
    AtomicInt128Storage(value.value)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicInt128Storage
  ) -> DoubleWord {
    DoubleWord(representation.storage)
  }
}
