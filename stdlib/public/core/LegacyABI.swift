//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file contains non-API (or underscored) declarations that are needed to
// be kept around for ABI compatibility

extension Unicode.UTF16 {
  @available(*, unavailable, renamed: "Unicode.UTF16.isASCII")
  @inlinable
  public static func _isASCII(_ x: CodeUnit) -> Bool  {
    return Unicode.UTF16.isASCII(x)
  }
}

@available(*, unavailable, renamed: "Unicode.UTF8.isASCII")
@inlinable
internal func _isASCII(_ x: UInt8) -> Bool {
  return Unicode.UTF8.isASCII(x)
}

@available(*, unavailable, renamed: "Unicode.UTF8.isContinuation")
@inlinable
internal func _isContinuation(_ x: UInt8) -> Bool {
  return UTF8.isContinuation(x)
}

extension Substring {
@available(*, unavailable, renamed: "Substring.base")
  @inlinable
  internal var _wholeString: String { return base }
}

extension String {
  @available(*, unavailable, renamed: "String.withUTF8")
  @inlinable
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8(body)
  }
}

extension Substring {
  @available(*, unavailable, renamed: "Substring.withUTF8")
  @inlinable
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8(body)
  }
}

// This function is no longer used but must be kept for ABI compatibility
// because references to it may have been inlined.
@usableFromInline
internal func _branchHint(_ actual: Bool, expected: Bool) -> Bool {
  // The LLVM intrinsic underlying int_expect_Int1 now requires an immediate
  // argument for the expected value so we cannot call it here. This should
  // never be called in cases where performance matters, so just return the
  // value without any branch hint.
  return actual
}

extension String {
  @usableFromInline // Never actually used in inlinable code...
  internal func _nativeCopyUTF16CodeUnits(
    into buffer: UnsafeMutableBufferPointer<UInt16>,
    range: Range<String.Index>
  ) { fatalError() }
}

extension String.UTF16View {
  // Swift 5.x: This was accidentally shipped as inlinable, but was never used
  // from an inlinable context. The definition is kept around for technical ABI
  // compatibility (even though it shouldn't matter), but is unused.
  @inlinable @inline(__always)
  internal var _shortHeuristic: Int { return 32 }
}

@usableFromInline
internal func unimplemented_utf8_32bit(
  _ message: String = "",
  file: StaticString = #file, line: UInt = #line
) -> Never {
  fatalError("32-bit: Unimplemented for UTF-8 support", file: file, line: line)
}

@usableFromInline
internal func _unsafePlus(_ lhs: Int, _ rhs: Int) -> Int {
#if INTERNAL_CHECKS_ENABLED
  return lhs + rhs
#else
  return lhs &+ rhs
#endif
}

@usableFromInline
internal func _unsafeMinus(_ lhs: Int, _ rhs: Int) -> Int {
#if INTERNAL_CHECKS_ENABLED
  return lhs - rhs
#else
  return lhs &- rhs
#endif
}

extension Dictionary {
  @usableFromInline
  @_silgen_name("$sSD9mapValuesySDyxqd__Gqd__q_KXEKlF")
  internal func __abi_mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> Dictionary<Key, T> {
    return try Dictionary<Key, T>(_native: _variant.mapValues(transform))
  }

  @usableFromInline
  @_silgen_name("$sSD16compactMapValuesySDyxqd__Gqd__Sgq_KXEKlF")
  internal func __abi_compactMapValues<T>(
    _ transform: (Value) throws -> T?
  ) rethrows -> Dictionary<Key, T> {
    let result: _NativeDictionary<Key, T> =
      try self.reduce(into: _NativeDictionary<Key, T>()) { (result, element) in
      if let value = try transform(element.value) {
        result.insertNew(key: element.key, value: value)
      }
    }
    return Dictionary<Key, T>(_native: result)
  }
}

extension Dictionary._Variant {
  @usableFromInline
  @_silgen_name("$sSD8_VariantV9mapValuesys17_NativeDictionaryVyxqd__Gqd__q_KXEKlF")
  internal func __abi_mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
#if _runtime(_ObjC)
    guard isNative else {
      return try asCocoa.mapValues(transform)
    }
#endif
    return try asNative.mapValues(transform)
  }
}

#if _runtime(_ObjC)
extension __CocoaDictionary {
  @usableFromInline
  @_silgen_name("$ss17__CocoaDictionaryV9mapValuesys07_NativeB0Vyxq0_Gq0_q_KXEKSHRzr1_lF")
  internal func __abi_mapValues<Key: Hashable, Value, T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    var result = _NativeDictionary<Key, T>(capacity: self.count)
    for (cocoaKey, cocoaValue) in self {
      let key = _forceBridgeFromObjectiveC(cocoaKey, Key.self)
      let value = _forceBridgeFromObjectiveC(cocoaValue, Value.self)
      try result.insertNew(key: key, value: transform(value))
    }
    return result
  }
}
#endif

extension _NativeDictionary {
  @usableFromInline
  @_silgen_name("$ss17_NativeDictionaryV9mapValuesyAByxqd__Gqd__q_KXEKlF")
  internal func __abi_mapValues<T>(
    _ transform: (Value) throws -> T
  ) rethrows -> _NativeDictionary<Key, T> {
    let resultStorage = unsafe _DictionaryStorage<Key, T>.copy(original: _storage)
    unsafe _internalInvariant(resultStorage._seed == _storage._seed)
    let result = unsafe _NativeDictionary<Key, T>(resultStorage)
    // Because the current and new buffer have the same scale and seed, we can
    // initialize to the same locations in the new buffer, skipping hash value
    // recalculations.
    for unsafe bucket in unsafe hashTable {
      let key = unsafe self.uncheckedKey(at: bucket)
      let value = unsafe self.uncheckedValue(at: bucket)
      try result._insert(at: bucket, key: key, value: transform(value))
    }
    return result
  }
}

extension Sequence {
  // ABI-only entrypoint for the rethrows version of map, which has been
  // superseded by the typed-throws version. Expressed as "throws", which is
  // ABI-compatible with "rethrows".
  @_spi(SwiftStdlibLegacyABI) @available(swift, obsoleted: 1)
  @usableFromInline
  @_silgen_name("$sSTsE3mapySayqd__Gqd__7ElementQzKXEKlF")
  func __rethrows_map<T>(
    _ transform: (Element) throws -> T
  ) throws -> [T] {
    try map(transform)
  }

  @usableFromInline
  @_silgen_name("$sSTsE6filterySay7ElementQzGSbACKXEKF")
  internal __consuming func __abi_filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> [Element] {
    return try __abi__filter(isIncluded)
  }

  @usableFromInline
  @_silgen_name("$sSTsE7_filterySay7ElementQzGSbACKXEKF")
  internal func __abi__filter(
    _ isIncluded: (Element) throws -> Bool
  ) rethrows -> [Element] {

    var result = ContiguousArray<Element>()

    var iterator = self.makeIterator()

    while let element = iterator.next() {
      if try isIncluded(element) {
        result.append(element)
      }
    }

    return Array(result)
  }

  @usableFromInline
  @_semantics("sequence.forEach")
  @_silgen_name("$sSTsE7forEachyyy7ElementQzKXEKF")
  internal func __abi_forEach(
    _ body: (Element) throws -> Void
  ) rethrows {
    for element in self {
      try body(element)
    }
  }

  @usableFromInline
  @_silgen_name("$sSTsE6reduceyqd__qd___qd__qd___7ElementQztKXEtKlF")
  internal func __abi_reduce<Result>(
    _ initialResult: Result,
    _ nextPartialResult:
      (_ partialResult: Result, Element) throws -> Result
  ) rethrows -> Result {
    var accumulator = initialResult
    for element in self {
      accumulator = try nextPartialResult(accumulator, element)
    }
    return accumulator
  }

  @usableFromInline
  @_silgen_name("$sSTsE6reduce4into_qd__qd__n_yqd__z_7ElementQztKXEtKlF")
  internal func __abi_reduce<Result>(
    into initialResult: __owned Result,
    _ updateAccumulatingResult:
      (_ partialResult: inout Result, Element) throws -> ()
  ) rethrows -> Result {
    var accumulator = initialResult
    for element in self {
      try updateAccumulatingResult(&accumulator, element)
    }
    return accumulator
  }
}
