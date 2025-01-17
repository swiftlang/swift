//===----------------------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file contains Swift wrappers for functions defined in the C++ runtime.
///
//===----------------------------------------------------------------------===//

import SwiftShims

//===----------------------------------------------------------------------===//
// Atomics
//===----------------------------------------------------------------------===//

@_transparent
public // @testable
func _stdlib_atomicCompareExchangeStrongPtr(
  object target: UnsafeMutablePointer<UnsafeRawPointer?>,
  expected: UnsafeMutablePointer<UnsafeRawPointer?>,
  desired: UnsafeRawPointer?
) -> Bool {
  // We use Builtin.Word here because Builtin.RawPointer can't be nil.
  let (oldValue, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
    target._rawValue,
    UInt(bitPattern: expected.pointee)._builtinWordValue,
    UInt(bitPattern: desired)._builtinWordValue)
  expected.pointee = UnsafeRawPointer(bitPattern: Int(oldValue))
  return Bool(won)
}

/// Atomic compare and exchange of `UnsafeMutablePointer<T>` with sequentially
/// consistent memory ordering.  Precise semantics are defined in C++11 or C11.
///
/// - Warning: This operation is extremely tricky to use correctly because of
///   writeback semantics.
///
/// It is best to use it directly on an
/// `UnsafeMutablePointer<UnsafeMutablePointer<T>>` that is known to point
/// directly to the memory where the value is stored.
///
/// In a call like this:
///
///     _stdlib_atomicCompareExchangeStrongPtr(&foo.property1.property2, ...)
///
/// you need to manually make sure that:
///
/// - all properties in the chain are physical (to make sure that no writeback
///   happens; the compare-and-exchange instruction should operate on the
///   shared memory); and
///
/// - the shared memory that you are accessing is located inside a heap
///   allocation (a class instance property, a `_BridgingBuffer`, a pointer to
///   an `Array` element etc.)
///
/// If the conditions above are not met, the code will still compile, but the
/// compare-and-exchange instruction will operate on the writeback buffer, and
/// you will get a *race* while doing writeback into shared memory.
@_transparent
public // @testable
func _stdlib_atomicCompareExchangeStrongPtr<T>(
  object target: UnsafeMutablePointer<UnsafeMutablePointer<T>>,
  expected: UnsafeMutablePointer<UnsafeMutablePointer<T>>,
  desired: UnsafeMutablePointer<T>
) -> Bool {
  let rawTarget = UnsafeMutableRawPointer(target).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  let rawExpected = UnsafeMutableRawPointer(expected).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  return _stdlib_atomicCompareExchangeStrongPtr(
    object: rawTarget,
    expected: rawExpected,
    desired: UnsafeRawPointer(desired))
}

/// Atomic compare and exchange of `UnsafeMutablePointer<T>` with sequentially
/// consistent memory ordering.  Precise semantics are defined in C++11 or C11.
///
/// - Warning: This operation is extremely tricky to use correctly because of
///   writeback semantics.
///
/// It is best to use it directly on an
/// `UnsafeMutablePointer<UnsafeMutablePointer<T>>` that is known to point
/// directly to the memory where the value is stored.
///
/// In a call like this:
///
///     _stdlib_atomicCompareExchangeStrongPtr(&foo.property1.property2, ...)
///
/// you need to manually make sure that:
///
/// - all properties in the chain are physical (to make sure that no writeback
///   happens; the compare-and-exchange instruction should operate on the
///   shared memory); and
///
/// - the shared memory that you are accessing is located inside a heap
///   allocation (a class instance property, a `_BridgingBuffer`, a pointer to
///   an `Array` element etc.)
///
/// If the conditions above are not met, the code will still compile, but the
/// compare-and-exchange instruction will operate on the writeback buffer, and
/// you will get a *race* while doing writeback into shared memory.
@_transparent
public // @testable
func _stdlib_atomicCompareExchangeStrongPtr<T>(
  object target: UnsafeMutablePointer<UnsafeMutablePointer<T>?>,
  expected: UnsafeMutablePointer<UnsafeMutablePointer<T>?>,
  desired: UnsafeMutablePointer<T>?
) -> Bool {
  let rawTarget = UnsafeMutableRawPointer(target).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  let rawExpected = UnsafeMutableRawPointer(expected).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  return _stdlib_atomicCompareExchangeStrongPtr(
    object: rawTarget,
    expected: rawExpected,
    desired: UnsafeRawPointer(desired))
}

@_transparent
@discardableResult
@_unavailableInEmbedded
public // @testable
func _stdlib_atomicInitializeARCRef(
  object target: UnsafeMutablePointer<AnyObject?>,
  desired: AnyObject
) -> Bool {
  // Note: this assumes that AnyObject? is layout-compatible with a RawPointer
  // that simply points to the same memory.
  var expected: UnsafeRawPointer? = nil
  let unmanaged = Unmanaged.passRetained(desired)
  let desiredPtr = unmanaged.toOpaque()
  let rawTarget = UnsafeMutableRawPointer(target).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  let wonRace = withUnsafeMutablePointer(to: &expected) {
    _stdlib_atomicCompareExchangeStrongPtr(
      object: rawTarget, expected: $0, desired: desiredPtr
    )
  }
  if !wonRace {
    // Some other thread initialized the value.  Balance the retain that we
    // performed on 'desired'.
    unmanaged.release()
  }
  return wonRace
}

@_transparent
@_unavailableInEmbedded
public // @testable
func _stdlib_atomicLoadARCRef(
  object target: UnsafeMutablePointer<AnyObject?>
) -> AnyObject? {
  let value = Builtin.atomicload_seqcst_Word(target._rawValue)
  if let unwrapped = UnsafeRawPointer(bitPattern: Int(value)) {
    return Unmanaged<AnyObject>.fromOpaque(unwrapped).takeUnretainedValue()
  }
  return nil
}

@_transparent
@_alwaysEmitIntoClient
@discardableResult
public func _stdlib_atomicAcquiringInitializeARCRef<T: AnyObject>(
  object target: UnsafeMutablePointer<T?>,
  desired: __owned T
) -> Unmanaged<T> {
  // Note: this assumes that AnyObject? is layout-compatible with a RawPointer
  // that simply points to the same memory, and that `nil` is represented by an
  // all-zero bit pattern.
  let unmanaged = Unmanaged.passRetained(desired)
  let desiredPtr = unmanaged.toOpaque()

  let (value, won) = Builtin.cmpxchg_acqrel_acquire_Word(
    target._rawValue,
    0._builtinWordValue,
    Builtin.ptrtoint_Word(desiredPtr._rawValue))

  if Bool(won) { return unmanaged }

  // Some other thread initialized the value before us. Balance the retain that
  // we performed on 'desired', and return what we loaded.
  unmanaged.release()
  let ptr = UnsafeRawPointer(Builtin.inttoptr_Word(value))
  return Unmanaged<T>.fromOpaque(ptr)
}

@_alwaysEmitIntoClient
@_transparent
public func _stdlib_atomicAcquiringLoadARCRef<T: AnyObject>(
  object target: UnsafeMutablePointer<T?>
) -> Unmanaged<T>? {
  let value = Builtin.atomicload_acquire_Word(target._rawValue)
  if Int(value) == 0 { return nil }
  let opaque = UnsafeRawPointer(Builtin.inttoptr_Word(value))
  return Unmanaged<T>.fromOpaque(opaque)
}

//===----------------------------------------------------------------------===//
// Conversion of primitive types to `String`
//===----------------------------------------------------------------------===//

/// A 32 byte buffer.
internal struct _Buffer32 {
  internal var _x0: UInt8 = 0
  internal var _x1: UInt8 = 0
  internal var _x2: UInt8 = 0
  internal var _x3: UInt8 = 0
  internal var _x4: UInt8 = 0
  internal var _x5: UInt8 = 0
  internal var _x6: UInt8 = 0
  internal var _x7: UInt8 = 0
  internal var _x8: UInt8 = 0
  internal var _x9: UInt8 = 0
  internal var _x10: UInt8 = 0
  internal var _x11: UInt8 = 0
  internal var _x12: UInt8 = 0
  internal var _x13: UInt8 = 0
  internal var _x14: UInt8 = 0
  internal var _x15: UInt8 = 0
  internal var _x16: UInt8 = 0
  internal var _x17: UInt8 = 0
  internal var _x18: UInt8 = 0
  internal var _x19: UInt8 = 0
  internal var _x20: UInt8 = 0
  internal var _x21: UInt8 = 0
  internal var _x22: UInt8 = 0
  internal var _x23: UInt8 = 0
  internal var _x24: UInt8 = 0
  internal var _x25: UInt8 = 0
  internal var _x26: UInt8 = 0
  internal var _x27: UInt8 = 0
  internal var _x28: UInt8 = 0
  internal var _x29: UInt8 = 0
  internal var _x30: UInt8 = 0
  internal var _x31: UInt8 = 0

  internal init() {}

  internal mutating func withBytes<Result>(
    _ body: (UnsafeMutablePointer<UInt8>) throws -> Result
  ) rethrows -> Result {
    return try withUnsafeMutablePointer(to: &self) {
      try body(UnsafeMutableRawPointer($0).assumingMemoryBound(to: UInt8.self))
    }
  }
}

/// A 72 byte buffer.
internal struct _Buffer72 {
  internal var _x0: UInt8 = 0
  internal var _x1: UInt8 = 0
  internal var _x2: UInt8 = 0
  internal var _x3: UInt8 = 0
  internal var _x4: UInt8 = 0
  internal var _x5: UInt8 = 0
  internal var _x6: UInt8 = 0
  internal var _x7: UInt8 = 0
  internal var _x8: UInt8 = 0
  internal var _x9: UInt8 = 0
  internal var _x10: UInt8 = 0
  internal var _x11: UInt8 = 0
  internal var _x12: UInt8 = 0
  internal var _x13: UInt8 = 0
  internal var _x14: UInt8 = 0
  internal var _x15: UInt8 = 0
  internal var _x16: UInt8 = 0
  internal var _x17: UInt8 = 0
  internal var _x18: UInt8 = 0
  internal var _x19: UInt8 = 0
  internal var _x20: UInt8 = 0
  internal var _x21: UInt8 = 0
  internal var _x22: UInt8 = 0
  internal var _x23: UInt8 = 0
  internal var _x24: UInt8 = 0
  internal var _x25: UInt8 = 0
  internal var _x26: UInt8 = 0
  internal var _x27: UInt8 = 0
  internal var _x28: UInt8 = 0
  internal var _x29: UInt8 = 0
  internal var _x30: UInt8 = 0
  internal var _x31: UInt8 = 0
  internal var _x32: UInt8 = 0
  internal var _x33: UInt8 = 0
  internal var _x34: UInt8 = 0
  internal var _x35: UInt8 = 0
  internal var _x36: UInt8 = 0
  internal var _x37: UInt8 = 0
  internal var _x38: UInt8 = 0
  internal var _x39: UInt8 = 0
  internal var _x40: UInt8 = 0
  internal var _x41: UInt8 = 0
  internal var _x42: UInt8 = 0
  internal var _x43: UInt8 = 0
  internal var _x44: UInt8 = 0
  internal var _x45: UInt8 = 0
  internal var _x46: UInt8 = 0
  internal var _x47: UInt8 = 0
  internal var _x48: UInt8 = 0
  internal var _x49: UInt8 = 0
  internal var _x50: UInt8 = 0
  internal var _x51: UInt8 = 0
  internal var _x52: UInt8 = 0
  internal var _x53: UInt8 = 0
  internal var _x54: UInt8 = 0
  internal var _x55: UInt8 = 0
  internal var _x56: UInt8 = 0
  internal var _x57: UInt8 = 0
  internal var _x58: UInt8 = 0
  internal var _x59: UInt8 = 0
  internal var _x60: UInt8 = 0
  internal var _x61: UInt8 = 0
  internal var _x62: UInt8 = 0
  internal var _x63: UInt8 = 0
  internal var _x64: UInt8 = 0
  internal var _x65: UInt8 = 0
  internal var _x66: UInt8 = 0
  internal var _x67: UInt8 = 0
  internal var _x68: UInt8 = 0
  internal var _x69: UInt8 = 0
  internal var _x70: UInt8 = 0
  internal var _x71: UInt8 = 0

  internal init() {}

  internal mutating func withBytes<Result>(
    _ body: (UnsafeMutablePointer<UInt8>) throws -> Result
  ) rethrows -> Result {
    return try withUnsafeMutablePointer(to: &self) {
      try body(UnsafeMutableRawPointer($0).assumingMemoryBound(to: UInt8.self))
    }
  }
}

#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))
#if arch(wasm32)
// Note that this takes a Float32 argument instead of Float16, because clang
// doesn't have _Float16 on all platforms yet.
@available(SwiftStdlib 5.3, *)
typealias _CFloat16Argument = Float32
#else
@available(SwiftStdlib 5.3, *)
typealias _CFloat16Argument = Float16
#endif

@available(SwiftStdlib 5.3, *)
@_silgen_name("swift_float16ToString")
internal func _float16ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: _CFloat16Argument,
  _ debug: Bool
) -> Int

@available(SwiftStdlib 5.3, *)
internal func _float16ToString(
  _ value: Float16,
  debug: Bool
) -> (buffer: _Buffer32, length: Int) {
  _internalInvariant(MemoryLayout<_Buffer32>.size == 32)
  var buffer = _Buffer32()
  let length = buffer.withBytes { (bufferPtr) in
    _float16ToStringImpl(bufferPtr, 32, _CFloat16Argument(value), debug)
  }
  return (buffer, length)
}
#endif

// Returns a UInt64, but that value is the length of the string, so it's
// guaranteed to fit into an Int. This is part of the ABI, so we can't
// trivially change it to Int. Callers can safely convert the result
// to any integer type without checks, however.
@_silgen_name("swift_float32ToString")
internal func _float32ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float32,
  _ debug: Bool
) -> UInt64

internal func _float32ToString(
  _ value: Float32,
  debug: Bool
) -> (buffer: _Buffer32, length: Int) {
  _internalInvariant(MemoryLayout<_Buffer32>.size == 32)
  var buffer = _Buffer32()
  let length = buffer.withBytes { (bufferPtr) in Int(
    truncatingIfNeeded: _float32ToStringImpl(bufferPtr, 32, value, debug)
  )}
  return (buffer, length)
}

// Returns a UInt64, but that value is the length of the string, so it's
// guaranteed to fit into an Int. This is part of the ABI, so we can't
// trivially change it to Int. Callers can safely convert the result
// to any integer type without checks, however.
@_silgen_name("swift_float64ToString")
internal func _float64ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float64,
  _ debug: Bool
) -> UInt64

internal func _float64ToString(
  _ value: Float64,
  debug: Bool
) -> (buffer: _Buffer32, length: Int) {
  _internalInvariant(MemoryLayout<_Buffer32>.size == 32)
  var buffer = _Buffer32()
  let length = buffer.withBytes { (bufferPtr) in Int(
    truncatingIfNeeded: _float64ToStringImpl(bufferPtr, 32, value, debug)
  )}
  return (buffer, length)
}


#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !(os(macOS) || os(iOS) || os(watchOS) || os(tvOS)))) && (arch(i386) || arch(x86_64))

// Returns a UInt64, but that value is the length of the string, so it's
// guaranteed to fit into an Int. This is part of the ABI, so we can't
// trivially change it to Int. Callers can safely convert the result
// to any integer type without checks, however.
@_silgen_name("swift_float80ToString")
internal func _float80ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float80,
  _ debug: Bool
) -> UInt64

internal func _float80ToString(
  _ value: Float80,
  debug: Bool
) -> (buffer: _Buffer32, length: Int) {
  _internalInvariant(MemoryLayout<_Buffer32>.size == 32)
  var buffer = _Buffer32()
  let length = buffer.withBytes { (bufferPtr) in Int(
    truncatingIfNeeded: _float80ToStringImpl(bufferPtr, 32, value, debug)
  )}
  return (buffer, length)
}
#endif

#if !$Embedded
// Returns a UInt64, but that value is the length of the string, so it's
// guaranteed to fit into an Int. This is part of the ABI, so we can't
// trivially change it to Int. Callers can safely convert the result
// to any integer type without checks, however.
@_silgen_name("swift_int64ToString")
internal func _int64ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Int64,
  _ radix: Int64,
  _ uppercase: Bool
) -> UInt64
#else
internal func _int64ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Int64,
  _ radix: Int64,
  _ uppercase: Bool
) -> UInt64 {
  return UInt64(value._toStringImpl(buffer, bufferLength, Int(radix), uppercase))
}
#endif

internal func _int64ToString(
  _ value: Int64,
  radix: Int64 = 10,
  uppercase: Bool = false
) -> String {
  if radix >= 10 {
    var buffer = _Buffer32()
    return buffer.withBytes { (bufferPtr) in
      let actualLength = _int64ToStringImpl(bufferPtr, 32, value, radix, uppercase)
      return String._fromASCII(UnsafeBufferPointer(
        start: bufferPtr, count: Int(truncatingIfNeeded: actualLength)
      ))
    }
  } else {
    var buffer = _Buffer72()
    return buffer.withBytes { (bufferPtr) in
      let actualLength = _int64ToStringImpl(bufferPtr, 72, value, radix, uppercase)
      return String._fromASCII(UnsafeBufferPointer(
        start: bufferPtr, count: Int(truncatingIfNeeded: actualLength)
      ))
    }
  }
}

#if !$Embedded
// Returns a UInt64, but that value is the length of the string, so it's
// guaranteed to fit into an Int. This is part of the ABI, so we can't
// trivially change it to Int. Callers can safely convert the result
// to any integer type without checks, however.
@_silgen_name("swift_uint64ToString")
internal func _uint64ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: UInt64,
  _ radix: Int64,
  _ uppercase: Bool
) -> UInt64
#else
internal func _uint64ToStringImpl(
  _ buffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: UInt64,
  _ radix: Int64,
  _ uppercase: Bool
) -> UInt64 {
  return UInt64(value._toStringImpl(buffer, bufferLength, Int(radix), uppercase))
}
#endif

public // @testable
func _uint64ToString(
    _ value: UInt64,
    radix: Int64 = 10,
    uppercase: Bool = false
) -> String {
  if radix >= 10 {
    var buffer = _Buffer32()
    return buffer.withBytes { (bufferPtr) in
      let actualLength = _uint64ToStringImpl(bufferPtr, 32, value, radix, uppercase)
      return String._fromASCII(UnsafeBufferPointer(
        start: bufferPtr, count: Int(truncatingIfNeeded: actualLength)
      ))
    }
  } else {
    var buffer = _Buffer72()
    return buffer.withBytes { (bufferPtr) in
      let actualLength = _uint64ToStringImpl(bufferPtr, 72, value, radix, uppercase)
      return String._fromASCII(UnsafeBufferPointer(
        start: bufferPtr, count: Int(truncatingIfNeeded: actualLength)
      ))
    }
  }
}

@inlinable
internal func _rawPointerToString(_ value: Builtin.RawPointer) -> String {
  var result = _uint64ToString(
    UInt64(
      UInt(bitPattern: UnsafeRawPointer(value))),
      radix: 16,
      uppercase: false
    )
  for _ in 0..<(2 * MemoryLayout<UnsafeRawPointer>.size - result.utf16.count) {
    result = "0" + result
  }
  return "0x" + result
}

#if _runtime(_ObjC)
// At runtime, these classes are derived from `__SwiftNativeNSXXXBase`,
// which are derived from `NSXXX`.
//
// The @swift_native_objc_runtime_base attribute
// allows us to subclass an Objective-C class and still use the fast Swift
// memory allocator.
//
// NOTE: older runtimes called these _SwiftNativeNSXXX. The two must
// coexist, so they were renamed. The old names must not be used in the
// new runtime.

@_fixed_layout
@usableFromInline
@objc @_swift_native_objc_runtime_base(__SwiftNativeNSArrayBase)
internal class __SwiftNativeNSArray {
  @inlinable
  @nonobjc
  internal init() {}
//  @objc public init(coder: AnyObject) {}
  @inlinable
  deinit {}
}

@available(*, unavailable)
extension __SwiftNativeNSArray: Sendable {}

@_fixed_layout
@usableFromInline
@objc @_swift_native_objc_runtime_base(__SwiftNativeNSMutableArrayBase)
internal class _SwiftNativeNSMutableArray {
  @inlinable
  @nonobjc
  internal init() {}
//  @objc public init(coder: AnyObject) {}
  @inlinable
  deinit {}
}

@available(*, unavailable)
extension _SwiftNativeNSMutableArray: Sendable {}

@_fixed_layout
@usableFromInline
@objc @_swift_native_objc_runtime_base(__SwiftNativeNSDictionaryBase)
internal class __SwiftNativeNSDictionary {
  @nonobjc
  internal init() {}
  @objc public init(coder: AnyObject) {}
  deinit {}
}

@available(*, unavailable)
extension __SwiftNativeNSDictionary: Sendable {}

@_fixed_layout
@usableFromInline
@objc @_swift_native_objc_runtime_base(__SwiftNativeNSSetBase)
internal class __SwiftNativeNSSet {
  @nonobjc
  internal init() {}
  @objc public init(coder: AnyObject) {}
  deinit {}
}

@available(*, unavailable)
extension __SwiftNativeNSSet: Sendable {}

@objc
@_swift_native_objc_runtime_base(__SwiftNativeNSEnumeratorBase)
internal class __SwiftNativeNSEnumerator {
  @nonobjc
  internal init() {}
  @objc public init(coder: AnyObject) {}
  deinit {}
}

//===----------------------------------------------------------------------===//
// Support for reliable testing of the return-autoreleased optimization
//===----------------------------------------------------------------------===//

@objc
internal class __stdlib_ReturnAutoreleasedDummy {
  @objc
  internal init() {}

  // Use 'dynamic' to force Objective-C dispatch, which uses the
  // return-autoreleased call sequence.
  @objc
  internal dynamic func returnsAutoreleased(_ x: AnyObject) -> AnyObject {
    return x
  }
}

/// This function ensures that the return-autoreleased optimization works.
///
/// On some platforms (for example, x86_64), the first call to
/// `objc_autoreleaseReturnValue` will always autorelease because it would fail
/// to verify the instruction sequence in the caller.  On x86_64 certain PLT
/// entries would be still pointing to the resolver function, and sniffing
/// the call sequence would fail.
///
/// This code should live in the core stdlib dylib because PLT tables are
/// separate for each dylib.
///
/// Call this function in a fresh autorelease pool.
public func _stdlib_initializeReturnAutoreleased() {
#if arch(x86_64)
  // On x86_64 it is sufficient to perform one cycle of return-autoreleased
  // call sequence in order to initialize all required PLT entries.
  let dummy = __stdlib_ReturnAutoreleasedDummy()
  _ = dummy.returnsAutoreleased(dummy)
#endif
}
#else

@_fixed_layout
@usableFromInline
internal class __SwiftNativeNSArray {
  @inlinable
  internal init() {}
  @inlinable
  deinit {}
}
@_fixed_layout
@usableFromInline
internal class __SwiftNativeNSDictionary {
  @inlinable
  internal init() {}
  @inlinable
  deinit {}
}
@_fixed_layout
@usableFromInline
internal class __SwiftNativeNSSet {
  @inlinable
  internal init() {}
  @inlinable
  deinit {}
}

#endif
