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
  let (oldValue, won) = unsafe Builtin.cmpxchg_seqcst_seqcst_Word(
    target._rawValue,
    UInt(bitPattern: expected.pointee)._builtinWordValue,
    UInt(bitPattern: desired)._builtinWordValue)
  unsafe expected.pointee = UnsafeRawPointer(bitPattern: Int(oldValue))
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
  let rawTarget = unsafe UnsafeMutableRawPointer(target).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  let rawExpected = unsafe UnsafeMutableRawPointer(expected).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  return unsafe _stdlib_atomicCompareExchangeStrongPtr(
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
  let rawTarget = unsafe UnsafeMutableRawPointer(target).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  let rawExpected = unsafe UnsafeMutableRawPointer(expected).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  return unsafe _stdlib_atomicCompareExchangeStrongPtr(
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
  let unmanaged = unsafe Unmanaged.passRetained(desired)
  let desiredPtr = unsafe unmanaged.toOpaque()
  let rawTarget = unsafe UnsafeMutableRawPointer(target).assumingMemoryBound(
    to: Optional<UnsafeRawPointer>.self)
  let wonRace = unsafe withUnsafeMutablePointer(to: &expected) {
    unsafe _stdlib_atomicCompareExchangeStrongPtr(
      object: rawTarget, expected: $0, desired: desiredPtr
    )
  }
  if !wonRace {
    // Some other thread initialized the value.  Balance the retain that we
    // performed on 'desired'.
    unsafe unmanaged.release()
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
  if let unwrapped = unsafe UnsafeRawPointer(bitPattern: Int(value)) {
    return unsafe Unmanaged<AnyObject>.fromOpaque(unwrapped).takeUnretainedValue()
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
  let unmanaged = unsafe Unmanaged.passRetained(desired)
  let desiredPtr = unsafe unmanaged.toOpaque()

  let (value, won) = Builtin.cmpxchg_acqrel_acquire_Word(
    target._rawValue,
    0._builtinWordValue,
    Builtin.ptrtoint_Word(desiredPtr._rawValue))

  if Bool(won) { return unsafe unmanaged }

  // Some other thread initialized the value before us. Balance the retain that
  // we performed on 'desired', and return what we loaded.
  unsafe unmanaged.release()
  let ptr = UnsafeRawPointer(Builtin.inttoptr_Word(value))
  return unsafe Unmanaged<T>.fromOpaque(ptr)
}

@_alwaysEmitIntoClient
@_transparent
public func _stdlib_atomicAcquiringLoadARCRef<T: AnyObject>(
  object target: UnsafeMutablePointer<T?>
) -> Unmanaged<T>? {
  let value = Builtin.atomicload_acquire_Word(target._rawValue)
  if Int(value) == 0 { return nil }
  let opaque = UnsafeRawPointer(Builtin.inttoptr_Word(value))
  return unsafe Unmanaged<T>.fromOpaque(opaque)
}

//===----------------------------------------------------------------------===//
// Conversion of primitive types to `String`
//===----------------------------------------------------------------------===//

@inlinable
internal func _rawPointerToString(_ value: Builtin.RawPointer) -> String {
  var result = _uint64ToString(
    UInt64(UInt(bitPattern: UnsafeRawPointer(value))),
    radix: 16,
    uppercase: false
  )
  for _ in unsafe 0..<(2 * MemoryLayout<UnsafeRawPointer>.size - result.utf16.count) {
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
