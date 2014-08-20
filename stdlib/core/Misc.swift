//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Extern C functions
//===----------------------------------------------------------------------===//

// FIXME: Once we have an FFI interface, make these have proper function bodies

@asmname("putchar")
func _putchar(value: Int32) -> Int32

@asmname("swift_stdlib_atomicCompareExchangeStrongPtr")
func _stdlib_atomicCompareExchangeStrongPtrImpl(
  #object: UnsafeMutablePointer<COpaquePointer>,
  #expected: UnsafeMutablePointer<COpaquePointer>,
  #desired: COpaquePointer) -> Bool

/// Atomic compare and exchange of `UnsafeMutablePointer<T>` with sequentially
/// consistent memory ordering.  Precise semantics are defined in C++11 or C11.
///
/// Warning: this operation is extremely tricky to use correctly because of
/// writeback semantics.
///
/// It is best to use it directly on an
/// `UnsafeMutablePointer<UnsafeMutablePointer<T>>` that is known to point
/// directly to the memory where the value is stored.
///
/// In a call like this:
///
/// ::
///
///   _stdlib_atomicCompareExchangeStrongPtr(&foo.property1.property2, ...)
///
/// you need to manually make sure that:
///
/// - all properties in the chain are physical (to make sure that no writeback
///   happens; the compare-and-exchange instruction should operate on on the
///   shared memory); and
///
/// - the shared memory that you are accessing is located inside a heap
///   allocation (a class instance property, a `HeapBuffer`, a pointer to
///   an `Array` element etc.)
///
/// If the conditions above are not met, the code will still compile, but the
/// compare-and-exchange instruction will operate on the writeback buffer, and
/// you will get a *race* while doing writeback into shared memory.
@transparent
func _stdlib_atomicCompareExchangeStrongPtr<T>(
  #object: UnsafeMutablePointer<UnsafeMutablePointer<T>>,
  #expected: UnsafeMutablePointer<UnsafeMutablePointer<T>>,
  #desired: UnsafeMutablePointer<T>) -> Bool {
  return _stdlib_atomicCompareExchangeStrongPtrImpl(
    object: object.asPointerTo(COpaquePointer.self),
    expected: expected.asPointerTo(COpaquePointer.self),
    desired: COpaquePointer(desired))
}

@transparent
func _countLeadingZeros(value: Int64) -> Int64 {
    return Int64(Builtin.int_ctlz_Int64(value.value, false.value))
}

/// Returns if `x` is a power of 2.
@transparent
func _isPowerOf2(x: UInt) -> Bool {
  if x == 0 {
    return false
  }
  // Note: use unchecked subtraction because we have checked that `x` is not
  // zero.
  return x & (x &- 1) == 0
}

/// Returns if `x` is a power of 2.
@transparent
func _isPowerOf2(x: Int) -> Bool {
  if x <= 0 {
    return false
  }
  // Note: use unchecked subtraction because we have checked that `x` is not
  // `Int.min`.
  return x & (x &- 1) == 0
}

@transparent public func _autorelease(x: AnyObject) {
  Builtin.retain(x)
  Builtin.autorelease(x)
}

/// Invoke `body` with an allocated, but uninitialized memory suitable for a
/// `String` value.
///
/// This function is primarily useful to call various runtime functions
/// written in C++.
func _withUninitializedString<R>(
  body: (UnsafeMutablePointer<String>) -> R
) -> (R, String) {
  var stringPtr = UnsafeMutablePointer<String>.alloc(1)
  let bodyResult = body(stringPtr)
  let stringResult = stringPtr.move()
  stringPtr.dealloc(1)
  return (bodyResult, stringResult)
}

/// Check if a given object (of value or reference type) conforms to the given
/// protocol.
///
/// Limitation: `DestType` should be a protocol defined in the `Swift` module.
@asmname("swift_stdlib_conformsToProtocol")
public func _stdlib_conformsToProtocol<SourceType, DestType>(
    value: SourceType, _: DestType.Type
) -> Bool

/// Cast the given object (of value or reference type) to the given protocol
/// type.  Traps if the object does not conform to the protocol.
///
/// Limitation: `DestType` should be a protocol defined in the `Swift` module.
@asmname("swift_stdlib_dynamicCastToExistential1Unconditional")
public func _stdlib_dynamicCastToExistential1Unconditional<
    SourceType, DestType
>(
    value: SourceType, _: DestType.Type
) -> DestType

/// Cast the given object (of value or reference type) to the given protocol
/// type.  Returns `.None` if the object does not conform to the protocol.
///
/// Limitation: `DestType` should be a protocol defined in the `Swift` module.
@asmname("swift_stdlib_dynamicCastToExistential1")
public func _stdlib_dynamicCastToExistential1<SourceType, DestType>(
    value: SourceType, _: DestType.Type
) -> DestType?

@asmname("swift_stdlib_getTypeName")
func _stdlib_getTypeNameImpl<T>(value: T, result: UnsafeMutablePointer<String>)

/// Returns the mangled type name for the given value.
public func _stdlib_getTypeName<T>(value: T) -> String {
  // FIXME: this code should be using _withUninitializedString, but it leaks
  // when called from here.
  // <rdar://problem/17892969> Closures in generic context leak their captures?
  var stringPtr = UnsafeMutablePointer<String>.alloc(1)
  _stdlib_getTypeNameImpl(value, stringPtr)
  let stringResult = stringPtr.move()
  stringPtr.dealloc(1)
  return stringResult
}

/// Returns the human-readable type name for the given value.
public func _stdlib_getDemangledTypeName<T>(value: T) -> String {
  return _stdlib_demangleName(_stdlib_getTypeName(value))
}

@asmname("swift_stdlib_demangleName")
func _stdlib_demangleNameImpl(
    mangledName: UnsafePointer<UInt8>,
    mangledNameLength: UWord,
    demangledName: UnsafeMutablePointer<String>)

public func _stdlib_demangleName(mangledName: String) -> String {
  var mangledNameUTF8 = Array(mangledName.utf8)
  return mangledNameUTF8.withUnsafeBufferPointer {
    (mangledNameUTF8) in
    let (_, demangledName) = _withUninitializedString {
      _stdlib_demangleNameImpl(
        mangledNameUTF8.baseAddress, UWord(mangledNameUTF8.endIndex),
        $0)
    }
    return demangledName
  }
}

/// Returns `floor(log(x))`.  This equals to the position of the most
/// significant non-zero bit, or 63 - number-of-zeros before it.
///
/// The function is only defined for positive values of `x`.
///
/// Examples::
///
///    floorLog2(1) == 0
///    floorLog2(2) == floorLog2(3) == 1
///    floorLog2(9) == floorLog2(15) == 3
///
/// TODO: Implement version working on Int instead of Int64.
@transparent
func _floorLog2(x: Int64) -> Int {
  _sanityCheck(x > 0, "_floorLog2 operates only on non-negative integers")
  // Note: use unchecked subtraction because we this expression can not
  // overflow.
  return 63 &- Int(_countLeadingZeros(x))
}

