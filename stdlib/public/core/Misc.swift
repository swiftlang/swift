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

@transparent
public // @testable
func _countLeadingZeros(value: Int64) -> Int64 {
    return Int64(Builtin.int_ctlz_Int64(value.value, false.value))
}

/// Returns if `x` is a power of 2.
@transparent
public // @testable
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
public // @testable
func _isPowerOf2(x: Int) -> Bool {
  if x <= 0 {
    return false
  }
  // Note: use unchecked subtraction because we have checked that `x` is not
  // `Int.min`.
  return x & (x &- 1) == 0
}

#if _runtime(_ObjC)
@transparent public func _autorelease(x: AnyObject) {
  Builtin.retain(x)
  Builtin.autorelease(x)
}
#endif

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

@asmname("swift_stdlib_getDemangledTypeName")
public func _stdlib_getDemangledTypeNameImpl<T>(value: T, result: UnsafeMutablePointer<String>)

@asmname("swift_stdlib_getDemangledMetatypeName")
public func _stdlib_getDemangledMetatypeNameImpl(type: Any.Type, result: UnsafeMutablePointer<String>)

/// Returns the demangled name of a metatype.
public func _typeName(type: Any.Type) -> String {
  var stringPtr = UnsafeMutablePointer<String>.alloc(1)
  _stdlib_getDemangledMetatypeNameImpl(type, stringPtr)
  let result = stringPtr.move()
  stringPtr.dealloc(1)
  return result
}

/// Returns the human-readable type name for the given value.
public func _stdlib_getDemangledTypeName<T>(value: T) -> String {
  // FIXME: this code should be using _withUninitializedString, but it leaks
  // when called from here.
  // <rdar://problem/17892969> Closures in generic context leak their captures?
  var stringPtr = UnsafeMutablePointer<String>.alloc(1)
  _stdlib_getDemangledTypeNameImpl(value, stringPtr)
  let stringResult = stringPtr.move()
  stringPtr.dealloc(1)
  return stringResult
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
public // @testable
func _floorLog2(x: Int64) -> Int {
  _sanityCheck(x > 0, "_floorLog2 operates only on non-negative integers")
  // Note: use unchecked subtraction because we this expression can not
  // overflow.
  return 63 &- Int(_countLeadingZeros(x))
}

