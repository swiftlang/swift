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
// Extern C functions
//===----------------------------------------------------------------------===//

// FIXME: Once we have an FFI interface, make these have proper function bodies

@_inlineable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _countLeadingZeros(_ value: Int64) -> Int64 {
    return Int64(Builtin.int_ctlz_Int64(value._value, false._value))
}

/// Returns if `x` is a power of 2.
@_inlineable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _isPowerOf2(_ x: UInt) -> Bool {
  if x == 0 {
    return false
  }
  // Note: use unchecked subtraction because we have checked that `x` is not
  // zero.
  return x & (x &- 1) == 0
}

/// Returns if `x` is a power of 2.
@_inlineable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _isPowerOf2(_ x: Int) -> Bool {
  if x <= 0 {
    return false
  }
  // Note: use unchecked subtraction because we have checked that `x` is not
  // `Int.min`.
  return x & (x &- 1) == 0
}

#if _runtime(_ObjC)
@_inlineable // FIXME(sil-serialize-all)
@_transparent
public func _autorelease(_ x: AnyObject) {
  Builtin.retain(x)
  Builtin.autorelease(x)
}
#endif

/// Invoke `body` with an allocated, but uninitialized memory suitable for a
/// `String` value.
///
/// This function is primarily useful to call various runtime functions
/// written in C++.
@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal func _withUninitializedString<R>(
  _ body: (UnsafeMutablePointer<String>) -> R
) -> (R, String) {
  let stringPtr = UnsafeMutablePointer<String>.allocate(capacity: 1)
  let bodyResult = body(stringPtr)
  let stringResult = stringPtr.move()
  stringPtr.deallocate()
  return (bodyResult, stringResult)
}

// FIXME(ABI)#51 : this API should allow controlling different kinds of
// qualification separately: qualification with module names and qualification
// with type names that we are nested in.
// But we can place it behind #if _runtime(_Native) and remove it from ABI on
// Apple platforms, deferring discussions mentioned above.
@_inlineable // FIXME(sil-serialize-all)
@_silgen_name("swift_getTypeName")
public func _getTypeName(_ type: Any.Type, qualified: Bool)
  -> (UnsafePointer<UInt8>, Int)

/// Returns the demangled qualified name of a metatype.
@_inlineable // FIXME(sil-serialize-all)
public // @testable
func _typeName(_ type: Any.Type, qualified: Bool = true) -> String {
  let (stringPtr, count) = _getTypeName(type, qualified: qualified)
  return ._fromWellFormedCodeUnitSequence(UTF8.self,
    input: UnsafeBufferPointer(start: stringPtr, count: count))
}

/// Lookup a class given a name. Until the demangled encoding of type
/// names is stabilized, this is limited to top-level class names (Foo.bar).
public // SPI(Foundation)
func _typeByName(_ name: String) -> Any.Type? {
  return _typeByMangledName(name);
}

@_silgen_name("swift_getTypeByMangledName")
internal func _getTypeByMangledName(
  _ name: UnsafePointer<UInt8>,
  _ nameLength: UInt,
  _ numberOfLevels: UInt,
  _ parametersPerLevel: UnsafePointer<UInt>,
  _ substitutions: UnsafePointer<Any.Type>)
  -> Any.Type?

/// Lookup a class given a mangled name. This is a placeholder while we bring
/// up this functionality.
public  // TEMPORARY
func _typeByMangledName(_ name: String,
                        substitutions: [[Any.Type]] = []) -> Any.Type? {
  // Map the substitutions to a flat representation that's easier to thread
  // through to the runtime.
  let numberOfLevels = UInt(substitutions.count)
  var parametersPerLevel = [UInt]()
  var flatSubstitutions = [Any.Type]()
  for level in substitutions {
    parametersPerLevel.append(UInt(level.count))
    flatSubstitutions.append(contentsOf: level)
  }

  let nameUTF8 = Array(name.utf8)
  return nameUTF8.withUnsafeBufferPointer { (nameUTF8) in
    return  _getTypeByMangledName(nameUTF8.baseAddress!,
                                  UInt(nameUTF8.endIndex),
                                  numberOfLevels,
                                  parametersPerLevel,
                                  flatSubstitutions)
  }
}

/// Returns `floor(log(x))`.  This equals to the position of the most
/// significant non-zero bit, or 63 - number-of-zeros before it.
///
/// The function is only defined for positive values of `x`.
///
/// Examples:
///
///      floorLog2(1) == 0
///      floorLog2(2) == floorLog2(3) == 1
///      floorLog2(9) == floorLog2(15) == 3
///
/// TODO: Implement version working on Int instead of Int64.
@_inlineable // FIXME(sil-serialize-all)
@_transparent
public // @testable
func _floorLog2(_ x: Int64) -> Int {
  _sanityCheck(x > 0, "_floorLog2 operates only on non-negative integers")
  // Note: use unchecked subtraction because we this expression cannot
  // overflow.
  return 63 &- Int(_countLeadingZeros(x))
}
