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

@transparent public func _countLeadingZeros(value: Int64) -> Int64 {
    return Int64(Builtin.int_ctlz_Int64(value.value, false.value))
}

@transparent public func _autorelease(x: AnyObject) {
  Builtin.retain(x)
  Builtin.autorelease(x)
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
  var resultPtr = UnsafeMutablePointer<String>.alloc(1)
  _stdlib_getTypeNameImpl(value, resultPtr)
  let result = resultPtr.memory
  resultPtr.dealloc(1)
  return result
}

@asmname("swift_stdlib_demangleName")
func _stdlib_demangleNameImpl(
    mangledName: UnsafePointer<UInt8>,
    mangledNameLength: UWord,
    demangledName: UnsafeMutablePointer<String>)

public func _stdlib_demangleName(mangledName: String) -> String {
  var resultPtr = UnsafeMutablePointer<String>.alloc(1)
  var mangledNameUTF8 = Array(mangledName.utf8)
  mangledNameUTF8.withUnsafeBufferPointer {
    _stdlib_demangleNameImpl($0.baseAddress, UWord($0.endIndex), resultPtr)
  }
  let result = resultPtr.memory
  resultPtr.dealloc(1)
  return result
}

