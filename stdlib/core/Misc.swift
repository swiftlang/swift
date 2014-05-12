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

// The C "abort" function
@asmname("abort") func _abort()

@asmname("putchar")
func c_putchar(value: Int32)
@asmname("print_int")
func c_print_int(p: Builtin.RawPointer, buf_len: Int, x: Int64, Radix: Int,
                 uppercase: Bool) -> UInt64
@asmname("print_uint")
func c_print_uint(p: Builtin.RawPointer, buf_len: Int, x: UInt64, Radix: Int,
                  uppercase: Bool) -> UInt64
@asmname("print_double")
func c_print_double(p: Builtin.RawPointer, x: Double) -> UInt64

@asmname("swift_replOutputIsUTF8") func _isUTF8() -> Bool

// Some file stuff

@asmname("write")
func posix_write(fd: Int32, buf: Builtin.RawPointer, sz: Int) -> Int

@asmname("read")
func posix_read(fd: Int32, buf: Builtin.RawPointer, sz: Int) -> Int

@asmname("llvm.ctlz.i64")
func __llvm_ctlz(value: Builtin.Int64, isZeroUndef: Builtin.Int1) -> Builtin.Int64

@transparent func countLeadingZeros(value: Int64) -> Int64 {
    return Int64(__llvm_ctlz(value.value, false.value))
}

@transparent func _autorelease(x: AnyObject) {
  Builtin.retain(x)
  Builtin.autorelease(x)
}

/// Check if a given object (of value or reference type) conforms to the given
/// protocol.
@asmname("swift_stdlib_conformsToProtocol")
func _stdlib_conformsToProtocol<SourceType, DestType>(
    value: SourceType, _: DestType.Type
) -> Bool

/// Cast the given object (of value or reference type) to the given protocol
/// type.  Traps if the object does not conform to the protocol.
@asmname("swift_stdlib_dynamicCastToExistential1Unconditional")
func _stdlib_dynamicCastToExistential1Unconditional<SourceType, DestType>(
    value: SourceType, _: DestType.Type
) -> DestType

/// Cast the given object (of value or reference type) to the given protocol
/// type.  Returns `.None` if the object does not conform to the protocol.
@asmname("swift_stdlib_dynamicCastToExistential1")
func _stdlib_dynamicCastToExistential1<SourceType, DestType>(
    value: SourceType, _: DestType.Type
) -> DestType?

