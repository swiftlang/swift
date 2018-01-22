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
// C Primitive Types
//===----------------------------------------------------------------------===//

/// The C 'char' type.
///
/// This will be the same as either `CSignedChar` (in the common
/// case) or `CUnsignedChar`, depending on the platform.
public typealias CChar = Int8

/// The C 'unsigned char' type.
public typealias CUnsignedChar = UInt8

/// The C 'unsigned short' type.
public typealias CUnsignedShort = UInt16

/// The C 'unsigned int' type.
public typealias CUnsignedInt = UInt32

/// The C 'unsigned long' type.
public typealias CUnsignedLong = UInt

/// The C 'unsigned long long' type.
public typealias CUnsignedLongLong = UInt64

/// The C 'signed char' type.
public typealias CSignedChar = Int8

/// The C 'short' type.
public typealias CShort = Int16

/// The C 'int' type.
public typealias CInt = Int32

#if os(Windows) && arch(x86_64)
/// The C 'long' type.
public typealias CLong = Int32
#else
/// The C 'long' type.
public typealias CLong = Int
#endif

#if os(Windows) && arch(x86_64)
/// The C 'long long' type.
public typealias CLongLong = Int
#else
/// The C 'long long' type.
public typealias CLongLong = Int64
#endif

/// The C 'float' type.
public typealias CFloat = Float

/// The C 'double' type.
public typealias CDouble = Double

// FIXME: long double

// FIXME: Is it actually UTF-32 on Darwin?
//
/// The C++ 'wchar_t' type.
public typealias CWideChar = Unicode.Scalar

// FIXME: Swift should probably have a UTF-16 type other than UInt16.
//
/// The C++11 'char16_t' type, which has UTF-16 encoding.
public typealias CChar16 = UInt16

/// The C++11 'char32_t' type, which has UTF-32 encoding.
public typealias CChar32 = Unicode.Scalar

/// The C '_Bool' and C++ 'bool' type.
public typealias CBool = Bool

@available(swift, deprecated: 5.0, message: "Please use 'UnsafeRawPointer'")
public typealias OpaquePointer = UnsafeRawPointer

/// A wrapper around a C `va_list` pointer.
@_fixed_layout
public struct CVaListPointer {
  @_versioned // FIXME(sil-serialize-all)
  internal var value: UnsafeMutableRawPointer

  @_inlineable // FIXME(sil-serialize-all)
  public // @testable
  init(_fromUnsafeMutablePointer from: UnsafeMutableRawPointer) {
    value = from
  }
}

extension CVaListPointer : CustomDebugStringConvertible {
  /// A textual representation of the pointer, suitable for debugging.
  @_inlineable // FIXME(sil-serialize-all)
  public var debugDescription: String {
    return value.debugDescription
  }
}

@_versioned
@_inlineable
internal func _memcpy(
  dest destination: UnsafeMutableRawPointer,
  src: UnsafeRawPointer,
  size: UInt
) {
  let dest = destination._rawValue
  let src = src._rawValue
  let size = UInt64(size)._value
  Builtin.int_memcpy_RawPointer_RawPointer_Int64(
    dest, src, size,
    /*alignment:*/ Int32()._value,
    /*volatile:*/ false._value)
}

/// Copy `count` bytes of memory from `src` into `dest`.
///
/// The memory regions `source..<source + count` and
/// `dest..<dest + count` may overlap.
@_versioned
@_inlineable
internal func _memmove(
  dest destination: UnsafeMutableRawPointer,
  src: UnsafeRawPointer,
  size: UInt
) {
  let dest = destination._rawValue
  let src = src._rawValue
  let size = UInt64(size)._value
  Builtin.int_memmove_RawPointer_RawPointer_Int64(
    dest, src, size,
    /*alignment:*/ Int32()._value,
    /*volatile:*/ false._value)
}
