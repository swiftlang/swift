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
#if os(Windows) && arch(x86_64)
public typealias CUnsignedLong = UInt32
#else
public typealias CUnsignedLong = UInt
#endif

/// The C 'unsigned long long' type.
public typealias CUnsignedLongLong = UInt64

/// The C 'signed char' type.
public typealias CSignedChar = Int8

/// The C 'short' type.
public typealias CShort = Int16

/// The C 'int' type.
public typealias CInt = Int32

/// The C 'long' type.
#if os(Windows) && arch(x86_64)
public typealias CLong = Int32
#else
public typealias CLong = Int
#endif

/// The C 'long long' type.
public typealias CLongLong = Int64

/// The C 'float' type.
public typealias CFloat = Float

/// The C 'double' type.
public typealias CDouble = Double

/// The C 'long double' type.
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
// On Darwin, long double is Float80 on x86, and Double otherwise.
#if arch(x86_64) || arch(i386)
public typealias CLongDouble = Float80
#else
public typealias CLongDouble = Double
#endif
#elseif os(Windows)
// On Windows, long double is always Double.
public typealias CLongDouble = Double
#elseif os(Linux)
// On Linux/x86, long double is Float80.
// TODO: Fill in definitions for additional architectures as needed. IIRC
// armv7 should map to Double, but arm64 and ppc64le should map to Float128,
// which we don't yet have in Swift.
#if arch(x86_64) || arch(i386)
public typealias CLongDouble = Float80
#endif
// TODO: Fill in definitions for other OSes.
#endif

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

/// A wrapper around an opaque C pointer.
///
/// Opaque pointers are used to represent C pointers to types that
/// cannot be represented in Swift, such as incomplete struct types.
@_fixed_layout
public struct OpaquePointer {
  @usableFromInline
  internal var _rawValue: Builtin.RawPointer

  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  internal init(_ v: Builtin.RawPointer) {
    self._rawValue = v
  }

  /// Creates an `OpaquePointer` from a given address in memory.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init?(bitPattern: Int) {
    if bitPattern == 0 { return nil }
    self._rawValue = Builtin.inttoptr_Word(bitPattern._builtinWordValue)
  }

  /// Creates an `OpaquePointer` from a given address in memory.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init?(bitPattern: UInt) {
    if bitPattern == 0 { return nil }
    self._rawValue = Builtin.inttoptr_Word(bitPattern._builtinWordValue)
  }

  /// Converts a typed `UnsafePointer` to an opaque C pointer.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init<T>(_ from: UnsafePointer<T>) {
    self._rawValue = from._rawValue
  }

  /// Converts a typed `UnsafePointer` to an opaque C pointer.
  ///
  /// The result is `nil` if `from` is `nil`.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init?<T>(_ from: UnsafePointer<T>?) {
    guard let unwrapped = from else { return nil }
    self.init(unwrapped)
  }

  /// Converts a typed `UnsafeMutablePointer` to an opaque C pointer.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init<T>(_ from: UnsafeMutablePointer<T>) {
    self._rawValue = from._rawValue
  }

  /// Converts a typed `UnsafeMutablePointer` to an opaque C pointer.
  ///
  /// The result is `nil` if `from` is `nil`.
  @inlinable // FIXME(sil-serialize-all)
  @_transparent
  public init?<T>(_ from: UnsafeMutablePointer<T>?) {
    guard let unwrapped = from else { return nil }
    self.init(unwrapped)
  }
}

extension OpaquePointer: Equatable {
  @inlinable // FIXME(sil-serialize-all)
  public static func == (lhs: OpaquePointer, rhs: OpaquePointer) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
  }
}

extension OpaquePointer: Hashable {
  /// The pointer's hash value.
  ///
  /// The hash value is not guaranteed to be stable across different
  /// invocations of the same program.  Do not persist the hash value across
  /// program runs.
  @inlinable // FIXME(sil-serialize-all)
  public var hashValue: Int {
    return _hashValue(for: self)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func hash(into hasher: inout Hasher) {
    hasher.combine(Int(Builtin.ptrtoint_Word(_rawValue)))
  }
}

extension OpaquePointer : CustomDebugStringConvertible {
  /// A textual representation of the pointer, suitable for debugging.
  @inlinable // FIXME(sil-serialize-all)
  public var debugDescription: String {
    return _rawPointerToString(_rawValue)
  }
}

extension Int {
  /// Creates a new value with the bit pattern of the given pointer.
  ///
  /// The new value represents the address of the pointer passed as `pointer`.
  /// If `pointer` is `nil`, the result is `0`.
  ///
  /// - Parameter pointer: The pointer to use as the source for the new
  ///   integer.
  @inlinable // FIXME(sil-serialize-all)
  public init(bitPattern pointer: OpaquePointer?) {
    self.init(bitPattern: UnsafeRawPointer(pointer))
  }
}

extension UInt {
  /// Creates a new value with the bit pattern of the given pointer.
  ///
  /// The new value represents the address of the pointer passed as `pointer`.
  /// If `pointer` is `nil`, the result is `0`.
  ///
  /// - Parameter pointer: The pointer to use as the source for the new
  ///   integer.
  @inlinable // FIXME(sil-serialize-all)
  public init(bitPattern pointer: OpaquePointer?) {
    self.init(bitPattern: UnsafeRawPointer(pointer))
  }
}

/// A wrapper around a C `va_list` pointer.
@_fixed_layout
public struct CVaListPointer {
  @usableFromInline // FIXME(sil-serialize-all)
  internal var value: UnsafeMutableRawPointer

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  init(_fromUnsafeMutablePointer from: UnsafeMutableRawPointer) {
    value = from
  }
}

extension CVaListPointer : CustomDebugStringConvertible {
  /// A textual representation of the pointer, suitable for debugging.
  @inlinable // FIXME(sil-serialize-all)
  public var debugDescription: String {
    return value.debugDescription
  }
}

@inlinable
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
@inlinable
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
