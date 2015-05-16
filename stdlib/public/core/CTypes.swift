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

/// The C 'long' type.
public typealias CLong = Int

/// The C 'long long' type.
public typealias CLongLong = Int64

/// The C 'float' type.
public typealias CFloat = Float

/// The C 'double' type.
public typealias CDouble = Double

// FIXME: long double

// FIXME: Is it actually UTF-32 on Darwin?
//
/// The C++ 'wchar_t' type.
public typealias CWideChar = UnicodeScalar

// FIXME: Swift should probably have a UTF-16 type other than UInt16.
//
/// The C++11 'char16_t' type, which has UTF-16 encoding.
public typealias CChar16 = UInt16

/// The C++11 'char32_t' type, which has UTF-32 encoding.
public typealias CChar32 = UnicodeScalar

/// The C '_Bool' and C++ 'bool' type.
public typealias CBool = Bool

/// A wrapper around an opaque C pointer.
///
/// Opaque pointers are used to represent C pointers to types that
/// cannot be represented in Swift, such as incomplete struct types.
public struct COpaquePointer : Equatable, Hashable, NilLiteralConvertible {
  var _rawValue: Builtin.RawPointer

  /// Construct a `nil` instance.
  @transparent
  public init() {
    _rawValue = _nilRawPointer
  }

  @transparent
  init(_ v: Builtin.RawPointer) {
    _rawValue = v
  }

  /// Construct a `COpaquePointer` from a given address in memory.
  ///
  /// This is a fundamentally unsafe conversion.
  @transparent
  public init(bitPattern: Word) {
    _rawValue = Builtin.inttoptr_Word(bitPattern._builtinWordValue)
  }

  /// Construct a `COpaquePointer` from a given address in memory.
  ///
  /// This is a fundamentally unsafe conversion.
  @transparent
  public init(bitPattern: UWord) {
    _rawValue = Builtin.inttoptr_Word(bitPattern._builtinWordValue)
  }

  /// Convert a typed `UnsafePointer` to an opaque C pointer.
  @transparent
  public init<T>(_ source: UnsafePointer<T>) {
    self._rawValue = source._rawValue
  }

  /// Convert a typed `UnsafeMutablePointer` to an opaque C pointer.
  @transparent
  public init<T>(_ source: UnsafeMutablePointer<T>) {
    self._rawValue = source._rawValue
  }

  /// Determine whether the given pointer is null.
  @transparent
  var _isNull : Bool {
    return self == nil
  }

  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
  ///
  /// - Note: The hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(_rawValue))
  }

  /// Create an instance initialized with `nil`.
  @transparent public
  init(nilLiteral: ()) {
    _rawValue = _nilRawPointer
  }
}

extension COpaquePointer : CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return _rawPointerToString(_rawValue)
  }
}

public func ==(lhs: COpaquePointer, rhs: COpaquePointer) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
}

/// The family of C function pointer types.
///
/// This type has been removed. Instead of `CFunctionType<(T) -> U>`, a native
/// function type with the C convention can be used, `@convention(c) (T) -> U`.
@available(*, unavailable, message="use a function type '@convention(c) (T) -> U'")
public struct CFunctionPointer<T> {}

/// The corresponding Swift type to `va_list` in imported C APIs.
public struct CVaListPointer {
  var value: UnsafeMutablePointer<Void>

  public // @testable
  init(_fromUnsafeMutablePointer from: UnsafeMutablePointer<Void>) {
    value = from
  }
}

extension CVaListPointer : CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return value.debugDescription
  }
}

func _memcpy(
  dest destination: UnsafeMutablePointer<Void>,
  src: UnsafeMutablePointer<Void>,
  size: UInt
) {
  let dest = destination._rawValue
  let src = src._rawValue
  let size = UInt64(size).value
  Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, size,
                                                 /*alignment*/Int32().value,
                                                 /*volatile*/false.value)
}
