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
typealias CChar = Int8

/// The C 'unsigned char' type.
typealias CUnsignedChar = UInt8

/// The C 'unsigned short' type.
typealias CUnsignedShort = UInt16

/// The C 'unsigned int' type.
typealias CUnsignedInt = UInt32

/// The C 'unsigned long' type.
typealias CUnsignedLong = UInt

/// The C 'unsigned long long' type.
typealias CUnsignedLongLong = UInt64

/// The C 'signed char' type.
typealias CSignedChar = Int8

/// The C 'short' type.
typealias CShort = Int16

/// The C 'int' type.
typealias CInt = Int32

/// The C 'long' type.
typealias CLong = Int

/// The C 'long long' type.
typealias CLongLong = Int64

/// The C 'float' type.
typealias CFloat = Float

/// The C 'double' type.
typealias CDouble = Double

/// FIXME: long double

// FIXME: Is it actually UTF-32 on Darwin?
//
/// The C++ 'wchar_t' type.
typealias CWideChar = UnicodeScalar

// FIXME: Swift should probably have a UTF-16 type other than UInt16.
//
/// The C++11 'char16_t' type, which has UTF-16 encoding.
typealias CChar16 = UInt16

/// The C++11 'char32_t' type, which has UTF-32 encoding.
typealias CChar32 = UnicodeScalar

/// The C '_Bool' and C++ 'bool' type.
typealias CBool = Bool

/// A wrapper around an opaque C pointer.
///
/// Opaque pointers are used to represent C pointers to types that
/// cannot be represented in Swift, such as incomplete struct types.
struct COpaquePointer : Equatable, Hashable, LogicValue {
  var value : Builtin.RawPointer
  
  init() {
    var zero : Int = 0
    value = Builtin.inttoptr_Word(zero.value)
  }

  init(_ v: Builtin.RawPointer) {
    value = v
  }

  static func null() -> COpaquePointer {
    return COpaquePointer()
  }

  /// Determine whether the given pointer is null.
  @transparent
  var _isNull : Bool {
    return self == COpaquePointer.null()
  }
  
  @transparent
  func getLogicValue() -> Bool {
    return !_isNull
  }

  var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(value))
  }
}

func ==(lhs: COpaquePointer, rhs: COpaquePointer) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

extension COpaquePointer {
  // FIXME: Make this an implicit conversion?
  // FIXME: This shouldn't have to be in an extension.
  //
  /// Convert a typed UnsafePointer to an opaque C pointer.
  init<T>(_ from : UnsafePointer<T>) {
    value = from.value;
  }
}

// Make nil work with COpaquePointer.
extension _Nil {
  @conversion func __conversion() -> COpaquePointer {
    return COpaquePointer()
  }
}

// The C va_list type
struct CVaListPointer {
  var value: UnsafePointer<Void>

  init(fromUnsafePointer from: UnsafePointer<Void>) {
    value = from
  }

  @conversion
  func __conversion() -> CMutableVoidPointer {
    return CMutableVoidPointer(owner: _nilNativeObject, value: value.value)
  }
}

/// Access to the raw argc value from C.
var C_ARGC : CInt = CInt()

/// Access to the raw argv value from C. Accessing the argument vector
/// through this pointer is unsafe.
var C_ARGV : UnsafePointer<CString> = UnsafePointer<CString>()

@asmname("memcpy")
func c_memcpy(#dest: UnsafePointer<Void>, #src: UnsafePointer<Void>, #size: UInt)
