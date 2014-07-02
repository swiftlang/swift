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
@public typealias CChar = Int8

/// The C 'unsigned char' type.
@public typealias CUnsignedChar = UInt8

/// The C 'unsigned short' type.
@public typealias CUnsignedShort = UInt16

/// The C 'unsigned int' type.
@public typealias CUnsignedInt = UInt32

/// The C 'unsigned long' type.
@public typealias CUnsignedLong = UInt

/// The C 'unsigned long long' type.
@public typealias CUnsignedLongLong = UInt64

/// The C 'signed char' type.
@public typealias CSignedChar = Int8

/// The C 'short' type.
@public typealias CShort = Int16

/// The C 'int' type.
@public typealias CInt = Int32

/// The C 'long' type.
@public typealias CLong = Int

/// The C 'long long' type.
@public typealias CLongLong = Int64

/// The C 'float' type.
@public typealias CFloat = Float

/// The C 'double' type.
@public typealias CDouble = Double

/// FIXME: long double

// FIXME: Is it actually UTF-32 on Darwin?
//
/// The C++ 'wchar_t' type.
@public typealias CWideChar = UnicodeScalar

// FIXME: Swift should probably have a UTF-16 type other than UInt16.
//
/// The C++11 'char16_t' type, which has UTF-16 encoding.
@public typealias CChar16 = UInt16

/// The C++11 'char32_t' type, which has UTF-32 encoding.
@public typealias CChar32 = UnicodeScalar

/// The C '_Bool' and C++ 'bool' type.
@public typealias CBool = Bool

/// A wrapper around an opaque C pointer.
///
/// Opaque pointers are used to represent C pointers to types that
/// cannot be represented in Swift, such as incomplete struct types.
@public struct COpaquePointer : Equatable, Hashable, LogicValue,
                                NilLiteralConvertible {
  var value : Builtin.RawPointer
  
  @public init() {
    var zero : Int = 0
    value = Builtin.inttoptr_Word(zero.value)
  }

  init(_ v: Builtin.RawPointer) {
    value = v
  }

  @public static func null() -> COpaquePointer {
    return COpaquePointer()
  }

  /// Determine whether the given pointer is null.
  @transparent
  var _isNull : Bool {
    return self == COpaquePointer.null()
  }
  
  @transparent @public
  func getLogicValue() -> Bool {
    return !_isNull
  }

  @public var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(value))
  }
  
  @transparent @public
  static func convertFromNilLiteral() -> COpaquePointer {
    return COpaquePointer()
  }
}

@public func ==(lhs: COpaquePointer, rhs: COpaquePointer) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

extension COpaquePointer {
  // FIXME: Make this an implicit conversion?
  // FIXME: This shouldn't have to be in an extension.
  //
  /// Convert a typed UnsafePointer to an opaque C pointer.
  @public init<T>(_ from : UnsafePointer<T>) {
    value = from.value;
  }
}

@public struct CFunctionPointer<T> : Equatable, Hashable, LogicValue,
                                     NilLiteralConvertible {
  var value: COpaquePointer

  @public init() {
    value = COpaquePointer()
  }

  @public init(_ value: COpaquePointer) {
    self.value = value
  }

  @public static func null() -> CFunctionPointer {
    return CFunctionPointer()
  }

  @transparent @public
  func getLogicValue() -> Bool {
    return value.getLogicValue()
  }

  @public var hashValue: Int {
    return value.hashValue
  }

  @transparent @public
  static func convertFromNilLiteral() -> CFunctionPointer {
    return CFunctionPointer()
  }
}

@public func ==<T>(lhs: CFunctionPointer<T>, rhs: CFunctionPointer<T>) -> Bool {
  return lhs.value == rhs.value
}

extension COpaquePointer {
  @public init<T>(_ from: CFunctionPointer<T>) {
    self = from.value
  }
}


// The C va_list type
@public struct CVaListPointer {
  var value: UnsafePointer<Void>

  init(fromUnsafePointer from: UnsafePointer<Void>) {
    value = from
  }
}

/// Access to the raw argc value from C.
@public var C_ARGC : CInt = CInt()

/// Access to the raw argv value from C. Accessing the argument vector
/// through this pointer is unsafe.
@public var C_ARGV : UnsafePointer<UnsafePointer<Int8>> = UnsafePointer()

func _memcpy(#dest: UnsafePointer<Void>, #src: UnsafePointer<Void>,
             #size: UInt) {
  let dest = dest.value
  let src = src.value
  let size = UInt64(size).value
  Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, size,
                                                 /*alignment*/Int32().value,
                                                 /*volatile*/false.value)
}
