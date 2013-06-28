//===----------------------------------------------------------------------===//
// C Primitive Types
//===----------------------------------------------------------------------===//

/// \brief The C 'char' type.
///
/// This will be the same as either \c CSignedChar (in the common
/// case) or \c CUnsignedChar, depending on the platform.
typealias CChar = Int8

/// \brief The C 'unsigned char' type.
typealias CUnsignedChar = UInt8

/// \brief The C 'unsigned short' type.
typealias CUnsignedShort = UInt16

/// \brief The C 'unsigned int' type.
typealias CUnsignedInt = UInt32

/// \brief The C 'unsigned long' type.
typealias CUnsignedLong = UInt64

/// \brief The C 'unsigned long long' type.
typealias CUnsignedLongLong = UInt64

/// \brief The C 'unsigned __int128' extended integer type.
typealias CUnsignedInt128 = UInt128

/// \brief The C 'signed char' type.
typealias CSignedChar = Int8

/// \brief The C 'short' type.
typealias CShort = Int16

/// \brief The C 'int' type.
typealias CInt = Int32

/// \brief The C 'long' type.
typealias CLong = Int64

/// \brief The C 'long long' type.
typealias CLongLong = Int64

/// \brief The C '__int128' extended integer type.
typealias CInt128 = Int128

/// \brief The C 'float' type.
typealias CFloat = Float

/// \brief The C 'double' type.
typealias CDouble = Double

/// FIXME: long double

/// \brief The C++ 'wchar_t' type.
///
/// FIXME: Is it actually UTF-32 on Darwin?
typealias CWideChar = Char

/// \brief The C++11 'char16_t' type, which has UTF-16 encoding.
///
/// FIXME: Swift should probably have a UTF-16 type other than UInt16.
typealias CChar16 = UInt16

/// \brief The C++11 'char32_t' type, which has UTF-32 encoding.
typealias CChar32 = Char

/// \brief The C '_Bool' and C++ 'bool' type.
struct CBool {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    if value == 0 {
      return false
    }
    return true
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  func [conversion] __conversion() -> Bool {
    return getLogicValue()
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to C Boolean
  /// type.
  func [conversion] __conversion() -> CBool {
    var result : CBool
    if this {
      result.value = 1
    } else {
      result.value = 0
    }
    return result
  }
}

/// \brief A wrapper around an opaque C pointer.
///
/// Opaque pointers are used to represent C pointers to types that
/// cannot be represented in Swift, such as incomplete struct types.
struct COpaquePointer : Equatable, Hashable {
  var value : Builtin.RawPointer

  static func null() -> COpaquePointer {
    return COpaquePointer()
  }

  /// \brief Determine whether the given pointer is null.
  func isNull() -> Bool {
    return this == COpaquePointer.null()
  }

  func __equal__(rhs : COpaquePointer) -> Bool {
    return _getBool(Builtin.cmp_eq_RawPointer(value, rhs.value))
  }
  func hashValue() -> Int {
    return Int(Builtin.ptrtoint_Int64(value))
  }
}

extension COpaquePointer {
  /// \brief Convert a typed UnsafePointer to an opaque C pointer.
  /// FIXME: Make this an implicit conversion?
  /// FIXME: This shouldn't have to be in an extension.
  constructor<T>(from : UnsafePointer<T>) {
    value = from.value;
  }
}

extension String {
  /// Returns an UnsafePointer to the base address of the string without
  /// copying.  The pointed-to string is not guaranteed to be null terminated
  /// so cannot be safely passed to APIs that require a C string.
  func cAddress() -> UnsafePointer<CChar> {
    return UnsafePointer(str_value.base.value)
  }
}

/// \brief Access to the raw argc value from C.
var C_ARGC : CInt

/// \brief Access to the raw argv value from C. Accessing the argument vector
/// through this pointer is unsafe.
var C_ARGV : UnsafePointer<CString>
