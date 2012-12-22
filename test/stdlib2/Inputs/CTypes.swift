//===----------------------------------------------------------------------===//
// C Primitive Types
//===----------------------------------------------------------------------===//

/// \brief The C 'void' type.
typealias CVoid = Void

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
    if value == 0 { return false }
    return true
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  func [conversion] __conversion() -> Bool {
    return this
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to C Boolean
  /// type.
  func [conversion] __conversion() -> CBool {
    var result : CBool
    if this { result.value = 1 }
    else { result.value = 0 }
    return result
  }
}

/// \brief A wrapper around a C pointer to type T.
///
/// This wrapper stores a C pointer to an object of type T, and is
/// intended to be used to interface with C libraries. It provides no
/// automated memory management, and therefore the user must take care
/// to allocate and free memory appropriately.
struct CPointer<T> {
  /// \brief The underlying raw (untyped) pointer.
  var value : Builtin.RawPointer

  /// \brief Determine whether the given pointer is null.
  func isNull() -> Bool { 
    return _getBool(Builtin.cmp_eq_RawPointer(
                      value, 
                      Builtin.inttoptr_Int64(Int64(0).value)))
  }

  /// \brief Retrieve the value the pointer points to.
  func get() -> T {
    // FIXME: assert(!isNull())
    return Builtin.load(value)
  }

  /// \brief Set the value the pointer points to, copying over the
  /// previous value.
  func set(newvalue : T) {
    // FIXME: assert(!isNull())
    Builtin.assign(newvalue, value)
  }

  /// \brief Initialize the value the pointer points to, to construct
  /// an object where there was no object previously stored.
  func init(newvalue : T) {
    // FIXME: assert(!isNull())
    Builtin.init(newvalue, value)
  }

  /// \brief Retrieve the value the pointer points to, moving it away
  /// from the location referenced in memory. 
  ///
  /// The object in memory should not be used again (except perhaps to
  /// initialize or destroy it).
  func move() -> T {
    // FIXME: assert(!isNull())
    return Builtin.move(value)
  }

  /// \brief Destroy the object the pointer points to.
  func destroy() {
    // FIXME: assert(!isNull())
    Builtin.destroy(T, value)
  }

  /// \brief Subscript into the memory referenced by the C pointer,
  /// getting or setting the ith element.
  subscript (i : Int) -> T {
    get {
      // FIXME: assert(!isNull())
      return (this + i).get()
    }

    set {
      // FIXME: assert(!isNull())
      (this + i).set(value)
    }
  }
}

// FIXME: Should we expose/use size_t and ptrdiff_t?

func [infix_left=190] + <T>(lhs : CPointer<T>,
                            rhs : Int64) -> CPointer<T> {
  typealias CPtr = CPointer<T>
  return CPtr(Builtin.gep_Int64(lhs.value, (rhs * Int(Builtin.strideof(T))).value))
}

func [infix_left=190] + <T>(lhs : Int64,
                            rhs : CPointer<T>) -> CPointer<T> {
  return rhs + lhs
}

func [infix_left=190] - <T>(lhs : CPointer<T>,
                            rhs : Int64) -> CPointer<T> {
  return lhs + -rhs
}

func [infix_left=190] - <T>(lhs : CPointer<T>,
                            rhs : CPointer<T>) -> Int {
  return Int(Builtin.sub_Int64(Builtin.ptrtoint_Int64(lhs.value),
                               Builtin.ptrtoint_Int64(rhs.value)))
}

func [infix_left=90,assignment] += <T>(lhs : [byref] CPointer<T>, rhs : Int64) {
  lhs = lhs + rhs
}

func [infix_left=90,assignment] -= <T>(lhs : [byref] CPointer<T>, rhs : Int64) {
  lhs = lhs - rhs
}

func [assignment] ++ <T>(a : [byref] CPointer<T>) { a += 1 }
func [assignment] -- <T>(a : [byref] CPointer<T>) { a -= 1 }

func == <T>(lhs : CPointer<T>, rhs : CPointer<T>) -> Bool {
  return _getBool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

func != <T>(lhs : CPointer<T>, rhs : CPointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ne_RawPointer(lhs.value, rhs.value))
}

func < <T>(lhs : CPointer<T>, rhs : CPointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ult_RawPointer(lhs.value, rhs.value))
}

func <= <T>(lhs : CPointer<T>, rhs : CPointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ule_RawPointer(lhs.value, rhs.value))
}

func > <T>(lhs : CPointer<T>, rhs : CPointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ugt_RawPointer(lhs.value, rhs.value))
}

func >= <T>(lhs : CPointer<T>, rhs : CPointer<T>) -> Bool {
  return _getBool(Builtin.cmp_uge_RawPointer(lhs.value, rhs.value))
}
