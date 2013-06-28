/// \brief A wrapper around a C pointer to type T.
///
/// This wrapper stores a C pointer to an object of type T, and is
/// intended to be used to interface with C libraries. It provides no
/// automated memory management, and therefore the user must take care
/// to allocate and free memory appropriately.
///
/// For C pointers for which the pointed-to type cannot be represented
/// directly in Swift, the \c COpaquePointer will be used instead.
struct UnsafePointer<T> : BidirectionalIndex, Comparable, Hashable {
  /// \brief The underlying raw (untyped) pointer.
  var value : Builtin.RawPointer

  /// \brief Construct a null pointer.
  constructor() {
    this.value = Builtin.inttoptr_Int64(0.value)
  }

  /// \brief Construct an UnsafePointer from a builtin raw pointer.
  constructor(value : Builtin.RawPointer) {
    this.value = value
  }

  /// \brief Convert from an opaque C pointer to a typed C pointer.
  ///
  /// This is a fundamentally unsafe conversion.
  constructor(other : COpaquePointer) {
    value = other.value
  }

  /// \brief Convert from a pointer of a different type.
  ///
  /// This is a fundamentally unsafe conversion.
  constructor<U>(from : UnsafePointer<U>) {
    value = from.value
  }

  static func null() -> UnsafePointer<T> {
    return UnsafePointer()
  }

  /// \brief Get the address of an lvalue.
  ///
  /// This is a fundamentally unsafe operation. The validity of the address
  /// cannot be guaranteed beyond the scope of the statement containing the
  /// constructed pointer. If the lvalue is a logical property, any value
  /// written to the pointer will be written back to the property, and the
  /// pointer will be invalidated at the end of the statement.
  static func addressOf(lv : [byref] T) -> UnsafePointer<T> {
    return UnsafePointer(Builtin.addressof(&lv))
  }

  static func alloc(num : Int) -> UnsafePointer<T> {
    // Don't both with overflow checking.
    var size = Int(Builtin.strideof(T)) * num
    return UnsafePointer(Builtin.allocRaw(size.value, Builtin.alignof(T)))
  }

  func dealloc(num : Int) {
    // Overflow checking is actually not required here.
    var size = Int(Builtin.strideof(T)) * num
    Builtin.deallocRaw(value, size.value)
  }


  /// \brief Retrieve the value the pointer points to.
  func get() -> T {
    debugTrap(!isNull())
    return Builtin.load(value)
  }

  /// \brief Set the value the pointer points to, copying over the
  /// previous value.
  func set(newvalue : T) {
    debugTrap(!isNull())
    Builtin.assign(newvalue, value)
  }

  /// \brief Initialize the value the pointer points to, to construct
  /// an object where there was no object previously stored.
  func init(newvalue : T) {
    debugTrap(!isNull())
    Builtin.init(newvalue, value)
  }

  /// \brief Retrieve the value the pointer points to, moving it away
  /// from the location referenced in memory.
  ///
  /// The object in memory should not be used again (except perhaps to
  /// initialize or destroy it).
  func move() -> T {
    debugTrap(!isNull())
    return Builtin.move(value)
  }

  /// \brief Destroy the object the pointer points to.
  func destroy() {
    debugTrap(!isNull())
    Builtin.destroy(T, value)
  }

  func isNull() -> Bool {
    return this == UnsafePointer.null()
  }

  subscript (i : Int) -> T {
  get:
    debugTrap(!isNull())
    return (this + i).get()
  set:
    debugTrap(!isNull())
    (this + i).set(value)
  }

  //
  // Protocol conformance
  //
  func __equal__(rhs: UnsafePointer<T>) -> Bool {
    return _getBool(Builtin.cmp_eq_RawPointer(value, rhs.value))
  }
  func __less__(rhs: UnsafePointer<T>) -> Bool {
    return _getBool(Builtin.cmp_ult_RawPointer(value, rhs.value))
  }
  func hashValue() -> Int {
    return Int(Builtin.ptrtoint_Int64(value))
  }
  func succ() -> UnsafePointer<T> {
    return this + 1
  }
  func pred() -> UnsafePointer<T> {
    return this - 1
  }
}

// FIXME: Should we expose/use size_t and ptrdiff_t?

func + <T>(lhs : UnsafePointer<T>,
           rhs : Int64) -> UnsafePointer<T> {
  debugTrap(!lhs.isNull())
  return UnsafePointer(
             Builtin.gep_Int64(lhs.value,
                               (rhs * Int(Builtin.strideof(T))).value))
}

func + <T>(lhs : Int64,
           rhs : UnsafePointer<T>) -> UnsafePointer<T> {
  return rhs + lhs
}

func - <T>(lhs : UnsafePointer<T>,
           rhs : Int64) -> UnsafePointer<T> {
  return lhs + -rhs
}

func - <T>(lhs : UnsafePointer<T>,
           rhs : UnsafePointer<T>) -> Int {
  debugTrap((!lhs.isNull() && !rhs.isNull()) ||
            (lhs.isNull() && rhs.isNull()),
            "subtracting unrelated pointers")
  return Int(Builtin.sub_Int64(Builtin.ptrtoint_Int64(lhs.value),
                               Builtin.ptrtoint_Int64(rhs.value)))
}

func [assignment] += <T>(lhs : [byref] UnsafePointer<T>, rhs : Int64) {
  lhs = lhs + rhs
}

func [assignment] -= <T>(lhs : [byref] UnsafePointer<T>, rhs : Int64) {
  lhs = lhs - rhs
}
