

/// \brief A wrapper around a C pointer to type T.
///
/// This wrapper stores a C pointer to an object of type T, and is
/// intended to be used to interface with C libraries. It provides no
/// automated memory management, and therefore the user must take care
/// to allocate and free memory appropriately.
struct CPointer<T> {
  /// \brief The underlying raw (untyped) pointer.
  var value : Builtin.RawPointer

  /// \brief Retrieve the value the pointer points to.
  func get() -> T {
    return Builtin.load(value)
  }

  /// \brief Set the value the pointer points to, copying over the
  /// previous value.
  func set(newvalue : T) {
    Builtin.assign(newvalue, value)
  }

  /// \brief Initialize the value the pointer points to, to construct
  /// an object where there was no object previously stored.
  func init(newvalue : T) {
    Builtin.init(newvalue, value)
  }

  /// \brief Retrieve the value the pointer points to, moving it away
  /// from the location referenced in memory. 
  ///
  /// The object in memory should not be used again (except perhaps to
  /// initialize or destroy it).
  func move() -> T {
    return Builtin.move(value)
  }

  /// \brief Destroy the object the pointer points to.
  func destroy() {
    Builtin.destroy(T, value)
  }

  /// \brief Subscript into the memory referenced by the C pointer,
  /// getting or setting the ith element.
  subscript (i : Int) -> T {
    get {
      return (this + i).get()
    }

    set {
      (this + i).set(value)
    }
  }
}

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
