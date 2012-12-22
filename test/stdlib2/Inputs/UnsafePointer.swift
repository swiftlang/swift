//===----------------------------------------------------------------------===//
// UnsafePointer<T> Type
//===----------------------------------------------------------------------===//

struct UnsafePointer<T> {
  var value : Builtin.RawPointer

  func get() -> T {
    return Builtin.load(value)
  }

  func set(newvalue : T) {
    Builtin.assign(newvalue, value)
  }

  func init(newvalue : T) {
    Builtin.init(newvalue, value)
  }

  func move() -> T {
    return Builtin.move(value)
  }

  func destroy() {
    Builtin.destroy(T, value)
  }

  static func alloc(num : Int) -> UnsafePointer<T> {
    typealias Ty = UnsafePointer<T>
    // Don't both with overflow checking.
    var size = Int(Builtin.strideof(T)) * num
    return Ty(Builtin.allocRaw(size.value, Builtin.alignof(T)))
  }

  func dealloc(num : Int) {
    // Overflow checking is actually not required here.
    var size = Int(Builtin.strideof(T)) * num
    Builtin.deallocRaw(value, size.value)
  }

  subscript (i : Int) -> T {
    get {
      return (this + i).get()
    }

    set {
      (this + i).set(value)
    }
  }
}

func [infix_left=190] + <T>(lhs : UnsafePointer<T>,
                            rhs : Int64) -> UnsafePointer<T> {
  typealias UnsafePtr = UnsafePointer<T>
  return UnsafePtr(Builtin.gep_Int64(lhs.value, (rhs * Int(Builtin.strideof(T))).value))
}

func [infix_left=190] + <T>(lhs : Int64,
                            rhs : UnsafePointer<T>) -> UnsafePointer<T> {
  return rhs + lhs
}

func [infix_left=190] - <T>(lhs : UnsafePointer<T>,
                            rhs : Int64) -> UnsafePointer<T> {
  return lhs + -rhs
}

func [infix_left=190] - <T>(lhs : UnsafePointer<T>,
                            rhs : UnsafePointer<T>) -> Int {
  return Int(Builtin.sub_Int64(Builtin.ptrtoint_Int64(lhs.value),
                               Builtin.ptrtoint_Int64(rhs.value)))
}

func [infix_left=90,assignment] += <T>(lhs : [byref] UnsafePointer<T>, rhs : Int64) {
  lhs = lhs + rhs
}

func [infix_left=90,assignment] -= <T>(lhs : [byref] UnsafePointer<T>, rhs : Int64) {
  lhs = lhs - rhs
}

func [assignment] ++ <T>(a : [byref] UnsafePointer<T>) { a += 1 }
func [assignment] -- <T>(a : [byref] UnsafePointer<T>) { a -= 1 }

func == <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

func != <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ne_RawPointer(lhs.value, rhs.value))
}

func < <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ult_RawPointer(lhs.value, rhs.value))
}

func <= <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ule_RawPointer(lhs.value, rhs.value))
}

func > <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ugt_RawPointer(lhs.value, rhs.value))
}

func >= <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_uge_RawPointer(lhs.value, rhs.value))
}
