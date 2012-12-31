// FIXME -- add [rootClass] attribute
// FIXME -- have non-rootClass classes default inherit from this via a typealias
// SEE ALSO -- <rdar://problem/12939349> ER: class attribute: "rootClass"

class Object : Equality {
}

// XXX -- this next line confuses and then crashes the compiler?!?
// typealias DefaultRootClass = Object

func [infix=160] == (lhs : Object, rhs : Object) -> Bool {
  return _getBool(Builtin.cmp_eq_RawPointer(
    Builtin.bridgeToRawPointer(lhs),
    Builtin.bridgeToRawPointer(rhs)
  ))
}

func [infix=160] != (lhs : Object, rhs : Object) -> Bool {
  return _getBool(Builtin.cmp_ne_RawPointer(
    Builtin.bridgeToRawPointer(lhs),
    Builtin.bridgeToRawPointer(rhs)
  ))
}
