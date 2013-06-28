// FIXME -- add [rootClass] attribute
// FIXME -- have non-rootClass classes default inherit from this via a typealias
// SEE ALSO -- <rdar://problem/12939349> ER: class attribute: "rootClass"

func [asmname="swift_getClassName"]
_className(obj:Object.metatype) -> CString

class Object : Identifiable, ClassNameable {
  static func className() -> String {
    return demangleType(
      String.fromCString(
      _className(this)))
  }
  func __eq__(rhs: Object) -> Bool {
    return _getBool(Builtin.cmp_eq_RawPointer(
      Builtin.bridgeToRawPointer(this),
      Builtin.bridgeToRawPointer(rhs)
    ))
  }
}

// XXX -- this next line confuses and then crashes the compiler?!?
// typealias DefaultRootClass = Object

// NOTE: Despite the presence of generic ===/!=== operators, we need
// these non-generic functions so they can operate on derived classes
func === (lhs : Object, rhs : Object) -> Bool {
  return lhs.__eq__(rhs)
}

func !== (lhs : Object, rhs : Object) -> Bool {
  return !lhs.__eq__(rhs)
}

