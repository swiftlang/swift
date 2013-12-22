/// \brief Definitions that make elements of Builtin usable in real
/// code without gobs of boilerplate.  These APIs will probably *not*
/// be exposed outside the stdlib.

func sizeof<T>(_:T.metatype) -> Int {
  return Int(Word(Builtin.sizeof(T)))
}

func sizeof<T>(_:T) -> Int {
  return Int(Word(Builtin.sizeof(T)))
}


// FIXME: should be a constructor of UnsafePointer
func addressof<T>(x: @inout T) -> UnsafePointer<T> {
  return UnsafePointer<T>(Builtin.addressof(&x))
}
