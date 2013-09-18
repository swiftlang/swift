class C {}

func [transparent] foo(x:Builtin.Int1, y:Builtin.Int1) -> Builtin.Int1 {
  return Builtin.cmp_eq_Int1(x, y)
}

func [transparent] destroy_obj(x:Builtin.RawPointer) {
  return Builtin.destroy(Builtin.ObjectPointer, x)
}

func [transparent] assign_tuple(x:(Builtin.Int64, Builtin.ObjectPointer),
                  y:Builtin.RawPointer) {
  Builtin.assign(x, y)
}

func [transparent] class_to_object_pointer(c:C) -> Builtin.ObjectPointer {
  return Builtin.castToObjectPointer(c)
}

func [transparent] class_from_object_pointer(p:Builtin.ObjectPointer) -> C {
  return Builtin.castFromObjectPointer(p)
}

func [transparent] class_to_raw_pointer(c:C) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(c)
}

func [transparent] class_from_raw_pointer(p:Builtin.RawPointer) -> C {
  return Builtin.bridgeFromRawPointer(p)
}

func [transparent] gep32(p:Builtin.RawPointer, i:Builtin.Int32) -> Builtin.RawPointer {
  return Builtin.gep_Int32(p, i)
}
