public class C {}

@transparent public func foo(x x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  return Builtin.cmp_eq_Int1(x, y)
}

@transparent public func destroy_obj(x x: Builtin.RawPointer) {
  return Builtin.destroy(Builtin.NativeObject, x)
}

@transparent public func assign_tuple(x x: (Builtin.Int64, Builtin.NativeObject),
                               y: Builtin.RawPointer) {
  Builtin.assign(x, y)
}

@transparent public func class_to_native_object(c c: C) -> Builtin.NativeObject {
  return Builtin.castToNativeObject(c)
}

@transparent public func class_from_native_object(p p: Builtin.NativeObject) -> C {
  return Builtin.castFromNativeObject(p)
}

@transparent public func class_to_raw_pointer(c c: C) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(c)
}

@transparent public func class_from_raw_pointer(p p: Builtin.RawPointer) -> C {
  return Builtin.bridgeFromRawPointer(p)
}

@transparent public func gep32(p p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  return Builtin.gep_Int32(p, i)
}
